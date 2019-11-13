-module(api_table_holder).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0,  status/0, assert/3, lookup/1,
         save_db/1, save_db/0, 
         load_from_dump/1, load_from_db/0, 
         flush_erlog/0, add_consisten_knowledge/0,
         erlog_once/1, erlog_load_code/1, load_erlog/0]).

-include("erws_console.hrl").

-define(ETS_NAME, ets_name1).
-define(ETS_NAME1, ets_name).

           
start_link() ->
          gen_server:start_link({local, ?MODULE},?MODULE, [],[]).

init([]) ->
            
        {ok, Host} = application:get_env(mysql_host),
        {ok, User} = application:get_env(mysql_user),
        {ok, Db} = application:get_env(mysql_db),
        {ok, Pwd} = application:get_env(mysql_pwd),

        {ok, Pid} = mysql:start_link([{host,  Host},
                                      {user, User},
                                      {password, Pwd},
                                      {database, Db}]),
        ?LOG_DEBUG("connected to ~p ~n", [Pid]),
        mysql:prepare(Pid, <<"INSERT INTO facts(Name, Value, Sign) VALUES(?, ?, ?)">>),
        Query = <<"SELECT  Name, Value, ts FROM  facts WHERE Value like CONCAT('%', ? , '%')">>,
        mysql:prepare(Pid, Query),
        Query1 = <<"SELECT  Name, Value, ts FROM  facts WHERE 1">>,
        mysql:prepare(Pid, Query1),
        {ok, Erlog} = erlog:new(erlog_db_ets, ?ETS_NAME),
        case   application:get_env(dump_name) of 
           undefined->
                {ok, Erlog1} = erlog:new(erlog_db_ets, ?ETS_NAME1),
                {ok, #monitor{pid=Pid, 
                              erlog=Erlog,
                              erlog1=Erlog1
                            }
                };
            {ok, DumpName}->
                %% load from file
                {ok, Erlog1} = erlog:new(erlog_db_ets, DumpName),
                {ok, #monitor{pid=Pid, 
                            erlog=Erlog,
                            erlog1=Erlog1,
                            dump_name=DumpName,
                            db_loaded=true
                            }
                }
        
        end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(status,_From ,State) ->
    ?LOG_DEBUG("get msg call ~p ~n", [status]),
    {reply, State, State};

handle_call({ lookup, Body}, _From, State) ->
    ?LOG_DEBUG("get msg call ~p ~n", [Body]),
    Pid = State#monitor.pid, 
    Query = <<"SELECT  Name, Value, ts FROM  facts WHERE Value like CONCAT('%', ? ,'%') ">>,
    {ok, ColumnNames, Rows} = mysql:query(Pid, Query, [Body]),
    ?LOG_DEBUG("found  ~p ~n", [{ColumnNames, Rows}]),
    {reply, Rows, State};    
handle_call({once, Goal},_From, State )->
  Erlog = State#monitor.erlog,
  ?LOG_DEBUG("start coal from  ~p ~n", [Goal]),
  case erlog:prove(Goal, Erlog) of
      {{succeed,Vs}, NewErl}->
            {reply, Vs, State#monitor{erlog=NewErl}};
      {fail, NewErl}->
            {reply, false, State#monitor{erlog=NewErl}};
      {{error,Error}, NewErl}->
            {reply, {error, Error}, State#monitor{erlog=NewErl}};
      {{'EXIT',Error}, NewErl}->
            {reply, {error, Error}, State#monitor{erlog=NewErl}}
  end
;
% ALGO of refreshing public api of expert system
% 1) flush() 
% 2) load expert  code
% 3) add_consisten_knowledge
handle_call(flush_erlog, _From ,State) ->
    ?LOG_DEBUG("start loading from   ~n", []),
    ets:delete(?ETS_NAME),
    {ok, Erlog} = erlog:new(erlog_db_ets, ?ETS_NAME),
    {reply, ok, State#monitor{erlog=Erlog}};
handle_call({erlog_code, Body}, _From, State )->
  File = tmp_export_file(),
  %%HACK add \n at the end of file for correct parsing
  file:write_file(File, <<Body/binary, "\n\n\n">>), 
  Erlog = State#monitor.erlog,
  {ok, Terms } = erlog_io:read_file(File),
  NewErl =  lists:foldl(fun(Elem, Erl )->    
                            Goal  = {assert, Elem},
                            { {succeed,_}, NewErl} = erlog:prove(Goal, Erl), 
                            NewErl end, Erlog, Terms),
  {reply, ok, State#monitor{erlog=NewErl}}
; 
%%very long operation you shoud rewrite it
handle_call(add_consisten_knowledge, _From, State )->
  EtsTab = get_inner_ets(State#monitor.erlog1),
  EtsTab1 = get_inner_ets(State#monitor.erlog),
  lists:foreach(fun(Elem)->     
                    ets:insert(EtsTab1, Elem)
                end,
                ets:tab2list(EtsTab)),
  {reply, ok, State}
; 
handle_call(Info,_From ,State) ->
    ?LOG_DEBUG("get msg call ~p ~n", [Info]),
    {reply, undefined , State}.
    

    
%% start new expert system 
%%  1) load from db
%%  2) dump_db
% and next time  for saving time 
%   1) load from dump

handle_cast({load_from_dump, FileName}, State) ->
    ets:delete(?ETS_NAME1),
    {ok, Erlog} = erlog:new(erlog_db_ets, FileName),
    ?LOG_DEBUG("loaded normal \n", []),
    {noreply, State#monitor{erlog1=Erlog, db_loaded=true}}
;
handle_cast(dump_db, State) ->
    Erl0 = State#monitor.erlog1,
    Tab = get_inner_ets(Erl0),
    ets:tab2file(Tab , State#monitor.dump_name),
    ?LOG_DEBUG("saved normal \n", []),
    {noreply, State}
;
handle_cast({dump_db, FileName}, State) ->
    Erl0 = State#monitor.erlog1,
    Tab = get_inner_ets(Erl0),
    ets:tab2file(Tab , FileName),
    ?LOG_DEBUG("saved normal \n", []),
    {noreply, State}
;
handle_cast(load_from_db, State) ->
    ?LOG_DEBUG("start loading from    ~n", []),
    Pid = State#monitor.pid, 
    Query = <<"SELECT  Name, Value, ts FROM  facts WHERE 1">>,
    {ok, _ColumnNames, Rows} = mysql:query(Pid, Query, []),    
    Erlog = State#monitor.erlog1,
%   Res = lists:map(fun([Name, Value, Ets])->   {[{<<"type">>, Name}, {<<"value">>, json_decode(Value)}, {<<"date">>,  list_to_binary(format_date(Ets)) }]}   end,  List),
    NewErl  = lists:foldl(fun([Name, Value, Ets], Accum)->
                            ?LOG_DEBUG("processing rule for loading ~p ~n", [{Name, Value}]),
                            case catch erws_api:json_decode(Value) of 
                              {'EXIT', Error}->
                                    ?LOG_DEBUG("cant process rule  ~p ~n ~p", [{Name, Value, Ets}, Error]),
                                    Accum;
                              DecodeRule -> 
                                    NewEts = erws_api:format_date(Ets),
                                    Functor = list_to_atom(binary_to_list(Name)),
                                    NewRuleL = [Functor, NewEts|DecodeRule],
                                    NewRule= list_to_tuple(NewRuleL),
                                    Goal  = {assert, NewRule},
                                    { {succeed, _}, NewErl} = erlog:prove(Goal, Accum), 
                                    NewErl 
                            end
                       end, Erlog, Rows),
    {noreply, State#monitor{erlog1=NewErl, db_loaded=true}};    
handle_cast({add, Key, Params, Sign}, MyState) ->
    ?LOG_DEBUG("start adding to memory to ~p ~n", [{Key,Params}]),
    Pid = MyState#monitor.pid, 
    Query = <<"INSERT INTO facts(Name, Value, Sign) VALUES(?, ?, ?)">>,
    ok = mysql:query(Pid, Query, [Key, Params, Sign]),
    Erlog = MyState#monitor.erlog,
    Erlog1 = MyState#monitor.erlog1,

    Ets = erlang:localtime(),
    case catch erws_api:json_decode(Params) of 
                              {'EXIT', Error}->
                                    ?LOG_DEBUG("cant process rule  ~p ~n ~p", [{Key, Params, Ets}, Error]),
                                     {noreply, MyState#monitor{erlog=Erlog}};
                              DecodeRule -> 
                                    NewEts = erws_api:format_date(Ets),
                                    Functor = list_to_atom(binary_to_list(Key)),
                                    NewRuleL = [Functor, NewEts|DecodeRule],
                                    NewRule= list_to_tuple(NewRuleL),
                                    Goal  = {assert, NewRule},
                                    { {succeed, _}, NewErl} = erlog:prove(Goal, Erlog), 
                                    { {succeed, _}, NewErl1} = erlog:prove(Goal, Erlog1), 
                                    {noreply, MyState#monitor{erlog=NewErl, erlog1=NewErl1}}
    end.

handle_info(Message,  State)->
    ?LOG_DEBUG("undefined child process ~p ~n", [Message]),
     {noreply,  State}.
  

terminate(_Reason, State) ->
    Erl0 = State#monitor.erlog1,
    Tab = get_inner_ets(Erl0), 
    ets:tab2file(Tab , State#monitor.dump_name),
    terminated.

get_inner_ets(Erl0)->
    MyErlog = element(3, Erl0),
    Db  = element(5, MyErlog),
    element(3, Db).
   
status() ->
        gen_server:call(?MODULE, status).
        
stop()->
        gen_server:call(?MODULE, stop).

erlog_once(Msg)->
        gen_server:call(?MODULE, {once, Msg}).
        
load_from_db()->
    gen_server:cast(?MODULE, load_from_db).

load_erlog()->
    gen_server:cast(?MODULE, load_erlog).
    

    
save_db()->
    gen_server:cast(?MODULE, dump_db).

save_db(FileName)->
    gen_server:cast(?MODULE, {dump_db, FileName}).

    
load_from_dump(FileName)->
    gen_server:cast(?MODULE, {load_from_dump, FileName}).


flush_erlog()->
    gen_server:call(?MODULE, flush_erlog).
    
add_consisten_knowledge()->
    gen_server:call(?MODULE, add_consisten_knowledge).
    

    
erlog_load_code(Code)->
    gen_server:call(?MODULE, {erlog_code, Code}).

    
lookup(Body)->
        gen_server:call(?MODULE, {lookup, Body}).
    
    
assert(Name, Params, Sign)->
    gen_server:cast(?MODULE, {add, Name, Params, Sign}).


id_generator()->
    {T1,T2,T3 } = erlang:now(),  
    List =   lists:flatten( io_lib:format("~.B~.B~.B",[T1,T2,T3]) ) ,
    List
.

tmp_export_file()->
        Base = id_generator(),
        "/tmp/prolog_tmp_"++Base++".pl".
