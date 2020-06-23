-module(api_table_holder).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0, 
         status/0, assert/4,
         assert/5,
         lookup/1,
         lookup/2,
         erlog_once4export/2,
         save_db/1, save_db/0, 
         load_from_dump/1, load_from_db/0, 
         flush_erlog/0, add_consisten_knowledge/0,
         erlog_once/1, erlog_load_code/1, 
         load_erlog/0, create_expert/2, tmp_export_file/0,
         api_stat/2, get_api_stat/0, start_queues/0,
         myqueue/1]).

-include("erws_console.hrl").
-include_lib("erlog/src/erlog_int.hrl").
%%HACK
-record(erlog, {vs=[],est}).


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
        ets:new(?UNIQ, [public, set, named_table]),
        ets:new(?STAT, [public, set, named_table]),

        case   application:get_env(dump_name) of 
           undefined->
                ets:new(?SYSTEMS, [public, set, named_table]),
                {ok, Erlog1} = erlog:new(),
                
                ets:insert(?SYSTEMS, {"", Erlog1, spawn_link(?MODULE, myqueue, [""] ) }), %empty key for default expert system
                {ok, #monitor{pid=Pid, 
                              erlog=Erlog,
                              erlog1=Erlog1
                            }
                };
            {ok, DumpName}->
                %% load from file
                {ok, ?SYSTEMS} = ets:file2tab(DumpName),
                case ets:lookup(?SYSTEMS, "") of
                    [] -> 
                        {ok, Erlog1} = erlog:new(),
                        ets:insert(?SYSTEMS, {"", Erlog1 }), %empty key for default expert system
                        start_queues(),
                        {ok, #monitor{pid=Pid, 
                                      erlog=Erlog,
                                      erlog1 = Erlog1,
                                      dump_name=DumpName,
                                      db_loaded=true
                                     }
                        };
                     [{"", Erlog1}]->                     
                       start_queues(),
                       {ok, #monitor{pid=Pid, 
                                     erlog=Erlog,
                                     erlog1 = Erlog1,
                                     dump_name=DumpName,
                                     db_loaded=true
                                    }
                        };
                    [{"", Erlog1, _P}]->
                       start_queues(),
                       {ok, #monitor{pid=Pid, 
                                     erlog=Erlog,
                                     erlog1 = Erlog1,
                                     dump_name=DumpName,
                                     db_loaded=true
                                    }
                        }
                end
        
        end.

        
start_queues()->
    lists:foreach(fun(Elem)->
                    case Elem of 
                        {Key, Erlog,_Pid} ->
                            ets:insert(?SYSTEMS, {Key, Erlog, spawn_link(?MODULE, myqueue, [Key] )});
                        {Key, Erlog} ->
                            ets:insert(?SYSTEMS, {Key, Erlog, spawn_link(?MODULE, myqueue, [Key] )})
                    end         
                  end, ets:tab2list(?SYSTEMS)).
        
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
% ALGO of refreshing public api of expert system
% 1) flush() 
% 2) load expert  code
% 3) add_consisten_knowledge
handle_call(flush_erlog, _From ,State) ->
    ?LOG_DEBUG("start loading from   ~n", []),
    {ok, Erlog} = erlog:new(),
    {reply, ok, State#monitor{erlog=Erlog, current_version=undefined}};
    

%%DEPRECATED
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
%% DEPRECATED
handle_cast( {erlog_code, Terms}, State)->
   Erlog = State#monitor.erlog1,
   Db = get_inner_db(Erlog),
  ?LOG_DEBUG("Db is  ~p  ~n and Terms ~p  ~n", [ Db, Terms]),
   Pid = spawn_link(fun() ->   lists:foreach(fun(Elem)->   
                                                ?LOG_DEBUG("process loading ~p \n", [Elem]),
                                                Res = erlog_int:asserta_clause(Elem, Db),
                                                ?LOG_DEBUG("result ~p \n", [Res])
                                                 end,
                                                Terms) 
                                           end),
    ?LOG_DEBUG("start loading ~p \n", [Pid]),
    {noreply, State#monitor{erlog1=Erlog}}
;
handle_cast({load_from_dump, FileName}, State) ->
    ets:delete(?SYSTEMS),
    {ok, ?SYSTEMS} = ets:file2tab(FileName),
    start_queues(),
    case ets:lookup(?SYSTEMS, "") of
          [] -> 
           {ok, Erlog1} = erlog:new(),
            ets:insert(?SYSTEMS, {"", Erlog1, spawn_link(?MODULE, myqueue, [""])  }),
            ?LOG_DEBUG("loaded normal \n", []),
            {noreply, State#monitor{db_loaded=true}};
          {"", Value}->
            ets:insert(?SYSTEMS, {"", Value, spawn_link(?MODULE, myqueue, [""])  }),
            {noreply, State#monitor{db_loaded=true}};
          {"", Value, _Pid}->
            ets:insert(?SYSTEMS, {"", Value, spawn_link(?MODULE, myqueue, [""])  }),
            {noreply, State#monitor{db_loaded=true}}
    end

;
handle_cast(dump_db, State) ->
    ets:tab2file(?SYSTEMS , State#monitor.dump_name),
    ?LOG_DEBUG("saved normal \n", []),
    {noreply, State}
;
handle_cast({dump_db, FileName}, State) ->
    ets:tab2file(?SYSTEMS , FileName),
    ?LOG_DEBUG("saved normal \n", []),
    {noreply, State}
;
handle_cast({create_expert, Username, Erlog}, State)->
    LS = State#monitor.systems,
    ets:insert(?SYSTEMS, {Username,  Erlog, spawn_link(?MODULE, myqueue, [ Username]) }),
    {noreply,  State#monitor{systems=[Username|LS]}}   
;
%%DEPRECATED
%%TODO add looking by info
handle_cast(load_from_db, State) ->
    ?LOG_DEBUG("start loading from    ~n", []),
    Pid = State#monitor.pid, 
    Query = <<"SELECT  Name, Value, ts FROM  facts WHERE 1">>,
    {ok, _ColumnNames, Rows} = mysql:query(Pid, Query, []),    
    Erlog = State#monitor.erlog1,
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
    {noreply, State#monitor{erlog1=NewErl, db_loaded=true}}.
    
handle_info(Message,  State)->
    ?LOG_DEBUG("undefined child process ~p ~n", [Message]),
     {noreply,  State}.

terminate(_Reason, State) ->
    ets:tab2file(?SYSTEMS , State#monitor.dump_name),
    terminated.

get_inner_db(Erl0)->
    MyErlog = Erl0#erlog.est,
    MyErlog#est.db.
     
get_inner_ets(Erl0)->
    MyErlog = Erl0#erlog.est,
    Db  = MyErlog#est.db,
    Db#db.loc.
   
%yet without standart call   
erlog_once4export(NameOfExport, Goal)->
    case ets:lookup(?SYSTEMS, NameOfExport) of 
        [] -> {error, <<"expert system does not exist">>};
        [{NameOfExport, Erlog, Pid}]->
            ?LOG_DEBUG("start coal from  ~p ~n", [Goal]),
            case catch erlog:prove(Goal, Erlog) of
                {{succeed, Vs}, NewErl}->
                        ets:insert(?SYSTEMS, {NameOfExport, NewErl, Pid}),
                        Vs;
                {fail, NewErl}->
                         ets:insert(?SYSTEMS, {NameOfExport, NewErl, Pid}),
                        false;
                {{error,Error}, _NewErl}->
                        {error, Error};
                {{'EXIT',Error}, _NewErl}->
                        {error, Error}
            end
    end
.
   
   
status() ->
        gen_server:call(?MODULE, status).
        
stop()->
        gen_server:call(?MODULE, stop).

erlog_once(Msg)->
  erlog_once4export("", Msg).
   
  
        
load_from_db()->
    gen_server:cast(?MODULE, load_from_db).

load_erlog()->
    gen_server:cast(?MODULE, load_erlog).
    

create_expert(Username, MyTerms)->
    ?LOG_DEBUG("create expert system ~p ~n", [Username]),
    {ok, Erlog} = erlog:new(),%erlog_db_ets, list_to_atom(binary_to_list(Username)) ),                       
    %load common rules of our system    
    FinaleErl =  lists:foldl(fun(Elem, Erl )->    
                            Goal  = {assert, Elem},
                            { {succeed,_}, NewErl1} = erlog:prove(Goal, Erl), 
                            NewErl1 end, Erlog, MyTerms), 
    gen_server:cast(?MODULE, {create_expert, Username, FinaleErl}).
    
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
    

%%you should check in constantly
api_stat(Res, Path)->
    spawn(fun()-> 
                ets:update_counter(?STAT, {Path, Res}, 1, { {Path, Res}, 0}) 
          end)
    
.

get_api_stat()->
    ets:tab2list(?STAT)
.

%%DEPRECATED
erlog_load_code(Code)->
  File = tmp_export_file(),
  %%HACK add \n at the end of file for correct parsing
  file:write_file(File, <<Code/binary, "\n\n\n">>), 
  {ok, Terms } = erlog_io:read_file(File),  
  gen_server:cast(?MODULE, {erlog_code, Terms}).

lookup(Body, Timout)->
    gen_server:call(?MODULE, {lookup, Body}, Timout).
  

lookup(Body)->
    lookup(Body, 20000).
    

    
myqueue(Key)->
    receive 
      {add, NameOfExport, Key, Params, Raw, Sign }->
          case ets:lookup(?SYSTEMS, NameOfExport ) of
             [{NameOfExport, Erlog, Pid}]->
                ?CONSOLE_LOG("add to ~p  ~p~n", [NameOfExport, {Key, Params, Raw} ]),
                Ets = erlang:localtime(),
                NewEts = erws_api:format_date(Ets),
                Functor = list_to_atom(binary_to_list(Key)),
                NewRuleL = [Functor, NewEts|Params],
                NewRule = list_to_tuple(NewRuleL),
                Db = get_inner_db(Erlog),
                NewDb = erlog_int:asserta_clause(NewRule, Db),
                Est = Erlog#erlog.est,
                ets:insert(?SYSTEMS, {NameOfExport, Erlog#erlog{est=Est#est{db=NewDb}} } ),
                myqueue(Key);
            []->
                ?CONSOLE_LOG("i didn't find expert system for ~p ~n", [NameOfExport]),
                exit(abnormal)
            end
    
    end
.
    
    
% it shoul
assert(NameOfExport, Key, Params, Raw, Sign)->
    %% ADDING TO DEFAULT
    assert(Key, Params, Raw, Sign),
    case ets:lookup(?SYSTEMS, NameOfExport) of 
        [] -> {fail, non_exist};
        [{NameOfExport, _Erlog, Pid}]->
            Pid ! { add, NameOfExport, Key, Params, Raw, Sign}
    end.

assert(Key, Params, Raw, Sign)->
%% turn of check doubles
    MyState = api_table_holder:status(),
    ?LOG_DEBUG("start adding to memory to ~p ~n", [{Key, Params, Raw}]),
    Pid = MyState#monitor.pid, 
    Query = <<"INSERT INTO facts(Name, Value, Sign) VALUES(?, ?, ?)">>,
    ok = mysql:query(Pid, Query, [Key, Raw, Sign]),
    case ets:lookup(?SYSTEMS, "") of
            [{"", _Erlog, Pid}]->             
              Pid ! { add, "", Key, Params, Raw, Sign};
            _ ->   
              ?LOG_DEBUG("we didn't find default system ~n", [] ),
              true
     end
.


id_generator()->
    {T1,T2,T3 } = erlang:now(),  
    List =   lists:flatten( io_lib:format("~.B~.B~.B",[T1,T2,T3]) ) ,
    List
.

tmp_export_file()->
        Base = id_generator(),
        "/tmp/prolog_tmp_"++Base++".pl".
