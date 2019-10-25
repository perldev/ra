-module(api_table_holder).
-behaviour(gen_server).


-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0, stop/0,  status/0, assert/3, lookup/1]).

-include("erws_console.hrl").



           
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
        mysql:prepare(Pid, <<"insert into facts(Name, Value, Sign) VALUES(?, ?, ?)">>),
        Query = <<"SELECT  Name, Value, ts FROM  facts WHERE Value like CONCAT('%', ? , '%')">>,
        mysql:prepare(Pid, Query),
        ?LOG_DEBUG("connected to ~p ~n", [Pid]),
        {ok, #monitor{pid=Pid}
        }.

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
handle_call(Info,_From ,State) ->
    ?LOG_DEBUG("get msg call ~p ~n", [Info]),
    {reply, undefined , State}.

%%HERE we receive tasks from common ajax
handle_cast({add, Key, Params, Sign}, MyState) ->
    Pid = MyState#monitor.pid, 
    Query = <<"insert into facts(Name, Value, Sign) VALUES(?, ?, ?)">>,
    ok = mysql:query(Pid, Query, [Key, Params, Sign]),
    {noreply, MyState}
.

handle_info(Message,  State)->
    ?LOG_DEBUG("undefined child process ~p ~n", [Message]),
     {noreply,  State}.
  

terminate(_Reason, _State) ->
   terminated.

status() ->
        gen_server:call(?MODULE, status).
        
stop()->
        gen_server:call(?MODULE, stop).


lookup(Body)->
        gen_server:call(?MODULE, {lookup, Body}).
    
    
assert(Name, Params, Sign)->
    gen_server:cast(?MODULE, {add, Name, Params, Sign}).



