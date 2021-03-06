%%%-------------------------------------------------------------------
%% @doc front public API
%% @end
%%%-------------------------------------------------------------------

-module(ra_app).
-include("erws_console.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, start/0, stop/0]).

%%====================================================================
%% API
%%====================================================================
routes() ->
    cowboy_router:compile([{'_',
			    [
			     {"/[...]", erws_api, dict:new()},
			     {"/static/[...]", cowboy_static,
			      [{directory, <<"static">>},
			       {mimetypes,
				[{<<".png">>, [<<"image/png">>]},
				 {<<".jpg">>, [<<"image/jpeg">>]},
				 {<<".css">>, [<<"text/css">>]},
				 {<<".js">>,
				  [<<"application/javascript">>]}]}]}
			     ]
			    }]).
start()->
    inets:start(),
    ok = application:start(crypto),
    ok = application:start(asn1),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    ok = application:start(compiler),
    %ok = application:start(dht_ring),
    ok = application:start(syntax_tools),
    ok = application:start(goldrush),    
    %ok = application:start(lager),
    application:start(ra).

start(_StartType, _StartArgs) ->
       io:format("~p",[code:all_loaded()]),    
       application:load(ra),
       {ok, Port} = application:get_env(ra, http_port),
       Dispatch = routes(),
	{ok, _} = cowboy:start_clear(http, [{port, Port}], #{
		env => #{dispatch => Dispatch}
	}),
       io:format("cowboy has been started",[]),    



	%{ok, _} = cowboy:start_clear(http, [{port, Port}], #{
%		env => #{dispatch => Dispatch}
%	}),
        io:format("~p",[code:which(mcd)]), 
	front_sup:start_link().



 %   ok = case cowboy:start_http(
 %               listener, 50000,
 %               [{port, Port}],
%            [{env, [{dispatch, Dispatch}]}]) of
 %            {ok, _} -> ok;
 %            {error, {already_started, _}} -> ok;
 %            {error, _} = Error -> Error
  %       end,
 %   front_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

    
stop()->
    appliction:stop(ra).
%%====================================================================
%% Internal functions
%%====================================================================

