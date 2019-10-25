-module(erws_api).

-include("erws_console.hrl").


% Behaviour cowboy_http_handler
-export([init/2, terminate/2,
         hexstring/1, get_key_dict/3, get_time/1, json_encode/1, json_decode/1, dict_to_json/1]).




% Called to know how to dispatch a new connection.
init(Req, Opts) ->
    ?CONSOLE_LOG("~p",[Req]),    
     Req4 = handle(Req, Opts),
    {ok, Req4, Opts}
    % we're not interested in serving any other content
.
    

headers_text_plain() ->
	#{<<"access-control-allow-origin">> => <<"*">>,  <<"Content-Type">> => <<"text/plain">>} .
        
headers_text_html() ->
	#{ <<"access-control-allow-origin">> => <<"*">>,  <<"Content-Type">> => <<"text/html">>}.      

headers_json_plain() ->
        #{ <<"access-control-allow-origin">> => <<"*">>,  <<"Content-Type">> => <<"application/json">>} .
        
headers_png() ->
	#{<<"access-control-allow-origin">> => <<"*">>,
         <<"Cache-Control">> => <<"no-cache, must-revalidate">>,
         <<"Pragma">> => <<"no-cache">>,
         <<"Content-Type">> => <<"image/png">>} 
        .
                
                
        
% Should never get here.
handle(Req, State) ->
      ?CONSOLE_LOG("====================================~nrequest: ~p ~n", [Req]),
      Path = cowboy_req:path_info(Req),
      ?CONSOLE_LOG("====================================~npath: ~p ~n", [Path]),
      {ok, Body, Req2 } = cowboy_req:read_body(Req),               
      ?CONSOLE_LOG("====================================~nbody: ~p ~n", [Body]),
      case process(Path, Body, Req2, State) of
	  {json, Json, ResReqLast, NewState }->
		?CONSOLE_LOG("got request result: ~p~n", [Json]),
		cowboy_req:reply(200, headers_json_plain(), json_encode(Json), ResReqLast);
		%{ok, JsonReq, NewState};
          {raw_answer, {Code, Binary, Headers }, ResReqLast, NewState } ->

		cowboy_req:reply(Code, Headers, Binary, ResReqLast)
      end.      

terminate(_Req, _State) -> ok.

false_response(Req, State)->
   {raw_answer, {500, <<"{\"status\":\"false\"}">>, headers_json_plain() },  Req, State}.
 
wait_response(Req, State)->
   {raw_answer, {502, <<"{\"status\":\"wait\",\"timeout\":1000}">>, headers_json_plain() },  Req, State}.
   
true_response(Req, State)->
   {raw_answer, {200, <<"{\"status\":\"true\"}">>, headers_json_plain() },  Req, State}.
   
 
-spec check_sign(tuple(), binary(), list())-> true|false. 
check_sign({undefined, undefined}, Body, State)->
  false;
check_sign({Sign, LocalKey}, Body, State)->
    CheckSign = generate_key(LocalKey, Body),
    ?CONSOLE_LOG("got salt result: calc sign ~p~n got sign ~p~n salt ~p~n body ~p~n ", 
                [CheckSign, Sign, LocalKey, Body ]),
    case list_to_binary(CheckSign)  of 
        Sign -> true;
        _ -> false
   end
.

process([<<"lookup">>],  Body, Req, State )->
    ?CONSOLE_LOG("process search from  ~p ~n",[Body]),
    List = api_table_holder:lookup(Body),
    Res = lists:map(fun([Name, Value, Ets])->   {[{<<"type">>, Name}, {<<"value">>, json_decode(Value)}, {<<"date">>,  list_to_binary(format_date(Ets)) }]}   end,  List),
    ListJson = {[{<<"status">>, true}, {<<"result">>, Res}]},
    {json, ListJson, Req, State}
    
;
process([<<"assert">>, Name],  Body, Req, State )->
    ?CONSOLE_LOG("process request from ~p ~p ~n",[Name, Body]),
    Sign = generate_key(Body),
    api_table_holder:assert(Name, Body, list_to_binary(Sign)),
    true_response(Req, State)
;    
process(Path, _Body, Req, State)->
     ?CONSOLE_LOG("undefined request from ~p ~p ~n",[Path, Req]),
     false_response(Req, State).
  

auth_user(Req, Body, State)->
       undefined
.

my_time()->
 {MegSecs, Secx, _} = now(),
  Time = MegSecs*1000000 + Secx + 3600*3,
  Time.


get_time(ResTime)->
     get_time(ResTime, <<"time">>).
     
get_time(ResTime, Key)->
  Time = my_time(), 
  [ {Key, Time}| ResTime].
  
get_key_dict(SessionObj,Key, Default)->
    case dict:find(Key, SessionObj) of
        {ok, Value} -> Value;
        error -> Default
    end
.
format_date({{Year, Month, Day},{Hour, Minute, Second}})->
   lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])).


     
generate_key(Body)->
        D = crypto:hash(sha256, Body),

        hexstring( D  ) 

.
     
generate_key(Salt, Body)->
        hexstring( crypto:hash(sha256, <<Salt/binary, Body/binary >>)  ) 
.


dict_to_json(Dict)->
	 List = dict:to_list(Dict),
    ?CONSOLE_LOG(" dict to list  ~p ~n",[List]), 
    dict_to_json(List, [])
.

dict_to_json([], Accum)->
     {Accum};

dict_to_json([{{pickle_unicode, Key}, Val}| Tail], Accum)->
           dict_to_json([{Key, Val}| Tail], Accum)
; 
dict_to_json([{ Key, {pickle_unicode, Val}}| Tail], Accum)->
           dict_to_json([{Key, Val}| Tail], Accum);
dict_to_json([{Key, Val}| Tail], Accum)->
    case checkdict(Val) of
        true -> 
            ValNormal =  {dict_to_json( dict:to_list(Val), []  )},
            dict_to_json(Tail, [{Key, ValNormal}|Accum]);
        false-> 
            case Val of
            {pickle_unicode, Val1} -> % do not save here not ASCII symbols
                dict_to_json(Tail, [{Key, Val1}|Accum]);
            Val1 -> % do not save here not ASCII symbols
                dict_to_json(Tail, [{Key, Val1}|Accum])
            end    
    end.
    
checkdict(DictCand) when is_tuple(DictCand) andalso element(1, DictCand) =:= dict ->
  true;
checkdict(_NotDict) ->
  false.


to_integer(Bin) when is_binary(Bin)->
    list_to_integer(binary_to_list(Bin))
;
to_integer(Bin) when is_list(Bin)->
    list_to_integer(Bin)
.

json_decode(Json)->
        jiffy:decode(Json).

json_encode(Doc)->
        jiffy:encode(Doc).

to_localtime(T)->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_datetime(T),
    StrTime = lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])),
    StrTime.

-spec hexstring( binary() ) -> list().
hexstring(<<X:128/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~32.16.0b", [X]));
hexstring(<<X:160/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~40.16.0b", [X]));
hexstring(<<X:256/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~64.16.0b", [X]));
hexstring(<<X:512/big-unsigned-integer>>) ->
    lists:flatten(io_lib:format("~128.16.0b", [X])).

