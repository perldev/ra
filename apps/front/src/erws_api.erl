-module(erws_api).

-include("erws_console.hrl").


% Behaviour cowboy_http_handler
-export([init/2, terminate/2,
         hexstring/1, 
         get_key_dict/3,
         get_time/1,
         json_encode/1, 
         json_decode/1, 
         dict_to_json/1, 
         format_date/1,
         to_binary/1]).




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
read_body_to_console(Req0, Accum) ->
    case cowboy_req:read_body(Req0, #{length => infinity}) of
        {ok, Data, Req} ->
            {Req, <<Accum/binary, Data/binary>>};
        {more, Data, Req} ->
            read_body_to_console(Req, <<Accum/binary, Data/binary>>)
    end.                
                
        
% Should never get here.
handle(Req, State) ->
      ?CONSOLE_LOG("====================================~nrequest: ~p ~n", [Req]),
      Path = cowboy_req:path_info(Req),
      ?CONSOLE_LOG("====================================~npath: ~p ~n", [Path]),
      ?CONSOLE_LOG("reading body", []),
      
%       {Req2, Body} = read_body_to_console(Req, <<>>),
      
%       ?CONSOLE_LOG("finishing ~p", [Body]),
      
      {ok, Body, Req2 } = cowboy_req:read_body(Req, #{length => infinity}),               
      ?CONSOLE_LOG("====================================~nbody: ~p ~n", [Body]),
      case process(Path, Body, Req2, State) of
      	  {Code, json, Json, ResReqLast, NewState }->
		?CONSOLE_LOG("got request result: ~p~n", [Json]),
		api_table_holder:api_stat(Code, Path),
		cowboy_req:reply(Code, headers_json_plain(), json_encode(Json), ResReqLast);
	  {json, Json, ResReqLast, NewState }->
		?CONSOLE_LOG("got request result: ~p~n", [Json]),
                api_table_holder:api_stat(200, Path),
		cowboy_req:reply(200, headers_json_plain(), json_encode(Json), ResReqLast);
		%{ok, JsonReq, NewState};
          {raw_answer, {Code, Binary, Headers }, ResReqLast, NewState } ->
                api_table_holder:api_stat(Code, Path),
		cowboy_req:reply(Code, Headers, Binary, ResReqLast)
      end.      

terminate(_Req, _State) -> ok.

false_response(Code, Req, State)->
   {raw_answer, {Code, <<"{\"status\":\"false\"}">>, headers_json_plain() },  Req, State}.
   
false_response(Req, State)->
   {raw_answer, {500, <<"{\"status\":\"false\"}">>, headers_json_plain() },  Req, State}.
 
wait_response(Req, State)->
   {raw_answer, {502, <<"{\"status\":\"wait\",\"timeout\":1000}">>, headers_json_plain() },  Req, State}.
   
true_response(Req, State)->
   {raw_answer, {200, <<"{\"status\":\"true\"}">>, headers_json_plain() },  Req, State}.

raw_answer(Raw, Req, State)->
   raw_answer(200, Raw, Req, State).
   
raw_answer(Code, Raw, Req, State)->
   {raw_answer, {Code, Raw, headers_text_html() },  Req, State}.
    
 
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

process([Expert, <<"once">>, Name],  Body, Req, State)->
    ?CONSOLE_LOG("call aim ~p ~n", [Body]),
%     {fact,1,2,3,4,5}
%     [ {[{<<"name">>,<<"X">>}]}, 1,3,4]
    Res = lists:map(fun(Elem)->  case  Elem of 
                                          {[{<<"name">>, NameX}]} -> { to_atom(NameX) };
                                          Value -> to_list(Value)   
                                       end 
                    end, 
                    jiffy:decode(Body) ),
    Atom = to_atom(Name),
    CallBody = list_to_tuple([Atom|Res]),
    ?CONSOLE_LOG("call aim ~p ~n", [CallBody]),
    Result = api_table_holder:erlog_once4export(Expert, CallBody),
    ?CONSOLE_LOG("result aim ~p ~n", [Result]),

    case Result of
        fail -> 
            ListJson = {[{<<"status">>, <<"fail">>}]},
            {500, json, ListJson, Req, State};
        {error, Error}->
            ErrorDesc = erlog_io:write1(Error),
            ListJson = {[{<<"status">>, <<"error">>}, {<<"description">>, to_binary(ErrorDesc) }]},
            {500, json, ListJson, Req, State};
        false ->
           false_response(200, Req, State);
        Success ->
            ResultL = lists:map(fun({NameX, Val}) ->
                                          {[{to_binary(NameX),  to_binary(Val) }]}
                                    end, Success),
            {json, {[{<<"status">>, true}, {<<"result">>, ResultL}]}, Req, State} 
    end
;
process([ExpertName, <<"once">>],  Body, Req, State)->
    ?CONSOLE_LOG("call aim ~p ~n", [Body]),
%     {fact,1,2,3,4,5}
%     [ {[{<<"name">>,<<"X">>}]}, 1,3,4]
    case erlog_io:read_string(unicode:characters_to_list(Body)) of
        {error, Error}->
            ErrorDesc = erlog_io:write1(Error),
            raw_answer(500, to_binary(ErrorDesc), Req, State);
        {ok, Terms}->
          case api_table_holder:erlog_once4export(ExpertName, Terms) of
              {error, Error}->
                    ErrorDesc = erlog_io:write1(Error),
                    ListJson = {[{<<"status">>, <<"error">>}, {<<"description">>, to_binary(ErrorDesc) }]},
                    {500, json, ListJson, Req, State};
              fail -> 
                    ListJson = {[{<<"status">>, <<"fail">>}]},
                    {500, json, ListJson, Req, State};
              false ->
                    {json, {[{<<"status">>, false}]}, Req, State};
              Success ->
%                   ?CONSOLE_LOG("result aim ~p ~n", [Success]),
                    ResultL = lists:map(fun({NameX, Val}) ->
                                            {[{to_binary(NameX),  to_binary(Val) }]}
                                        end, Success),
                    {json, {[{<<"status">>, true}, {<<"result">>, ResultL}]}, Req, State}
          end
    end
;
process([<<"once">>, Name],  Body, Req, State)->
    ?CONSOLE_LOG("call aim ~p ~n", [Body]),
%     {fact,1,2,3,4,5}
%     [ {[{<<"name">>,<<"X">>}]}, 1,3,4]
    Res = lists:map(fun(Elem)->  case  Elem of 
                                          {[{<<"name">>, NameX}]} -> { to_atom(NameX) };
                                          Value -> to_list(Value)   
                                       end 
                    end, 
                    jiffy:decode(Body) ),
    Atom = to_atom(Name),
    CallBody = list_to_tuple([Atom|Res]),
    ?CONSOLE_LOG("call aim ~p ~n", [CallBody]),
    Result = api_table_holder:erlog_once(CallBody),
    ?CONSOLE_LOG("result aim ~p ~n", [Result]),
    case Result of
        {error, Error}->
            ErrorDesc = erlog_io:write1(Error),
            ListJson = {[{<<"status">>, <<"error">>}, {<<"description">>, to_binary(ErrorDesc) }]},
            {500, json, ListJson, Req, State};
        fail -> 
            ListJson = {[{<<"status">>, <<"fail">>}]},
            {500, json, ListJson, Req, State};
        false ->
           false_response(200, Req, State);
        Success ->
                ResultL = lists:map(fun({NameX, Val}) ->
                                          {[{to_binary(NameX),  to_binary(Val) }]}
                                    end, Success),
                {json, {[{<<"status">>, true}, {<<"result">>, ResultL}]}, Req, State} 
    end
;
process([<<"stat">>],  _Body, Req, State)->
     ResultL = lists:map(fun( { {Path, Code}, Count}  ) ->   
                       {[{<<"path">>, binary_join(Path, <<"/">>)  }, {<<"code">>, Code} ,{<<"count">>,Count}]}
                        end,  api_table_holder:get_api_stat()),
     {json, {[{<<"status">>, true}, {<<"result">>, ResultL}]}, Req, State} 
;
process([<<"memory">>],  _Body, Req, State)->
    
    WholeSize = ets:info(?SYSTEMS, memory),
    Systems = ets:foldl(fun({Key, _Erlog, _Q}, Accum)->  
%                             Size = erts_debug:size(Erlog),
                            [ Key | Accum ]
                        end, [], ?SYSTEMS),
     {json, {[{<<"status">>, true}, {<<"memory">>, WholeSize}, {<<"expert_systems">>, Systems } ]}, Req, State} 
;
process([<<"check_store">>, U],  Body, Req, State )->
      Res = api_table_holder:check_store(U),
      ListJson = {[ {<<"status">>, Res} ]},
      {200, json, ListJson, Req, State}
;
process([<<"create_store">>, U],  Body, Req, State )->
      Res = api_table_holder:create_store(U),
      ListJson = {[ {<<"status">>, Res} ]},
      {200, json, ListJson, Req, State};
process([<<"create_expert">>, U],  Body, Req, State )->
    ?CONSOLE_LOG("create expert system  ~n", []),
    
        File = api_table_holder:tmp_export_file(),
        %%HACK add \n at the end of file for correct parsing
        file:write_file(File, <<Body/binary, "\n\n\n">>), 
        case  erlog_io:read_file(File) of 
         {ok, MyTerms } ->
            api_table_holder:create_expert(U, MyTerms),
            true_response(Req, State);
         {error, Error}->
            ErrorDesc = erlog_io:write1(Error),
            ListJson = {[{<<"status">>, <<"error">>}, {<<"description">>, to_binary(ErrorDesc) }]},
            {500, json, ListJson, Req, State}
        end
;
process([<<"add_consistent">>],  _Body, Req, State )->
    ?CONSOLE_LOG("process load  all from dump ~n", []),
    api_table_holder:add_consisten_knowledge(),
    true_response(Req, State)    
;
process([<<"dump_db">>, FileName],  _Body, Req, State )->
    ?CONSOLE_LOG("process load  all from dump ~n", []),
    api_table_holder:save_db(FileName),
    true_response(Req, State)    
;
process([<<"dump_db">>],  _Body, Req, State )->
    ?CONSOLE_LOG("process load  all from dump ~n", []),
    api_table_holder:save_db(),
    true_response(Req, State)    
;
process([<<"load_from_dump">>],  _Body, Req, State )->
    ?CONSOLE_LOG("process load  all from dump ~n", []),
    api_table_holder:load_from_dump(),
    true_response(Req, State)    
;
process([<<"flush">>],  _Body, Req, State )->
    ?CONSOLE_LOG("process load remove all temp ~n", []),
    api_table_holder:flush_erlog(),
    true_response(Req, State)    
;
%DEPRECATED
process([<<"load_from_db">>],  _Body, Req, State )->
    ?CONSOLE_LOG("process load code from db   ~n", []),
    api_table_holder:load_from_db(),
    true_response(Req, State)    
;
process([<<"load">>],  Body, Req, State )->
    ?CONSOLE_LOG("process load code from  ~p ~n", [Body]),
    api_table_holder:erlog_load_code(Body),
    true_response(Req, State)    
;
process([ExperSystem, <<"lookup">>],  Body, Req, State )->
    ?CONSOLE_LOG("process search from  ~p ~n",[Body]),
    List = api_table_holder:lookup(ExperSystem, Body, 60000),
    Res = lists:map(fun([Name, Value, Ets])->   {[{<<"type">>, Name}, {<<"value">>, json_decode(Value)}, {<<"date">>,  list_to_binary(format_date(Ets)) }]}   end,  List),
    ListJson = {[{<<"status">>, true}, {<<"result">>, Res}]},
    {json, ListJson, Req, State}
;
process([<<"lookup">>],  Body, Req, State )->
    ?CONSOLE_LOG("process search from  ~p ~n",[Body]),
    List = api_table_holder:lookup(Body),
    Res = lists:map(fun([Name, Value, Ets])->   {[{<<"type">>, Name}, {<<"value">>, json_decode(Value)}, {<<"date">>,  list_to_binary(format_date(Ets)) }]}   end,  List),
    ListJson = {[{<<"status">>, true}, {<<"result">>, Res}]},
    {json, ListJson, Req, State}
;
process([ExpertSystem, <<"assert">>, Name],  Body, Req, State )->
    ?CONSOLE_LOG("process request from ~p ~p ~n",[Name, Body]),
    case catch erws_api:json_decode(Body) of 
            {'EXIT', Error}->
                    ?LOG_DEBUG("cant process rule  ~p ~n ~p", [{Name, Body}, Error]),
                    ListJson = {[{<<"status">>, true}, {<<"decription">>, <<"malformed json">>}]},
                    {500, json, ListJson, Req, State};
            DecodeRule -> 
                Sign = generate_key(Body),
                DecodeRuleL = lists:map(fun convert/1, DecodeRule),
                % YOU should check reponse of erl
                Res = api_table_holder:assert(ExpertSystem, Name, DecodeRuleL, Body, list_to_binary(Sign)),
                true_response(Req, State)
    end
;  
process([<<"assert">>, Name],  Body, Req, State )->
    ?CONSOLE_LOG("process request from ~p ~p ~n",[Name, Body]),
    Sign = generate_key(Body),
    case catch erws_api:json_decode(Body) of 
            {'EXIT', Error}->
                    ?LOG_DEBUG("cant process rule  ~p ~n ~p", [{Name, Body}, Error]),
                    ListJson = {[{<<"status">>, true}, {<<"decription">>, <<"malformed json">>}]},
                    {500, json, ListJson, Req, State};
            DecodeRule -> 
                DecodeRuleL = lists:map(fun convert/1, DecodeRule),
                api_table_holder:assert(Name, DecodeRuleL, Body, list_to_binary(Sign)),
                true_response(Req, State)
    end
;    
process(Path, _Body, Req, State)->
     ?CONSOLE_LOG("undefined request from ~p ~p ~n",[Path, Req]),
     false_response(404, Req, State).
  

auth_user(Req, Body, State)->
       undefined
.


% -spec assertdb(string(), string(), list()) -> true|false.
assertdb(ExpertSystem, NameOfRule, Params):-
   NewParamas = lists:map(fun to_binary/1, Params),
   Body = erws_api:json_encode(NewParamas),
   Sign = generate_key(Body),
   %% all expert systems created through api is binary
   Res = api_table_holder:assert(to_binary(ExpertSystem), to_binary(NameOfRule), Params, Body, list_to_binary(Sign)),
   Res.

   

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
   lists:flatten(io_lib:format("~4..0w/~2..0w/~2..0w ~2..0w:~2..0w:~2..0w",[Year,Month,Day,Hour,Minute,Second])).


   
   
     
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

     
    
convert(Elem) when is_binary(Elem)->
    to_list(Elem);
convert(Elem) when is_list(Elem)->
    Elem;    
convert(Elem) when is_integer(Elem)->
    Elem;
convert(Elem) when is_float(Elem)->
    Elem.    
    
to_atom(Name) when is_atom(Name) ->
   Name;
to_atom(Name) when is_binary(Name) ->
   to_atom(binary_to_list(Name));
to_atom(Name) when is_list(Name) ->
   to_atom(list_to_atom(Name)).

to_list(Name) when is_binary(Name)->
    unicode:characters_to_list(Name);
to_list(Name) when is_list(Name)->
    Name.
   
to_binary(Name) when is_binary(Name) ->
   Name;
to_binary(Name) when is_tuple(Name) ->
   NameDesc = erlog_io:write1(Name),
   to_binary(NameDesc);
to_binary(Name) when is_integer(Name) ->
   Name; %% do not commit
to_binary(Name) when is_list(Name) ->
   to_binary(list_to_binary(Name));
to_binary(Name) when is_atom(Name) ->
   to_binary(atom_to_list(Name)).
   
   
-spec binary_join([binary()], binary()) -> binary().
binary_join([], _Sep) ->
  <<>>;
binary_join([Part], _Sep) ->
  Part;
binary_join(List, Sep) ->
  lists:foldr(fun (A, B) ->
    if
      bit_size(B) > 0 -> <<A/binary, Sep/binary, B/binary>>;
      true -> A
    end
  end, <<>>, List).
   
