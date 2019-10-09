


%-define(DEBUG,1).

-define(TURNOFFCACHE,1).

-ifdef(DEBUG).

-define('CONSOLE_LOG'(Str, Params),  lager:info(Str, Params) ).
-define('LOG_DEBUG'(Str, Params), lager:debug(Str, Params) ).


-else.

-define('CONSOLE_LOG'(Str, Params),  true).
-define('LOG_DEBUG'(Str, Params),  true ).


-endif.
-define(LOCAL_CACHE, d).

-define(KEY_PREFIX, d).



-record(monitor,{
                 pid
                }).



