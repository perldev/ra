


-define(DEBUG,1).

%-define(TURNOFFCACHE,1).

-ifdef(DEBUG).

-define('CONSOLE_LOG'(Str, Params),  io:format(Str, Params) ).
-define('LOG_DEBUG'(Str, Params), io:format(Str, Params) ).


-else.

-define('CONSOLE_LOG'(Str, Params),  true).
-define('LOG_DEBUG'(Str, Params),  true ).


-endif.
-define(LOCAL_CACHE, d).

-define(KEY_PREFIX, d).
-define(UNIQ, uniq).
-define(SYSTEMS, systems).
-define(STAT, stat).



-record(monitor,{
                 pid,
                 pid1,
                 erlog, 
                 erlog1, 
                 dump_name=undefined,
                 db_loaded=false,
                 systems=[],
                 current_version,
		 timer,
		 slaves=[]
                }).



