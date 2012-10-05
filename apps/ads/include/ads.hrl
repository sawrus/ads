% Logging modes
-define(LOG_DEBUG(Str, Args), 
    erlang:apply(error_logger, info_msg, 
        [lists:concat(["[DEBUG]	pid: ", pid_to_list(self()), "~n	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args]
    )
).
-define(LOG_INFO(Str, Args), 
    erlang:apply(error_logger, info_msg, 
        [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args]
    )
).
-define(LOG_WARNING(Str, Args), 
    erlang:apply(error_logger, warning_msg, 
        [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args]
    )
).
-define(LOG_ERROR(Str, Args), 
    erlang:apply(error_logger, error_msg, 
        [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args]
    )
).

% Expected parameters (for GET requests)
-define(ADJSON, ["Platform", "Version"]).
-define(ADSTAT, ["Campaign", "Customer"]).

% Initial statistic parameters
-define(STAT_SIZE, 3).
-define(STAT_NIL, 0).
-define(STAT_INC, 1).

% Another parameters
-define(STAT_SEPARATOR, ":").
-define(HTML_SEPARATOR, "$$$").
