-module(ads_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	{ok, Port} = application:get_env(http_port),
    ads_http:start(Port).

stop(_State) ->
	ads_http:stop().
	
	