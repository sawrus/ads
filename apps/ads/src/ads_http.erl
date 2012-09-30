-module(ads_http).

% API
-export([start/0, stop/0]).

%% HTTP Misultin callbacks

% Start <M> HTTP server
start() ->
    {ok, Port} = application:get_env(http_port),
    {ok, Folder} = application:get_env(http_folder),
    Conn = ads_data:open(),
    misultin:start_link([
        {port, Port},
        {static, Folder},
        {loop, fun(Req) -> ads_req:handle(Req, Conn) end}
    ]).

% Stop <M> HTTP server
stop() ->
    misultin:stop().
