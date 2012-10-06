%%
%% ADS - Module with call the next point:
%%  * Starting Misultin HTTP server with env parameters
%%  * Handling GET requests in loop
%%  * Stop functionality
%%
%% Usage:
%%  * ads_http:start();
%%  * ads_http:stop();
-module(ads_http).
-vsn("0.1").

%%% API
-export([start/0, stop/0]).

%%% HTTP Misultin callbacks

% Start <M> HTTP server
start() ->
    {ok, Port} = application:get_env(http_port),
    {ok, Folder} = application:get_env(http_folder),
    {ok, Compress} = application:get_env(http_compress),
    Conn = ads_data:open(),
    misultin:start_link([
        {port, Port},
        {access_log, fun(AccessInfo) -> access_log(AccessInfo) end},
        {compress, Compress},
        {static, Folder},
        {loop, fun(Req) -> ads_req:handle(Req, Conn) end}
    ]).

% callback on access log
access_log({PeerAddr, DateTime, RequestLine, HttpCode, ContentLength}) ->
    io:format("~s - - [~s] \"~s\" ~p ~p~n", [PeerAddr, DateTime, RequestLine, HttpCode, ContentLength]).

% Stop <M> HTTP server
stop() ->
    misultin:stop().
