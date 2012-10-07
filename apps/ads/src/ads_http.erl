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

% includes
-include("../include/ads.hrl").

%%% API
-export([start/0, stop/0]).

%%% HTTP Misultin callbacks

% Start <M> HTTP server
start() ->
    {ok, Port} = application:get_env(http_port),
    {ok, Folder} = application:get_env(http_folder),
    {ok, Compress} = application:get_env(http_compress),
    {ok, Ssl} = application:get_env(https_enable),
    Conn = ads_data:open(),
    ServerOptions = [
        {port, Port},
        {access_log, fun(AccessInfo) -> access_log(AccessInfo) end},
        {compress, Compress},
        {static, Folder},
        {loop, fun(Req) -> 
            try
                ads_req:handle(Req, Conn) 
            catch        
                Exception : Reason -> 
                    ETempl = "Exception: ~p~nReason: ~p~nStacktrace: ~p",
                    EList  = [Exception, Reason, erlang:get_stacktrace()],
                    ?LOG_DEBUG(ETempl, EList),
                    Message = io_lib:format(ETempl, EList),
                    Req:respond(500, [{"Content-Type", "text/plain"}], Message)
            end
        end
        }
    ],
    ?LOG_DEBUG("Start with ServerOptions: ~p", [ServerOptions]),
    if 
        false == Ssl ->
            misultin:start_link(ServerOptions);
        true ->
            {ok, SslOptions} = application:get_env(https_options),
            misultin:start_link(lists:append(ServerOptions, [{ssl, SslOptions}]))
    end.

% callback on access log
access_log({PeerAddr, DateTime, RequestLine, HttpCode, ContentLength}) ->
    io:format("~s - - [~s] \"~s\" ~p ~p~n", [PeerAddr, DateTime, RequestLine, HttpCode, ContentLength]).

% Stop <M> HTTP server
stop() ->
    misultin:stop().
