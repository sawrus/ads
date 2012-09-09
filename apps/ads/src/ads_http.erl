-module(ads_http).

% API
-export([start/1, stop/0]).

%% ===================================================================
%% HTTP Misultin callbacks
%% ===================================================================

% Start <M> HTTP server
start(Port) ->
  C = ads_data:start(),
  misultin:start_link([{port, Port}, {loop, fun(Req) -> ads_req:handle(Req, C) end}]).

% Stop <M> HTTP server
stop() ->
  misultin:stop().
