-module(ads_sup).

% API
-export([start/1, stop/0]).

%% ===================================================================
%% HTTP Misultin callbacks
%% ===================================================================

% Start <M> HTTP server
start(Port) ->
  misultin:start_link([{port, Port}, {loop, fun(Req) -> ads_req:handle(Req) end}]).

% Stop <M> HTTP server
stop() ->
  misultin:stop().
