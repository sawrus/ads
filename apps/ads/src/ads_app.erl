%% 
%% ADS - Main module of application 
%%
%% Usage:
%%  application:start(ads).
-module(ads_app).
-vsn("0.1").
-behaviour(application).

%%% API
-export([start/2, stop/1]).

%%% API functions

%% @doc Starting Ad server
start(_StartType, _StartArgs) ->
    ads_http:start().

%% @doc Stop of server
stop(_State) ->
    ads_http:stop().
	
	
