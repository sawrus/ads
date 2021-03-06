%%
%% ADS - Manipulate data on NOSQL level:
%% Interface for:
%%  * Getting/Put data by KEY
%%  * Getting/Set input statistics
-module(ads_data).
-vsn("0.1").

%% API
% NoSQL callbacks
-export([get/2, put/3, open/0]).
% Statistics callbacks
-export([get_stat/2, set_stat/3]).

% includes
-include("../include/ads.hrl").

%% Application callbacks

%%
%% NoSQL functions
%%
open() ->
    Response = eredis:start_link(),
    {ok, Conn} = Response,
    Conn.

get(Key, Conn) ->
    Response = eredis:q(Conn, ["GET", Key]),
    Response.

put(Key, Value, Conn) ->
    {ok, <<"OK">>} = eredis:q(Conn, ["SET", Key, Value]).

%%
%% Stat functions
%%
inc_stat(Stat, IncNumber) -> inc_stat(Stat, IncNumber, 1).
inc_stat(Stat, IncNumber, IncValue) ->
    lists:sublist(Stat, IncNumber - 1) ++
    [lists:nth(IncNumber, Stat) + IncValue] ++
    lists:nthtail(IncNumber, Stat).


init_stat() ->
    {ok, StatUrls} = application:get_env(stat_urls),
    lists:duplicate(length(StatUrls), ?STAT_NIL).
    
get_stat(Key, Conn) ->
    {ok, Value} = get(Key, Conn),
    if
        undefined == Value ->
            Stat = init_stat();
        true ->
            Stat = binary_to_list(Value)
    end,
    Stat.

set_stat(StatNumber, Key, Conn) ->
    {ok, Stat} = get(Key, Conn),
    if
        undefined == Stat ->
            put(Key, init_stat(), Conn);
        true ->
            NewStat = inc_stat(binary_to_list(Stat), StatNumber),
            put(Key, NewStat, Conn)
    end.
