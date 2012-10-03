-module(ads_data).

%% API
-export([get/2, put/3, open/0, get_stat/2, set_stat/3]).

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
%% JSON serialize util functions
%% json_eep:term_to_json({[{Key, Value}]}),

%%
%% Stat functions
%%
inc_stat(Stat, IncNumber) -> inc_stat(Stat, IncNumber, 1).
inc_stat(Stat, IncNumber, IncValue) ->
    lists:sublist(Stat, IncNumber - 1) ++
    [lists:nth(IncNumber, Stat) + IncValue] ++
    lists:nthtail(IncNumber, Stat).

get_stat(Key, Conn) ->
    {ok, Value} = get(Key, Conn),
    if
        undefined == Value ->
            Stat = lists:duplicate(?STAT_SIZE, ?STAT_NIL);
        true ->
            Stat = binary_to_list(Value)
    end,
    Stat.

set_stat(StatNumber, Key, Conn) ->
    {ok, Stat} = get(Key, Conn),
    if
        undefined == Stat ->
            put(Key, lists:duplicate(?STAT_SIZE, ?STAT_NIL), Conn);
        true ->
            NewStat = inc_stat(binary_to_list(Stat), StatNumber),
            put(Key, NewStat, Conn)
    end.
