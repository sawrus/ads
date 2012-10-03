-module(ads_test).

%% Eunit macros
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Application test callbacks

-ifdef(TEST).

%% App module tests
app_start_test() ->
    ok = application:start(ads),
    ?assert(undefined == whereis(ads_sup)).

%% Data module tests
data_conn_test() ->
    Res = eredis:start_link(),
    ?assertMatch({ok, _}, Res),
    {ok, Conn} = Res,
    Conn.

data_redis_test() ->
    Conn = data_conn_test(),
    ?assertMatch({ok, _}, eredis:q(Conn, ["DEL", foo])),
    ?assertEqual({ok, undefined}, eredis:q(Conn, ["GET", foo])),
    ?assertEqual({ok, <<"OK">>}, eredis:q(Conn, ["SET", foo, bar])),
    ?assertEqual({ok, <<"bar">>}, eredis:q(Conn, ["GET", foo])),
    ?assertEqual({ok, <<"1">>}, eredis:q(Conn, ["DEL", foo])).

data_putget_test() ->
    Conn = data_conn_test(),
    Key = ads_util:genkey([{"A", "A"}, {"B", "B"}]),
    ?assertEqual("A:B:", Key),
    ?assertEqual({ok, undefined}, ads_data:get(Key, Conn)),
    ads_data:put(Key, Key, Conn),
    {ok, Value} = ads_data:get(Key, Conn),
    ?assertEqual(<<"A:B:">>, Value),
    ?assertMatch({ok, _}, eredis:q(Conn, ["DEL", Key])),
    ?assertMatch({ok, _}, eredis:q(Conn, ["DEL", "A:B:C:"])),
    ?assertEqual({ok, undefined}, ads_data:get("A:B:C:", Conn)),
    ads_data:put("A:B:C:", "A:B:C:", Conn),
    ?assertEqual({ok, <<"A:B:C:">>}, ads_data:get("A:B:C:", Conn)).

data_getstat_test() ->
    Conn = ads_data:open(),
    ?assertMatch({ok, _}, eredis:q(Conn, ["DEL", "K"])),
    ads_data:set_stat(0, "K", Conn),
    ?assertEqual([0, 0, 0], ads_data:get_stat("K", Conn)),
    ads_data:set_stat(1, "K", Conn),
    ?assertEqual([1, 0, 0], ads_data:get_stat("K", Conn)),
    ads_data:set_stat(2, "K", Conn),
    ?assertEqual([1, 1, 0], ads_data:get_stat("K", Conn)),
    ads_data:set_stat(3, "K", Conn),
    ?assertEqual([1, 1, 1], ads_data:get_stat("K", Conn)).

%%
%% Util module tests
%%

util_genkey_test() ->
    ?assertEqual("A:", ads_util:genkey([{"A", "A"}])),
    ?assertEqual("A:B:", ads_util:genkey([{"A", "A"}, {"B", "B"}])).

util_validate_test() ->
    ?assertEqual(true, ads_util:validate([{"A", "B"}], ["A"])),
    ?assertEqual(false, ads_util:validate([{"A", "B"}], ["B"])),
    ?assertEqual(true, ads_util:validate([{"A", "B"},{"A2", "B2"},{"A3","B3"}], ["A","A2","A3"])),
    ?assertEqual(false, ads_util:validate([{"A", "B"},{"A2", "B2"},{"A3","B3"}], ["A","A2","B3"])).

-endif.

