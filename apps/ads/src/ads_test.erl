-module(ads_test).

%% Eunit macros
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-export([data_get/0, data_rget/0]).

%% ===================================================================
%% Application test callbacks
%% ===================================================================
		
-ifdef(TEST).

app_start_test() ->
    ok = application:start(ads),
    ?assert(undefined == whereis(ads_sup)).
	
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

data_putget_test()->
	Conn = data_conn_test(),
	Key = ads_util:genkey([{"A","A"}, {"B","B"}]),
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
	
util_genkey_test() ->
	?assertEqual("A:", ads_util:genkey([{"A","A"}])),
	?assertEqual("A:B:", ads_util:genkey([{"A","A"},{"B","B"}])).
	
-endif.

data_get()->
	Conn = ads_data:open(),
	R = ads_data:get("A:B:C", Conn),
	{ok, V} = R,
	io:format("V=~p", [V]).

data_rget()->
	Conn = ads_data:open(),
	{ok, undefined} = eredis:q(Conn, ["GET", "A:B:C:"]).
	
	