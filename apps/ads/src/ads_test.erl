-module(ads_test).

%% Eunit macros
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


%% ===================================================================
%% Application test callbacks
%% ===================================================================
		
-ifdef(TEST).

app_start_test() ->
    ok = application:start(ads),
    ?assert(undefined == whereis(ads_sup)).

get_set_test() ->
    C = c(),
    ?assertMatch({ok, _}, eredis:q(C, ["DEL", foo])),

    ?assertEqual({ok, undefined}, eredis:q(C, ["GET", foo])),
    ?assertEqual({ok, <<"OK">>}, eredis:q(C, ["SET", foo, bar])),
    ?assertEqual({ok, <<"bar">>}, eredis:q(C, ["GET", foo])).

generate_key_test() ->
	?assertEqual("A", ads_util:generate_key([{"A","A"}])).
	
handle_adtest_test()->
	C = c(),
	K = ads_util:generate_key([{"K1","V1"}, {"K2","V2"}]),
	?assertEqual("V1V2", K), 
	ads_data:put(K, K, C),
	R = ads_data:get(K, C),
	{ok, V} = R,
	?assertEqual(<<"V1V2">>, V).
	
c() ->
    Res = eredis:start_link(),
    ?assertMatch({ok, _}, Res),
    {ok, C} = Res,
    C.		
	
-endif.


	
	