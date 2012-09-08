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
    ?assertNot(undefined == whereis(ads_sup)).

-endif.
	
	