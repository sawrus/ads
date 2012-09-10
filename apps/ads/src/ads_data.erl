-module(ads_data).

%% API
-export([get/2, put/3, open/0, get_stat/2, set_stat/3, parse_stat/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

get(Key, Conn) ->
    Response = eredis:q(Conn, ["GET", Key]),
    Response.

put(Key, Value, Conn) ->
    {ok, <<"OK">>} = eredis:q(Conn, ["SET", Key, Value]).

open() ->
    Response = eredis:start_link(),
    {ok, Conn} = Response,
    Conn.

empty_stat()->
	Stat = io_lib:format("{\"~s\",\"~s\",\"~s\"}", [
		integer_to_list(0), 
		integer_to_list(0), 
		integer_to_list(0) 
	]),
	Stat.

get_stat(Key, Conn) ->
	{ok, Stat} = get(Key, Conn),
	if 
		undefined == Stat->
			empty_stat();
		true->
			Stat
	end.

bin_to_num(Bin) ->
    N = binary_to_list(Bin),
    case string:to_float(N) of
        {error,no_float} -> list_to_integer(N);
        {F,_Rest} -> F
    end.

numlist([H|T])-> [bin_to_num(H) | numlist(T)];
numlist([])-> [].

%%
%% Stat JSON: "{\"key\": [\"1\",\"2\",\"3\"] }"
%%

parse_stat(JsonStat) ->
	T1 = json_eep:json_to_term(JsonStat),
	L1 = element(1, T1),
	[H|[]]  = L1, 
	L2 = element(2, H),
	L3 = numlist(L2),
	L3.

set_stat(StatType, Key, Conn)->
	ok.
		


