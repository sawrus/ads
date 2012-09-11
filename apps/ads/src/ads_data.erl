-module(ads_data).

%% API
-export([get/2, put/3, open/0, get_stat/2, set_stat/3, alloc_stat/2]).


%% macros
-define(STAT_VALUE_COUNT, 3).

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


alloc_stat(_, 0)-> [];
alloc_stat(Stat, Size) when Size == length(Stat) -> Stat;
alloc_stat(Stat, Size) -> alloc_stat([0 | Stat], Size).

empty_stat(Key)->
	Value = alloc_stat([], ?STAT_VALUE_COUNT),
	JSON = to_json(Key, Value),
	JSON.

get_stat(Key, Conn) ->
	{ok, Stat} = get(Key, Conn),
	if 
		undefined == Stat->
			empty_stat(Key);
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
%% Stat JSON: "{\"key\": [1, 2, 3] }"
%%

parse_stat(JsonStat) ->
	T1 = json_eep:json_to_term(JsonStat),
	L1 = element(1, T1),
	[H|[]]  = L1, 
	L2 = element(2, H),
	L3 = numlist(L2),
	L3.

to_json(Key, Value)->
	JSON = json_epp:term_to_json({[{Key, Value}]}),
	JSON.

inc_stat([], _, _) -> [];
inc_stat([H|Stat], CountKey, IncNumber) when CountKey == IncNumber -> [H+1| Stat];
inc_stat([H|Stat], CountKey, IncNumber) -> [H | inc_stat(Stat, CountKey - 1, IncNumber)].

set_stat(StatNumber, Key, Conn)->
	{ok, Stat} = get(Key, Conn),
	if 
		undefined == Stat ->
			put(Key, empty_stat(Key), Conn);
		true ->
		CurStat = parse_stat(Stat),
		NewStat = inc_stat(CurStat, ?STAT_VALUE_COUNT, StatNumber),
		put(Key, to_json(Key, NewStat), Conn)
	end.	


