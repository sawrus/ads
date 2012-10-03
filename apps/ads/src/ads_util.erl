-module(ads_util).

%% API
-export([genkey/1, validate/2]).

%% Application callbacks
genkey(Args) ->
    F = fun({Key, Value}, Acc) ->
        [lists:flatten(Value ++ ":") | Acc]
    end,
    K = lists:flatten(lists:reverse(lists:foldl(F, [], Args))),
    K.

validate([], []) -> true;
validate(L1, L2) when length(L1) =/= length(L2)-> false;
validate([{A, _} | T1], [E | T2]) -> 
    R = string:equal(A, E),
    if 
        false == R -> false;
        true -> validate(T1,T2)    
    end.

