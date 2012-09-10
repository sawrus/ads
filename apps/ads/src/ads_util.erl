-module(ads_util).

%% API
-export([genkey/1]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

genkey(Args)->
  F = fun({Key, Value}, Acc) ->
    [lists:flatten(Value++":") | Acc]
  end,
  K = lists:flatten(lists:reverse(lists:foldl(F, [], Args))),
  K.