%%
%% ADS - Ad server utility
%%
-module(ads_util).
-vsn("0.1").

%% includes
-include("../include/ads.hrl").
-include_lib("kernel/include/file.hrl").

%% API
-export([genkey/1, genkey/2]).
-export([validate/2]).
-export([get_mtime/1]).

%% Application callbacks

%% @doc Generate Key based on Args proplist
%% Usage:
%% ads_util:genkey([{"A", "B"}, {"C", "D"}]) returns string "B:D"
-spec genkey(TupleList::[tuple()]) -> string().
genkey(TupleList) -> genkey(TupleList, ?STAT_SEPARATOR).

-spec genkey(TupleList::[tuple()], Sep::string()) -> string().
genkey([{_, H} | T], Sep) ->
    H ++ lists:append([Sep ++ Key || {_, Key} <- T]).

%% @doc Validate that keys of proplist equals the second argument of this function.
%% Usage:
%% ads_util:validate([{A, B}, {C, D}], [A, C]) returns true
%% ads_util:validate([{A, B}, {C, D}], [B, D]) returns false
-spec validate(PropList, ValList) -> ValResult when
    PropList::[tuple()],
    ValList::[string()],
    ValResult::boolean.
validate([], []) -> true;
validate(L1, L2) when length(L1) =/= length(L2) -> false;
validate([{A, _} | T1], [E | T2]) -> 
    R = string:equal(A, E),
    if 
        false == R -> false;
        true -> validate(T1,T2)    
    end.

%% @doc Getting modification time in seconds of defined File
%% Usage:
%% ads_util:get_mtime(File)
-spec get_mtime(FilePath::string()) -> integer().
get_mtime(FilePath) ->
    {ok, FileInfo} = file:read_file_info(FilePath),
    {Date, Time} = FileInfo#file_info.mtime,
    {YYYY,MM,DD} = Date, {HH,Mi,SS} = Time,
    MTime = YYYY + MM + DD + HH + Mi + SS,
    MTime.
