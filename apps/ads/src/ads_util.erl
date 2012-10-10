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
-export([index_of/2]).
-export([sub/3, gsub/3]).
-export([build_page/2, get_page/2]).

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

index_of(Item, List) -> index_of(Item, List, 1).
index_of(_, [], _)  -> -1;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

-spec sub(Str::string, Old::string(), New::string()) -> string().
sub(Str, Old, New) ->
    RegExp = "\\Q"++Old++"\\E",
    re:replace(Str,RegExp,New,[multiline, {return, list}]).

-spec gsub(Str::string, Old::string(), New::string()) -> string().
gsub(Str, Old, New) ->
    RegExp = "\\Q"++Old++"\\E",
    re:replace(Str,RegExp,New,[global, multiline, {return, list}]).

save_file(FilePath, Url, Conn) ->
     {ok, BinaryFileContent} = file:read_file(FilePath),
     FileModifiedTime = io_lib:format("~p", [ads_util:get_mtime(FilePath)]),
     FileContent = binary_to_list(BinaryFileContent),
     ads_data:put(Url, string:join([FileModifiedTime, FileContent], ?HTML_SEPARATOR), Conn),
     get_page(Url, Conn).
     
get_page("", _) -> "";
get_page(PageName, Conn) ->
    {ok, Folder} = application:get_env(http_folder),
    FilePath = filename:join([Folder, PageName]),
    FileExist = filelib:is_file(FilePath),
    Result = ads_data:get(PageName, Conn),
    if 
        false == FileExist -> PageName;
        true ->
        case Result of 
            {ok, undefined} -> 
                save_file(FilePath, PageName, Conn);
            {ok, BinaryFileContent} -> 
                ActualModifiedTime = integer_to_list(ads_util:get_mtime(FilePath)),
                [CachedModifiedTime | FileContent] = string:tokens(binary_to_list(BinaryFileContent), ?HTML_SEPARATOR),
                if 
                    ActualModifiedTime =/= CachedModifiedTime ->
                        save_file(FilePath, PageName, Conn);
                    true ->
                        build_page(FileContent, Conn)
                end;
            _ -> PageName
        end
    end.

build_page([], _) -> "";
build_page([PagePart | T], Conn) ->
    IsHtmlPart = lists:suffix(".html", PagePart),
    if 
        false == IsHtmlPart ->
            Result = PagePart;
        true ->
            Result = get_page(PagePart, Conn)
    end,
    Result ++ build_page(T, Conn).
