-module(ads_req).

% includes
-include("../include/ads.hrl").

% API
-export([handle/2]).

%% Handle HTTP request callbacks
handle(Req, Conn) ->
    % get params depending on method
    Method = Req:get(method),
    case Method of
        'GET' ->
            Args = Req:parse_qs();
        'POST' ->
            Args = Req:parse_post()
    end,
    Uri = Req:resource([lowercase, urldecode]),
    % Handle request by parameters
    try
        handle(Method, Uri, Args, Req, Conn)
    catch
        Exception : Reason -> 
            ETempl = "Exception: ~p~nReason: ~p~nStacktrace: ~p",
            EList  = [Exception, Reason, erlang:get_stacktrace()],
            ?LOG_DEBUG(ETempl, EList),
            Message = io_lib:format(ETempl, EList),
            Req:respond(500, [{"Content-Type", "text/plain"}], Message)
    end.


%% Responces 
%% Respone body types: [application/json, plain/html].
calc_stat(IncNumber, Args, Req, Conn) ->
    ValidateResult = ads_util:validate(Args, ?ADSTAT), 
    if 
        false == ValidateResult -> Req:respond(400); 
        true ->
            ads_data:set_stat(IncNumber, ads_util:genkey(Args), Conn),
            Req:respond(200)
    end.
 
prepare_report(Args, Req, Conn) ->
    ValidateResult = ads_util:validate(Args, ?ADSTAT), 
    if 
        false == ValidateResult -> Req:respond(400); 
        true ->
            Key = ads_util:genkey(Args),
            Value = ads_data:get_stat(Key, Conn),
            JSON = json_eep:term_to_json({[{Key, Value}]}),
            Req:ok([{"Content-Type", "application/json"}], JSON)
    end.

%% function for test mode
%% TODO: need to delete this block
build_adjson(Key) ->
    {H, M, S} = time(),
    T = io_lib:format('~2..0b:~2..0b:~2..0b', [H, M, S]),
    E = [{"Extra", "P"}, {"V", "EVALUE"}, {"Key", Key}, {"Time", T}],
    N = [{"Networks", "P"}, {"V", "NVALUE"}, {"Key", Key}, {"Time", T}],
    IN = [{"Networks", "P"}, {"V", "INVALUE"}, {"Key", Key}, {"Time", T}],
    BuildJSON = fun({Param, Value}, Acc) ->
        [lists:flatten(
            io_lib:format("{\"~s\":\"~s\"},\n", [Param, Value])) | Acc
        ]
    end,
    EJ = lists:flatten(lists:reverse(lists:foldl(BuildJSON, [], E))),
    NJ = lists:flatten(lists:reverse(lists:foldl(BuildJSON, [], N))),
    INJ = lists:flatten(lists:reverse(lists:foldl(BuildJSON, [], IN))),
    EJ ++ NJ ++ INJ.
%% TODO: need to delete this block


%% Handle GET requests with URI type of '/ad/**'
%% Respone body types: [application/json].
handle_adjson(Args, Req, Conn) ->
    ValidateResult = ads_util:validate(Args, ?ADJSON), 
    Key = ads_util:genkey(Args),
    {ok, Value} = ads_data:get(Key, Conn),
    if
        false == ValidateResult ->
            Req:respond(400);
        undefined == Value ->
            %% ! test mode !
            NewValue = build_adjson(Key),
            ads_data:put(Key, NewValue, Conn),
            Req:ok([{"Content-Type", "application/json"}], NewValue);
        true ->
            Req:ok([{"Content-Type", "application/json"}], Value)
    end.

handle('GET', ["ad", RespType], Args, Req, Conn) ->
    case RespType of
        "json" -> handle_adjson(Args, Req, Conn);
        _ -> Req:respond(400)
    end;


%% Handle GET requests with URI type of '/report/**'
%% Respone body type: application/json.
handle('GET', ["report", RespType], Args, Req, Conn) ->
    case RespType of
        "campaign" -> prepare_report(Args, Req, Conn);
        _ -> Req:respond(400)
    end;


%% Handle GET requests with URI type of '/stat/**'
%% Respone body type: [plain/text].
handle('GET', ["stat", RespType], Args, Req, Conn) ->
    case RespType of
        "clicks" -> calc_stat(1, Args, Req, Conn);
        "downloads" -> calc_stat(2, Args, Req, Conn);
        "impressions" -> calc_stat(3, Args, Req, Conn);
        _ -> Req:respond(400)
    end;


%% Handle GET requests by input Url
%% Respone body types: [plain/html].
handle('GET', Url, _, Req, Conn) ->
    {ok, Folder} = application:get_env(http_folder),
    File = Folder ++ "/" ++ Url,
    {ok, FileContent} = ads_data:get(Url, Conn),
    if
        undefined == FileContent->
            case filelib:is_file(File) of
                true -> 
                    {ok, Bin} = file:read_file(File),
                    ads_data:put(Url, binary_to_list(Bin), Conn),
                    Req:file(File);
                false -> 
                    Req:respond(404)
            end;
        true ->
            Req:ok([{"Content-Type", "text/html"}], FileContent) 
    end;


%% Handle any other requests
handle(_, _, _, Req, _) ->
    Req:respond(404).
  
