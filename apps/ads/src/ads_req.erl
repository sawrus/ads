%%
%% ADS - Handle request module
%%
-module(ads_req).
-vsn("0.1").

% includes
-include("../include/ads.hrl").
-include_lib("kernel/include/file.hrl").

% API
-export([handle/2]).

%% @doc Handle HTTP request callbacks
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


%%% Respone body types: [application/json].

%% @doc Calculate statistics by request parameters and IncNumber
calc_stat(IncNumber, Args, Req, Conn) ->
    ValidateResult = ads_util:validate(Args, ?ADSTAT), 
    if 
        false == ValidateResult -> Req:respond(400); 
        true ->
            ads_data:set_stat(IncNumber, ads_util:genkey(Args), Conn),
            Req:respond(200)
    end.

%% @doc Prepare statistics report via GET parameters of input request
prepare_report(Args, Req, Conn) ->
    ValidateResult = ads_util:validate(Args, ?ADSTAT), 
    if 
        false == ValidateResult -> Req:respond(400); 
        true ->
            Key = ads_util:genkey(Args),
            Value = ads_data:get_stat(Key, Conn),
            JSONTemplate = "{\"~s\":\"~p\"}",
            JSON = io_lib:format(JSONTemplate, [Key, Value]),
            Req:ok([{"Content-Type", "application/json"}], JSON)
    end.

%% @doc Building configurations of defined applications
prepare_config(Key) ->
    {H, M, S} = time(),
    Time  = io_lib:format('~2..0b:~2..0b:~2..0b', [H, M, S]),
    Extra = [{"Key", Key}, {"Time", Time}],
    BuildJSON = fun({Param, Value}, Acc) ->
        [lists:flatten(
            io_lib:format("{\"~s\":\"~s\"},\n", [Param, Value])) | Acc
        ]
    end,
    ExtraJson = lists:flatten(lists:reverse(lists:foldl(BuildJSON, [], Extra))),
    ExtraJson.

%% @doc Put file content in NoSQL cache
save_file(FilePath, Url, Req, Conn) ->
    {ok, BinaryFileContent} = file:read_file(FilePath),
    FileModifiedTime = io_lib:format("~p", [ads_util:get_mtime(FilePath)]),
    FileContent = binary_to_list(BinaryFileContent),
    ads_data:put(Url, string:join([FileModifiedTime, FileContent], ?HTML_SEPARATOR), Conn),
    Req:file(FilePath).

%% @doc Handle GET requests with URI type of '/ad/**'
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
            NewValue = prepare_config(Key),
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


%% @doc Handle GET requests with URI type of '/report/**'
%% Respone body type: application/json.
handle('GET', ["report", RespType], Args, Req, Conn) ->
    case RespType of
        "campaign" -> prepare_report(Args, Req, Conn);
        _ -> Req:respond(400)
    end;


%% @doc Handle GET requests with URI type of '/stat/**'
%% Respone body type: [plain/text].
handle('GET', ["stat", RespType], Args, Req, Conn) ->
    case RespType of
        "clicks" -> calc_stat(1, Args, Req, Conn);
        "downloads" -> calc_stat(2, Args, Req, Conn);
        "impressions" -> calc_stat(3, Args, Req, Conn);
        _ -> Req:respond(400)
    end;

% handle a GET on /
handle('GET', ["upload"], _, Req, _) -> Req:ok([{"Content-Type", "text/html"}], 
["<html><head><title>File Upload</title></head>
    <body>
        <form action=\"/upload\" method=\"POST\" enctype=\"multipart/form-data\">
            <input type=\"file\" name=\"file\">
            <input type=\"submit\">
        </form>
    </body>
</html>"]);

% handle a POST on / -> file received
handle('POST', ["upload"], Args, Req, _) ->
    case Args of
        [{_Tag, Attributes, FileData}] ->
            % build destination file path
            {ok, DestPath} = application:get_env(http_folder),
            FileName = misultin_utility:get_key_value("filename", Attributes),
            DestFile = DestPath ++ "/" ++ FileName,
            % save file
            case file:write_file(DestFile, FileData) of
                ok ->
                    Req:file(DestFile);
                {error, _Reason} ->
                    Req:respond(500)
            end;
        _ ->
            Req:respond(500)
    end;

%% @doc Handle / requests. 
handle(_, [], _, Req, _) ->
    Req:redirect("/upload");

%% @doc Handle GET requests by input Url
%% Respone body types: [plain/html].
handle('GET', Url, _, Req, Conn) ->
    {ok, Folder} = application:get_env(http_folder),
    FilePath = Folder ++ "/" ++ Url,
    {ok, BinaryFileContent} = ads_data:get(Url, Conn),
    FileExist = filelib:is_file(FilePath), 
    if
        false == FileExist ->
            Req:respond(404);
        undefined == BinaryFileContent->
            save_file(FilePath, Url, Req, Conn);
        true ->                            
            ActualModifiedTime = integer_to_list(ads_util:get_mtime(FilePath)),
            [CachedModifiedTime, FileContent] = string:tokens(binary_to_list(BinaryFileContent), ?HTML_SEPARATOR),
            if 
                ActualModifiedTime =/= CachedModifiedTime  ->
                    save_file(FilePath, Url, Req, Conn);
                true ->
                    Req:ok([{"Content-Type", "text/html"}], FileContent)
            end
    end;

%% @doc Handle any other requests
handle(_, _, _, Req, _) ->
    Req:respond(404).
  
