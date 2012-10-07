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
    {ok, StatUrls} = application:get_env(stat_urls),
    {ok, ReportUrls} = application:get_env(report_urls),
    {ok, ConfigUrl} = application:get_env(config_url),
    {ok, HomeUrl} = application:get_env(home_url),
    {ok, UploadUrl} = application:get_env(upload_url),
    StatIndex = ads_util:index_of(Uri, StatUrls),
    ReportIndex = ads_util:index_of(Uri, ReportUrls),
    if 
        StatIndex > 0 ->
            calc_stat(StatIndex, Args, Req, Conn);
        ReportIndex > 0 ->
            prepare_report(Args, Req, Conn);
        ConfigUrl == Uri ->
            prepare_config(Args, Req, Conn);
        HomeUrl == Uri ->
            handle_home(Req);
        [] == Uri ->
            handle_home(Req);
        UploadUrl == Uri ->
            handle_upload(Method, Args, Req);
        true ->
            handle(Uri, Req, Conn)        
    end.

calc_stat(IncNumber, Args, Req, Conn) ->
    {ok, StatKeys} = application:get_env(stat_keys),
    ValidateResult = ads_util:validate(Args, StatKeys), 
    if 
        false == ValidateResult -> Req:respond(400); 
        true ->
            ads_data:set_stat(IncNumber, ads_util:genkey(Args), Conn),
            Req:respond(200)
    end.

prepare_report(Args, Req, Conn) ->
    {ok, StatKeys} = application:get_env(stat_keys),
    ValidateResult = ads_util:validate(Args, StatKeys), 
    if 
        false == ValidateResult -> Req:respond(400); 
        true ->
            Key = ads_util:genkey(Args),
            Value = ads_data:get_stat(Key, Conn),
            JSONTemplate = "{\"~s\":\"~p\"}",
            JSON = io_lib:format(JSONTemplate, [Key, Value]),
            Req:ok([{"Content-Type", "application/json"}], JSON)
    end.

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

save_file(FilePath, Url, Req, Conn) ->
    {ok, BinaryFileContent} = file:read_file(FilePath),
    FileModifiedTime = io_lib:format("~p", [ads_util:get_mtime(FilePath)]),
    FileContent = binary_to_list(BinaryFileContent),
    ads_data:put(Url, string:join([FileModifiedTime, FileContent], ?HTML_SEPARATOR), Conn),
    Req:file(FilePath).

prepare_config(Args, Req, Conn) ->
    {ok, ConfigKeys} = application:get_env(config_keys),
    ValidateResult = ads_util:validate(Args, ConfigKeys), 
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

build_hrefs([], _, _) -> "";
build_hrefs([H | T], Keys, Def) ->
    build_href(H, Keys, Def) ++ build_hrefs(T, Keys, Def).
    
build_href(Url, Keys, Def) ->
    Href = "/" ++ 
        string:join(Url, "/") ++ "?" ++ 
        string:join(Keys, "=" ++ Def ++ "&") ++ "=" ++ Def,
    "<li><a href=\""++ Href ++ "\">" ++ Href ++ "</a></li>".
     
handle_home(Req) ->
    {ok, StatUrls} = application:get_env(stat_urls),
    {ok, StatKeys} = application:get_env(stat_keys),
    {ok, ReportUrls} = application:get_env(report_urls),
    {ok, ConfigUrl} = application:get_env(config_url),
    {ok, ConfigKeys} = application:get_env(config_keys),
    {ok, HomeUrl} = application:get_env(home_url),
    {ok, UploadUrl} = application:get_env(upload_url),
    StatDef = io_lib:format("~p", [random:uniform(51) + 24]),
    ConfigDef = io_lib:format("~p", [random:uniform(75) + 51]),
    ReportHref = build_hrefs(ReportUrls, StatKeys, StatDef),
    StatHref = build_hrefs(StatUrls, StatKeys, StatDef),
    ConfigHref = "/" ++ string:join(ConfigUrl, "/") ++ "?" ++ string:join(ConfigKeys, "="++ConfigDef++"&") ++ "="++ ConfigDef,
    HomeHref = "/" ++ string:join(HomeUrl, "/"),
    UploadHref = "/" ++ string:join(UploadUrl, "/"),
    Req:ok([{"Content-Type", "text/html"}], 
["<html><head><title>Home</title></head>
    <body>
        <ul>
            ", ReportHref,"
            ", StatHref,"
            <li><a href=\"", ConfigHref, "\">", ConfigHref, "</a</li>
            <li><a href=\"", HomeHref, "\">", HomeHref, "</a</li>
            <li><a href=\"", UploadHref, "\">", UploadHref, "</a</li>
        </ul>
    </body>
</html>"]).

handle_upload(Method, Args, Req) ->
    if 
        'GET' == Method ->
            Req:ok([{"Content-Type", "text/html"}], 
                ["<html><head><title>File Upload</title></head><body>
                  <form action=\"/upload\" method=\"POST\" enctype=\"multipart/form-data\">
                  <input type=\"file\" name=\"file\">
                  <input type=\"submit\">
                  </form></body></html>"]);
        true ->
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
        end
    end.

handle(Url, Req, Conn) ->
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
    end.

