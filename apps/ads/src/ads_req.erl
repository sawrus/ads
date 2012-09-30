-module(ads_req).

% includes
-include("../include/ads.hrl").

% API
-export([handle/2]).

%% Build default response with 404 and other HTTP codes
page_not_found(Req) ->
    Req:ok([{"Content-Type", "text/plain"}], "Page not found.").

respond(HttpCode, Req) ->
	Req:respond(HttpCode, [{"Content-Type", "text/plain"}], 
		integer_to_list(HttpCode)).

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
    handle(Method, Uri, Args, Req, Conn).


%% Responces 
%% Respone body types: [application/json, plain/html].
calc_stat('clicks', Args, Req, Conn) ->
	ads_data:set_stat(1, ads_util:genkey(Args), Conn),
    respond(200, Req);

calc_stat('downloads', Args, Req, Conn) ->
	ads_data:set_stat(2, ads_util:genkey(Args), Conn),
    respond(200, Req);

calc_stat('impressions', Args, Req, Conn) ->
	ads_data:set_stat(3, ads_util:genkey(Args), Conn),
    respond(200, Req).

prepare_report(Args, Req, Conn) ->
	Key   = ads_util:genkey(Args),
	Value = ads_data:get_stat(Key, Conn),
	JSON  = json_eep:term_to_json({[{Key, Value}]}),
	Req:ok([{"Content-Type", "application/json"}], JSON).

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
    Key = ads_util:genkey(Args),
    {ok, Value} = ads_data:get(Key, Conn),
    if
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
        "json" 	-> handle_adjson(Args, Req, Conn);
        _ -> respond(400, Req)
    end;

		
%% Handle GET requests with URI type of '/report/**'
%% Respone body type: application/json.
handle('GET', ["report", RespType], Args, Req, Conn)->
    case RespType of
        "campaign" -> prepare_report(Args, Req, Conn);
        _ -> respond(400, Req)
    end;


%% Handle GET requests with URI type of '/stat/**'
%% Respone body type: [plain/text].
handle('GET', ["stat", RespType], Args, Req, Conn)->
    case RespType of
        "clicks" -> calc_stat('clicks', Args, Req, Conn);
        "downloads" -> calc_stat('downloads', Args, Req, Conn);
        "impressions" -> calc_stat('impressions', Args, Req, Conn);
        _ -> respond(400, Req)
    end;


%% Handle GET requests by input Url
%% Respone body types: [plain/html].
handle('GET', Url, _, Req, _) ->
    {ok,Folder} = application:get_env(http_folder),
    File = Folder ++ "/" ++ Url,
    case filelib:is_file(File) of
		true -> Req:file(File);
		false-> respond(404, Req)
    end;
	

%% Handle any other requests
handle(_, _, _, Req, _) ->
    page_not_found(Req).
  
