-module(ads_req).

% includes
-include("../include/ads.hrl").

% API
-export([handle/2]).


%% ===================================================================
%% Build default response with 404 code
%% ===================================================================
page_not_found(Req)->
  Req:ok([{"Content-Type", "text/plain"}], "Page not found.").  
 

%% ===================================================================
%% Handle HTTP request callbacks
%% ===================================================================

% Handle HTTP request
handle(Req, Conn) ->
  % get params depending on method
  Method = Req:get(method),
  case Method of
    'GET' ->
      Args = Req:parse_qs();
    'POST' ->
      Args = Req:parse_post()
  end,
  % Handle request by parameters
  handle(Req:get(method), Req:resource([lowercase, urldecode]), Args, Req, Conn).

%% ===================================================================
%% Build Responces 
%% Respone body types: [application/json, plain/html].
%% ===================================================================

build_adjson(Key)->
	{H, M, S} = time(),
	T = io_lib:format('~2..0b:~2..0b:~2..0b', [H, M, S]),
	E = [{"Extra","Parameters"}, {"Value","EVALUE"}, {"Key", Key}, {"Time", T}],
	N = [{"Networks","Parameters"}, {"Value","NVALUE"}, {"Key", Key}, {"Time", T}],
	IN = [{"Networks","Parameters"}, {"Value","INVALUE"}, {"Key", Key}, {"Time", T}],
	BuildJSON = fun({Param, Value}, Acc) ->
		[lists:flatten(io_lib:format("{\"~s\":\"~s\"},\n", [Param, Value])) | Acc]
	end,
	EJ = lists:flatten(lists:reverse(lists:foldl(BuildJSON, [], E))),
	NJ = lists:flatten(lists:reverse(lists:foldl(BuildJSON, [], N))),
	INJ = lists:flatten(lists:reverse(lists:foldl(BuildJSON, [], IN))),
	EJ ++ NJ ++ INJ.

%% ===================================================================
%% Handle GET requests with URI type of '/ad/**'
%% Respone body types: [application/json, plain/html].
%% ===================================================================

handle_adtest(Args, Req, Conn)->
  Key = ads_util:genkey(Args),
  {ok, Value} = ads_data:get(Key, Conn),
  if   
	undefined == Value ->
		NewValue = build_adjson(Key),
		ads_data:put(Key, NewValue, Conn),
		Req:ok([{"Content-Type", "application/json"}], NewValue);
	true->	
		Req:ok([{"Content-Type", "application/json"}], Value)
  end.
  
handle_adjson(Args, Req)->
  % prepare JSON
  BuildJSON = fun({Param, Value}, Acc) ->
    [lists:flatten(io_lib:format("{\"~s\":\"~s\"}", [Param, Value])) | Acc]
  end,
  JSON = lists:flatten(lists:reverse(lists:foldl(BuildJSON, [], Args))),
  % output
  Req:ok([{"Content-Type", "application/json"}], JSON).

handle_adhtml(Args, Req) ->
  % build an XML with all parameters and values
  BuildXml = fun({Param, Value}, Acc) ->
    [lists:flatten(io_lib:format("        <tr><td>Param = ~s</td></tr>
          <tr><td>Value = ~s</td></tr>", [Param, Value])) | Acc]
  end,
  Xml = lists:flatten(lists:reverse(lists:foldl(BuildXml, [], Args))),
  % output
  Req:ok([{"Content-Type", "text/html"}], "<html>
  <head>
      <title>/ad/html</title>
  </head>
  <body>
      <h1>/ad/html</h1>
      <table>
            ~s
      </table>
  </body>
  </html>", [Xml]).
    
handle('GET', ["ad", RespType], Args, Req, Conn)->
case RespType of
  "json" -> handle_adjson(Args, Req);
  "html" -> handle_adhtml(Args, Req);
  "test" -> handle_adtest(Args, Req, Conn);
  _ -> page_not_found(Req)
end;


%% ===================================================================
%% Handle GET requests with URI type of '/report/**'
%% Respone body type: application/json.
%% ===================================================================

%todo: Need to implemented cases from the list above
%handle('GET', ["report", RespType], Args, Req)->
%case RespType of
%  _ -> page_not_found(Req)
%end;


%% ===================================================================
%% Handle GET requests with URI type of '/stat/**'
%% Respone body type: undefined.
%% ===================================================================

%todo: Need to implemented cases from the list above
%handle('GET', ["stat", RespType], Args, Req)->
%case RespType of
%  _ -> page_not_found(Req)
%end;

	
%% ===================================================================
%% Handle any other requests
%% =================================================================== 

handle(_, _, _, Req, _) ->
  page_not_found(Req).
  