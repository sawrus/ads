-module(ads_data).

%% API
-export([get/2, put/3, open/0]).


%% ===================================================================
%% Application callbacks
%% ===================================================================

get(Key, Conn)->
	Response = eredis:q(Conn, ["GET", Key]),
	Response.
	
put(Key, Value, Conn)->
	{ok, <<"OK">>} = eredis:q(Conn, ["SET", Key, Value]).
	
open()->
	Response = eredis:start_link(),
	{ok, Conn} = Response,
	Conn.