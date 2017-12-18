%% @author billcyz
%% @doc @todo Add description to elas.


%% Erlang lightweight API server (ELAS)
%% 
%% Task:
%% 1. import respond dataset (via shell command)
%% 2. create customized url 
%% 3. define http action
%% 4. parse input dataset & url
%% 5. return message / result
%%

-module(elas).

-export([start/0, start/1]).


-include("elas_include.hrl").

%% -----------------------------------------------------------------

%% Start server
start() -> start(?DEFAULT_HTTP_PORT).

start(Port) when is_integer(Port) ->
	Service = service_name(Port),
	
	case elas_server:start_link(Service) of
		{ok, _Pid} -> {ok, elas_started};
		E -> E
	end,
	
	elas_http_sup:start_link(Port).
	
%% 	case elas_server:start_link(Port) of
%% 		{ok, _Pid} -> {ok, started};
%% 		E -> E
%% 	end.


%% Stop server
stop() ->
	elas_server:stop(),
	ok.

%% %% Import dataset
%% import_dataset() ->
%% 	1.
%% 
%% %% Return url


%% Service name
-spec service_name(integer()) -> atom().
service_name(Port) -> list_to_atom("elas_" ++ integer_to_list(Port)).

%% Data storage name
-spec storage_name(integer()) -> atom().
storage_name(Port) -> list_to_atom("elas_" ++ integer_to_list(Port)
								  ++ "_storage").

%% Http service name
-spec http_name(integer()) -> atom().
http_name(Port) -> list_to_atom("elas_" ++ integer_to_list(Port)
							   ++ "_http").

%% Connect remote node or url
-spec try_connect('node' | 'url', atom()) -> any().
try_connect(Type, ConDst) when is_atom(ConDst) ->
	case Type of
		node -> 
			R = elas_network:connect_remoteNode(ConDst);
		url -> 
			R = elas_http:connect_url(ConDst);
		_ -> R = invalid_connect_type
	end,
	R.


