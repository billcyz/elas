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

-export([]).

%% -----------------------------------------------------------------

%% Start server
start(Port) when is_port(Port) ->
	ok.
	
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


