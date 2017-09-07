%% @author billcyz
%% @doc @todo Add description to elas.


%% Erlang lightweight API server (ELAS)

-module(elas).

-export([]).

%% -----------------------------------------------------------------

%% Start server
start(Port) when is_port(Port) ->
	
	
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
