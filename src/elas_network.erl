%% @author billcyz
%% @doc @todo Add description to elas_network.

%% Network communication for ELAS. Including TCP and UDP

-module(elas_network).

-export([]).

%% -----------------------------------------------------------------




%% Connect remote node
%% Node can run one the same server or run in different network
%% locations
%%
%% Need negotiation for node cookie before connecting
-spec connect_remoteNode(atom()) -> 'ok'.
connect_remoteNode(NodeName) ->
	ok.

