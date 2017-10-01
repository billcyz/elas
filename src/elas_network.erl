%% @author billcyz
%% @doc @todo Add description to elas_network.

%% Network communication for ELAS. Including TCP and UDP

-module(elas_network).
-behaviour(gen_server).
-export([start_link/1,
		 parse_node/2]).

-export([init/1]).

-define(SERVER, elas_networkSrv).

-record(status, {}).

%% -----------------------------------------------------------------

%% Start network server
-spec start_link(integer()) -> 'ok'.
start_link(Port) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

init([]) ->
	{ok, #status{}}.

%% TCP

%% UDP


%% Connect remote node
%% Node can run one the same server or run in different network
%% locations
%%
%% Need negotiation for node cookie before connecting
-spec connect_remoteNode(atom()) -> 'ok'.
connect_remoteNode(NodeName) ->
	RAddress = parse_node(ip, NodeName),
	initial_connect(RAddress),
	ok.

%% Parse node name for location and other related info
%% node name spec: example@192.168.1.1
-spec parse_node(atom(), atom()) -> list().
parse_node(ParseType, NodeName) ->
	R = string:tokens(atom_to_list(NodeName), "@"),
	case ParseType of
		all -> R;
		name ->
			[Name, _] = R,
			Name;
		ip ->
			[_, IP] = R,
			IP
	end.



