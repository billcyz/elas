%% @author billcyz
%% @doc @todo Add description to elas_network.

%% Network communication for ELAS. Including TCP and UDP

-module(elas_network).
-behaviour(gen_server).
-export([start_link/2,
		 parse_node/2]).

-export([init/1]).

-define(SERVER, elas_networkSrv).

-record(status, {}).

%% -----------------------------------------------------------------

%% Start network server
-spec start_link(integer(), integer()) -> 'ok'.
start_link(TPort, UPort) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [TPort, UPort], []).

%% Stop network server
-spec stop() -> 'ok'.
stop() ->
	terminate().

init([TPort, UPort]) ->
	
	
	{ok, #status{}}.

%% Start TCP server
-spec start_tcp_listener(integer(), list()) -> 'ok'.
start_tcp_listener(Port, Option) ->
	Socket = gen_tcp:listen(Port, []),
	ok.


%% Start UDP server
-spec start_udp_listener(integer(), list()) -> 'ok'.
start_udp_listener(Port, Option) ->
	ok.


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



