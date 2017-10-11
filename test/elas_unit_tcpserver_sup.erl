%% @author billcyz
%% @doc @todo Add description to elas_unit_tcpserver_sup.


-module(elas_unit_tcpserver_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start_link/1,
		 start_socket/0]).

-define(SERVER, tcp_sup).

%% ------------------------------------------------------------------

%% start server
start_link(Port) ->
	case is_integer(Port) of
		true -> supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]);
		false -> io:format("Wrong type of port number ~p~n", [Port])
	end.

init([Port]) ->
	{ok, Socket} = gen_tcp:listen(Port, [binary, {active, false}]),
	spawn_link(fun init_listener/0),
	{ok, {{simple_one_for_one, 60, 3600}, 
		  [{tcp_srv, {elas_unit_tcpserver, start_link, [Socket]},
			temporary, 1000, worker, [elas_unit_tcpserver]}]}}.

start_socket() ->
	supervisor:start_child(?SERVER, []).

init_listener() ->
	[start_socket() || _ <- lists:seq(1, 5)],
	ok.

