%% @author billcyz
%% @doc @todo Add description to elas_unit_tcpserver_sup.


-module(elas_unit_tcpserver_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start_link/1]).

-define(SERVER, tcp_sup).

%% ------------------------------------------------------------------

%% start server
start_link(Port) ->
	case is_integer(Port) of
		true -> supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]);
		false -> io:format("Wrong type of port number ~p~n", [Port])
	end.

init([Port]) ->
	{ok, Socket} = gen_tcp:listen(Port, [binary, {active, true}]),
	{ok, {{one_for_one, 0, 60}, 
		  [{tcp_srv, {elas_unit_tcpserver, start_link, [Socket]},
			permanent, 1000, worker, [elas_unit_tcpserver]}]}}.

