%% @author billcyz
%% @doc @todo Add description to elas_http_sup.


-module(elas_http_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start_link/0, start_link/1,
		 start_socket/0]).

-include("elas_include.hrl").

%% -----------------------------------------------------------------

%% Start http server supervisor
-spec start_link() -> 'ok'.
start_link() ->
	start_link(8080).

start_link(Port) ->
	case is_integer(Port) of
		true -> supervisor:start_link({local, ?MODULE}, ?MODULE, [Port]);
		false -> io:format("Wrong type of port number ~p~n", [Port])
	end.

init([Port]) ->
	{ok, Socket} = gen_tcp:listen(Port, [binary, {active, false},
										 {packet, http_bin}]),
	spawn_link(fun init_listener/0),
	{ok, {{simple_one_for_one, 60, 3600}, 
		  [{http_server, {elas_http, start_link, [Socket]},
			temporary, 1000, worker, [elas_http]}]}}.

start_socket() ->
	supervisor:start_child(?MODULE, []).

init_listener() ->
	[start_socket() || _ <- lists:seq(1, 5)],
	ok.





