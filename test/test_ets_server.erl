%% @author billcyz
%% @doc @todo Add description to test_ets_server.


-module(test_ets_server).

-export([start/0, loop/0]).

%% ---------------------------------------------------------

start() ->
	Pid = spawn(?MODULE, loop, []),
	register(ets_server, Pid),
	case whereis(ets_server) of
		undefined -> false;
		_Pid ->
			ets:new(test_ets, [named_table]),			
			{ok, ets_server}
	end.

loop() ->
	receive
		{From, {add, Value}} ->
			io:format("Received Add Request~n"),
			R = ets:insert(test_ets, {Value}),
			From ! R,
			loop();
		{From, {check, Value}} ->
			io:format("Received Check Request~n"),
			R = ets:lookup(test_ets, Value),
			From ! R,
			loop();
		{From, {delete, Value}} ->
			io:format("Received Delete Request~n"),
			R = ets:delete(test_ets, Value),
			From ! R,
			loop()
	end.




