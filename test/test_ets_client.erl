%% @author billcyz
%% @doc @todo Add description to test_ets_client.


-module(test_ets_client).

-export([start/0, loop/0,
		 add/1, check/1, delete/1]).

%% -------------------------------------------------

start() ->
	register(?MODULE, spawn(?MODULE, loop, [])).

add(Value) ->
	ets_server ! {add, Value}.

check(Value) ->
	ets_server ! {check, Value}.

delete(Value) ->
	ets_server ! {delete, Value}.

loop() ->
	receive
		{_From, Result} -> 
			Result,
			loop()
	end.