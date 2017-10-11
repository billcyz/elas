%% @author billcyz
%% @doc @todo Add description to elas_debug.


-module(elas_debug).

-export([debug/1]).


%% -------------------------------------------------------------

-spec debug(any()) -> 'ok'.
debug(Msg) ->
	io:format("Message is: ~p~n", [Msg]).
