%% @author billcyz
%% @doc @todo Add description to elas_unit_tcpserver.


-module(elas_unit_tcpserver).
-behaviour(gen_server).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-export([start_link/1]).

-record(state, {socket}).

%% ------------------------------------------------------------

start_link(Socket) ->
	gen_server:start_link(?MODULE, [Socket], []).

init([Socket]) ->
	io:format("Get socket ~p~n", [Socket]),
	
	gen_server:cast(self(), accept),
	{ok, #state{socket = Socket}}.

handle_info({tcp, _S, Data}, NewState = #state{socket = S}) ->
	io:format("Received data ~p~n", [Data]),
	send_msg(Data, S),
	inet:setopts(S, [{active, once}]),
	io:format("Set socket active once again~n"),
	{noreply, NewState};
handle_info({tcp_error, S, Reason}, NewState) ->
	io:format("Socket ~p has error ~p~n", [S, Reason]),
	{noreply, NewState};
handle_info({tcp_closed, _S}, NewState) ->
	{stop, normal, NewState}.

handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast(accept, S = #state{socket = Socket}) ->
	{ok, ASocket} = gen_tcp:accept(Socket),
	elas_unit_tcpserver_sup:start_socket(), %% start a new worker
	inet:setopts(ASocket, [{active, once}]),
	send_msg("accept socket established~n", ASocket),
	{noreply, S#state{socket = ASocket}}.

terminate(_Reason, _State) ->
	ok.

code_change(_Old, State, _Extra) ->
	{ok, State}.



-spec send_msg(binary(), atom()) -> 'ok'.
send_msg(Msg, S) ->
	gen_tcp:send(S, Msg).

