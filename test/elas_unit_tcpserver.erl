%% @author billcyz
%% @doc @todo Add description to elas_unit_tcpserver.


-module(elas_unit_tcpserver).
-behaviour(gen_server).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-export([start_link/1]).

-record(state, {socket, accept_socket}).

-define(SERVER, tcp_srv).

%% ------------------------------------------------------------

start_link(Socket) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Socket], []).

init([Socket]) ->
	io:format("Get socket ~p~n", [Socket]),
	{ok, ASocket} = gen_tcp:accept(Socket),
	{ok, #state{socket = Socket, accept_socket = ASocket}}.

handle_info({tcp, _Socket, _Data}, 
			S = #state{socket = Socket, accept_socket = ASocket}) ->
	inet:setopts(ASocket, [{active, once}]),
	{ok, Packet} = gen_tcp:recv(ASocket, 0),
	io:format("Received packet is ~p~n", [Packet]),
	{noreply, S};
handle_info({error, closed}, #state{socket = Socket} = NewState) ->
	io:format("Listening socket ~p closed ~n", [Socket]),
	{noreply, NewState};
handle_info({error, timeout}, #state{socket = Socket} = NewState) ->
	io:format("Exceeded connection timeout~n"),
	{noreply, NewState};
handle_info({error, system_limit}) ->
	2.

handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

terminate(_Reason, #state{accept_socket = Socket}) ->
	gen_tcp:close(Socket),
	ok.

code_change(_Old, State, _Extra) ->
	{ok, State}.



