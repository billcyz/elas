%% @author billcyz
%% @doc @todo Add description to elas_unit_tcpserver.


-module(elas_unit_tcpserver).
-behaviour(gen_server).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-export([start_link/1]).

-record(state, {socket, asocket}).

-define(SERVER, tcp_srv).

%% ------------------------------------------------------------

start_link(Socket) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Socket], []).

init([Socket]) ->
	io:format("Get socket ~p~n", [Socket]),
	{ok, ASocket} = gen_tcp:accept(Socket),
	io:format("Accept socket is ~p~n", [ASocket]),
	{ok, #state{socket = Socket, asocket = ASocket}}.

%% handle_info(timeout, NewState = #state{asocket = AS}) ->
%% 	inet:setopts(AS, [{active, once}]),
%% 	io:format("Set accept socket ~p to active once~n", [AS]),
%% 	{noreply, NewState};
handle_info({tcp, S, Data}, NewState) ->
%% 	{ok, Packet} = gen_tcp:recv(ASocket, 0),
	io:format("Received packet is ~p~n", [Data]),
	send_msg(Data, S),
	{noreply, NewState};
handle_info({tcp_error, S, Reason}, NewState) ->
	io:format("Socket ~p has error ~p~n", [S, Reason]),
	{noreply, NewState};
handle_info({tcp_closed, S}, NewState) ->
	io:format("Socket ~p closed~n", [S]),
	{noreply, NewState}.

handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

terminate(_Reason, #state{socket = S}) ->
	gen_tcp:close(S),
	ok.

code_change(_Old, State, _Extra) ->
	{ok, State}.

-spec send_msg(binary(), atom()) -> 'ok'.
send_msg(Msg, S) ->
	gen_tcp:send(S, Msg).


