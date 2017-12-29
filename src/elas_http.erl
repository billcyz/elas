%% @author billcyz
%% @doc @todo Add description to elas_http.

%% HTTP module for ELAS
%% Manage http request

-module(elas_http).
-behaviour(gen_server).

-export([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-export([start_link/1, add_resource_path/1,
		 is_action/1]).

-record(state, {socket}).

-include("elas_include.hrl").

%% -----------------------------------------------------------------

%% Start http server to parse http request
start_link(Socket) ->
	gen_server:start_link(?MODULE, [Socket], []).

init([Socket]) ->
	io:format("Get socket ~p~n", [Socket]),
	gen_server:cast(self(), start),
	{ok, #state{socket = Socket}}.

handle_info({tcp, _S, Data}, NewState = #state{socket = S}) ->
	io:format("Received data ~p~n", [Data]),
	send_response("Hello", S),
	parse_input(Data),
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

handle_cast(start, S = #state{socket = Socket}) ->
	{ok, ASocket} = gen_tcp:accept(Socket),
	elas_http_sup:start_socket(), %% start a new worker
	inet:setopts(ASocket, [{active, once}]),
	send_msg("accept socket established~n", ASocket),
	{noreply, S#state{socket = ASocket}}.

terminate(_Reason, _State) ->
	ok.

code_change(_Old, State, _Extra) ->
	{ok, State}.

%% Send message back
-spec send_msg(binary(), atom()) -> 'ok'.
send_msg(Msg, S) ->
	gen_tcp:send(S, Msg).

%% Send response message
-spec send_response(list(), socket()) -> 'ok'.
send_response(Msg, Conn) ->
  B = iolist_to_binary(Msg),
  ResponseMsg = iolist_to_binary(io_lib:fwrite(
    "HTTP/1.0 200 OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
    [size(B), B]
  )),
  gen_tcp:send(Conn, ResponseMsg),
  gen_tcp:close(Conn),
  ok.

%% Parse incoming data
-spec parse_input(binary()) -> any().
parse_input(Data) ->
	case erlang:decode_packet(http_bin, Data, [{packet_size, 0}]) of
    {ok, Packet, _Rest} ->
      io:format("Received data is ~p~n", [binary_to_term(Packet)]);
    E -> io:format("Get message ~p~n", [E])
  end.


%% Store process
%% Add resource path
%% **** string, binary, json
-spec add_resource_path(list()) -> 'ok'.
add_resource_path(Path) ->
	io:format("Final resource path is: ~p~n", [?SERVER_URL ++ Path]),
	elas_meman:store_resource_path(Path).

%% Http action collection
-spec is_action(list()) -> atom() | false.
is_action(Action) ->
	case string:to_upper(Action) of
		"GET" -> get;
		"POST" -> post;
		"UPDATE" -> update;
		"PUT" -> put;
		"DELETE" -> delete;
		"HEAD" -> head;
		_ -> false
	end.


