%% @author billcyz
%% @doc @todo Add description to elas_http.

%% HTTP module for ELAS
%% Manage http request

-module(elas_http).
-behaviour(gen_server).
-export([add_resource_path/1,
		 parse_resource_path/1]).

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

%% Parse incoming data
-spec parse_input(binary()) -> 'ok'.
parse_input(Data) ->
	
	1.


%% Store process
%% Add resource path
%% **** string, binary, json
-spec add_resource_path(list()) -> 'ok'.
add_resource_path(Path) ->
	io:format("Final resource path is: ~p~n", [?SERVER_URL ++ Path]),
	elas_meman:store_resource_path(Path).

%% Find resource data
-spec find_resource_data(binary() | list()) -> any().
find_resource_data(Path) ->
	ResPath = elas_parser:transfer_path(Path),
	1.
	

%% Retrieve process
%% Parse resource path and redirect traffic
%% string, binary
%% -spec parse_resource_path(list()) -> 'ok'.
%% parse_resource_path(Path) ->
%% 	%% not useful currently
%% 	string:tokens(Path, "/").