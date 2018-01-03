%% @author billcyz
%% @doc @todo Add description to elas_http.

%% HTTP module for ELAS
%% Manage http request

-module(elas_http).
-behaviour(gen_server).

-export([init/1, handle_info/2, handle_call/3, handle_cast/2, code_change/3, terminate/2]).

-export([start_link/1,
		 is_action/1]).

-export([code_to_code_description/1]).

-record(state, {socket}).

%% -include("elas_include.hrl").

%% -----------------------------------------------------------------

%% Start http server to parse http request
start_link(Socket) ->
	gen_server:start_link(?MODULE, [Socket], []).

init([Socket]) ->
	io:format("Get socket ~p~n", [Socket]),
	gen_server:cast(self(), start),
	{ok, #state{socket = Socket}}.

handle_info({http, S, Data}, State) ->
	elas_http_sup:start_socket(),
	io:format("~p got message: ~p~n", [self(), Data]),
	A = parse_data(Data),
	send_response(list_to_binary(A), S),
	exit(self(), normal),
	{noreply, State};
handle_info({tcp_error, S, Reason}, NewState) ->
	io:format("Socket ~p has error ~p~n", [S, Reason]),
	{noreply, NewState};
handle_info({tcp_closed, _S}, NewState) ->
	{stop, normal, NewState}.

handle_call(_Request, _From, State) ->
	{noreply, State}.

handle_cast(start, S = #state{socket = Socket}) ->
	io:format("Initialize Start~n"),
	{ok, ASocket} = gen_tcp:accept(Socket),
	%elas_http_sup:start_socket(), %% start a new worker
	inet:setopts(ASocket, [{active, once}]),
	{noreply, S#state{socket = ASocket}}.

terminate(_Reason, _State) ->
	ok.

code_change(_Old, State, _Extra) ->
	{ok, State}.

%% Send response message
%% -spec send_response(list(), socket()) -> 'ok'.
send_response(Msg, Conn) when is_binary(Msg) ->
	ResponseMsg = iolist_to_binary(io_lib:fwrite("HTTP/1.1 200 OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
									 [size(Msg), Msg])),
	gen_tcp:send(Conn, ResponseMsg),
	gen_tcp:close(Conn).

%% Parse data test
parse_data(Data) when is_tuple(Data) ->
	{http_request, RequestType, _RequestPath, _} = Data,
	case RequestType of
		'GET' -> "{http_request, get_request}";
		'PUT' -> "{http_request, put_request}"
	end.

%%
%% Parse incoming data
-spec parse_input(binary()) -> any().
parse_input(Data) ->
	case erlang:decode_packet(http_bin, Data, [{packet_size, 0}]) of
		{ok, Packet, _Rest} ->
			{_, Action, _RequestPath, _} = Packet,
			case Action of
				'GET' -> "{http_request, get}";
				'PUT' -> "{http_request, put}"
			end;
		E -> E
	end.

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

%% Status code
-spec code_to_code_description(integer()) -> binary().
code_to_code_description(100) -> "100 Continue";
code_to_code_description(101) -> "101 Switching Protocols";
code_to_code_description(102) -> "102 Processing";
code_to_code_description(200) -> "200 OK";
code_to_code_description(201) -> "201 Created";
code_to_code_description(202) -> "202 Accepted";
code_to_code_description(203) -> "203 Non-Authoritative Information";
code_to_code_description(204) -> "204 No Content";
code_to_code_description(205) -> "205 Reset COntent";
code_to_code_description(206) -> "206 Partial Content";
code_to_code_description(207) -> "207 Multi-Status";
code_to_code_description(300) -> "300 Multiple Choices";
code_to_code_description(301) -> "301 Moved Permanently";
code_to_code_description(302) -> "302 Found";
code_to_code_description(303) -> "303 See Other";
code_to_code_description(304) -> "304 Not Modified";
code_to_code_description(305) -> "305 Use Proxy";
code_to_code_description(307) -> "307 Temporary Redirect";
code_to_code_description(400) -> "400 Bad Request";
code_to_code_description(401) -> "401 Unauthorized";
code_to_code_description(402) -> "402 Payment Required";
code_to_code_description(403) -> "403 Forbidden";
code_to_code_description(404) -> "404 Not Found";
code_to_code_description(405) -> "405 Method Not Allowed";
code_to_code_description(406) -> "406 Not Acceptable";
code_to_code_description(407) -> "407 Proxy Authentication Required";
code_to_code_description(408) -> "408 Request Time-out";
code_to_code_description(409) -> "409 Conflict";
code_to_code_description(410) -> "410 Gone";
code_to_code_description(411) -> "411 Length Required";
code_to_code_description(412) -> "412 Precondition Failed";
code_to_code_description(413) -> "413 Request Entity Too Large";
code_to_code_description(414) -> "414 Request-URI Too Large";
code_to_code_description(415) -> "415 Unsupported Media Type";
code_to_code_description(416) -> "416 Requested range not satisfiable";
code_to_code_description(417) -> "417 Expectation Failed";
code_to_code_description(421) ->
    "421 There are too many connections from your internet address";
code_to_code_description(422) -> "422 Unprocessable Entity";
code_to_code_description(423) -> "423 Locked";
code_to_code_description(424) -> "424 Failed Dependency";
code_to_code_description(425) -> "425 Unordered Collection";
code_to_code_description(426) -> "426 Upgrade Required";
code_to_code_description(449) -> "449 Retry With";
code_to_code_description(500) -> "500 Internal Server Error";
code_to_code_description(501) -> "501 Not Implemented";
code_to_code_description(502) -> "502 Bad Gateway";
code_to_code_description(503) -> "503 Service Unavailable";
code_to_code_description(504) -> "504 Gateway Time-out";
code_to_code_description(505) -> "505 HTTP Version not supported";
code_to_code_description(506) -> "506 Variant Also Negotiates";
code_to_code_description(507) -> "507 Insufficient Storage";
code_to_code_description(509) -> "509 Bandwidth Limit Exceeded";
code_to_code_description(510) -> "510 Not Extended";
code_to_code_description(Code) -> Code.





