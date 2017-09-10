%% @author billcyz
%% @doc @todo Add description to elas_respond.


-module(elas_respond).
-behaviour(gen_server). %% can be event_handler
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-export([]).

-define(SERVER, ?MODULE).

-record(state, {clientSocket, httpAction,
				respondStatus}).

%% -----------------------------------------------------------------

%% Start respond server
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.

%% Respond user url request
respond_request(Url, HttpAction, Option) ->
	gen_server:call(?SERVER, , _).

%% Reply raw dataset to user
respond_raw() -> 1.


handle_call({respond_call, Url, HttpAction, Option}, Socket, State) ->
	ParseResult = elas_parse:parse_url(Url),
	case Option of
		raw -> respond_raw();
		_ ->
			1
	end,
	{ok, State#state{clientSocket = Socket, httpAction = HttpAction}}.
	

