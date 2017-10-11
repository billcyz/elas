%% @author billcyz
%% @doc @todo Add description to elas_parser.


-module(elas_parser).
-behaviour(gen_server).
-export([init/1]).

-export([start_link/0]).

-record(state, {}).

-define(SERVER, elas_parser).

%% -----------------------------------------------------------------

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
	gen_server:call(?SERVER, init_test),
	{ok, #state{}}.

handle_call(init_test, _From, State) ->
	R = parse_example(),
	{reply, R, State}.


%% Check dataset file types (plaintext, json, xml, .....) (file extension)
check_dataset_types(File) -> 1.


%% Parse input dataset (file)
get_input(File) ->
	{ok, IoD} = file:open(File, [read]),
	1.

%% Parse user request url
parse_url() ->
	1.

%% Parse data sample
parse_example() ->
	1.




