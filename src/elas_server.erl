%% @author billcyz
%% @doc @todo Add description to elas_server.


%% Main server of ELAS

-module(elas_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0]).



-record(state, {}).

%% -----------------------------------------------------------------

%% Start server
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	{ok, #state{}}.



