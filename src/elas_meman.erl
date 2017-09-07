%% @author billcyz
%% @doc @todo Add description to elas_meman.


%% Memory management server of ELAS

-module(elas_meman).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([]).


-record(state, {}).

%% -----------------------------------------------------------------

%% Start server
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #state{}}.


%% Store dataset for response
%% Dataset can be plaintext, json, xml, and csv
store_dataset(Data) ->
	gen_server:call(_, _, _).


%% Handle Behaviour
handle_call() -> 1.

