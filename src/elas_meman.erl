%% @author billcyz
%% @doc @todo Add description to elas_meman.


%% Memory management server of ELAS

-module(elas_meman).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0,
  create_table/2, delete_table/1, check_table/1,
  store_resource_path/1]).


-record(state, {project_sturcture}).

%% -----------------------------------------------------------------

%% Start server
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	%% initialize project structure list
  {ok, #state{project_sturcture = []}}.

%% Create ets table
-spec create_table(atom(), list()) -> 'ok'.
create_table(Tab, TabOption) ->
  case check_table(Tab) of
    undefined -> ets:new(Tab, TabOption);
    _ -> {Tab, table_already_exists}
  end.

%% Delete ets table
-spec delete_table(atom()) -> 'ok'.
delete_table(Tab) ->
  case Tab of
    all -> 1;
    _ when is_atom(Tab) -> 2
  end,
  case check_table(Tab) of
    undefined -> {Tab, table_undefined};
    _ ->
      case ets:delete(Tab) of
        true -> ok;
        E -> E
      end
  end.

%% Check ets table info
-spec check_table(atom()) -> list().
check_table(Tab) ->
  case ets:info(Tab) of
    undefined -> undefined;
    R -> R
  end.

%% Store resource path for response
-spec store_resource_path(binary() | list()) -> 'ok'.
store_resource_path(Path) ->
  gen_server:cast(?MODULE, {store_path, Path}).

%% Store dataset for response
%% Dataset can be plaintext, json, xml, and csv
store_dataset(Data) ->
  gen_server:call(_, _, _).


%% Handle Behaviour
handle_call() -> 1.

handle_cast({store_path, Path}, S = #state{}) ->
  ResPath = case is_binary(Path) of
              true -> binary_to_list(Path);
              false -> Path
            end,
  ets:insert(ets_path, ResPath),
  {noreply, S}.




