%% @author billcyz
%% @doc @todo Add description to elas_meman.


%% Memory management server of ELAS

-module(elas_meman).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0,
		 create_table/2, delete_table/1, check_table/1, check_project_info/1,
		 store_resource_path/2]).


%% Test export
-export([]).

-record(state, {project_sturcture}).

-define(PROJECT_ETS, [project, project_url, url_content]).

%% Store project port relationship into map

%% -----------------------------------------------------------------

%% Start server
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	prepare_tables(),
	%% initialize project structure list
	{ok, #state{project_sturcture = []}}.

%% Prepare all related tables when the process starts
%% project table (project), url table (project_url), url content table
%% (url_content) 
-spec prepare_tables() -> list().
prepare_tables() ->
	lists:map(
	  fun(ETS) -> ets:new(ETS, [named_table]) end, ?PROJECT_ETS).

%% Check project basic info
-spec check_project_info(atom()) -> true | false.
check_project_info(Project) ->
	case ets:lookup(_, _) of
		_ -> true;
		E -> false
	end.

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

%% Find required info in ets table





%% Store resource path for response
-spec store_resource_path(atom(), binary() | list()) -> 'ok'.
store_resource_path(Project, Path) ->
	Url = elas_parser:parse_url(Path),
	gen_server:call(?MODULE, {store_path, Project, Url}).

%% Store dataset for response
%% Dataset can be plaintext, json, xml, and csv
store_dataset(Data) ->
	gen_server:call(_, _, _).


%% Handle Behaviour
%% Check project exist
handle_call({check_project, Project}, _From, State) ->
	case check_project_info(Project) of
		true -> {reply, Project, State};
		false -> {reply, false, State}
	end;
%% Store url path to ets
handle_call({store_path, Project, Path}, _From, S = #state{}) ->
	case gen_server:call(?MODULE, {{check_project, Project}}) of
		false -> {reply, invalid_project, S};
		_ -> 
			case ets:insert(ets_path, {Project, Path}) of
				true -> {reply, ok, S};
				E -> {reply, E, S}
			end
	end.




