%% @author billcyz
%% @doc @todo Add description to elas_meman.


%% Memory management server of ELAS

-module(elas_meman).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0,
		 create_table/2, delete_table/1, check_table/1, check_project_info/1,
		 store_resource_path/2,
		 find_project/1, find_project_url/2, find_url_content/2]).


%% Test export
-export([]).

-record(state, {project_sturcture}).

%% Store project port relationship into map

%% -----------------------------------------------------------------

%% Start server
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
	%% initialize project structure list
	{ok, #state{project_sturcture = []}}.

%% Check project basic info
-spec check_project_info(atom()) -> true | false.
check_project_info(Project) ->
	case ets:lookup(project, Project) of
		[_] -> true;
		[] -> false
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
%% Find project
-spec find_project(atom()) -> atom().
find_project(Project) ->
	case ets:lookup(project, Project) of
		[] -> {no_project, Project};
		_ -> {find_project, Project}
	end.

%% Find project url
-spec find_project_url(atom(), list()) -> atom() | list().
find_project_url(Project, Url) ->
	ProjectUrl = ets:lookup(project_url, Project),
	parse_project_url_list(Url, ProjectUrl).

parse_project_url_list(U, [UrlH|UrlT]) ->
	{_, {Url}} = UrlH,
	if
		Url =:= U -> {url_found, U};
		true -> parse_project_url_list(U, UrlT)
	end;
parse_project_url_list(U, []) -> {url_not_found, U}.

%% Find project url content
-spec find_url_content(atom(), list()) -> term().
find_url_content(Project, Url) ->
	ContentList = ets:lookup_element(
					url_content, Project, 2),
	parse_url_content(Url, ContentList).

parse_url_content(U, [ConH|ConT]) ->
	[{Url}, {UrlContent}] = ConH,
	if
		Url =:= U -> {url_content_found, UrlContent};
		true -> parse_url_content(U, ConT)
	end;
parse_url_content(U, []) -> {url_content_not_found, U}.

%% Store resource path for response
-spec store_resource_path(atom(), binary() | list()) -> 'ok'.
store_resource_path(Project, Path) ->
	Url = elas_parser:parse_url(Path),
	gen_server:call(?MODULE, {store_path, Project, Url}).

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
	end;
handle_call({add_url_action, {Project, Url, Action, Opt}},
			_From, State) ->
	case ets:insert(url_cation, [{Project, [Action, {Url}]}]) of
		true -> {reply, ok, State};
		E -> {reply, E, State}
	end.




