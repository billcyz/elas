%% @author billcyz
%% @doc @todo Add description to elas_server.


%% Main server of ELAS
%% Standalone server, which is able to run at anytime, anywhere without any dependency

-module(elas_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0, start_link/1,
		 add_project/2, add_project_url/2]).

-record(state, {service, service_port}).

%% (bag) project -> Project
%% (bag) project_url -> {project, [{url_01}, {url_02}]}
%% (bag) url_content -> {project, [{url_01}, {url_01 content}]}
-define(PROJECT_ETS, [project, project_url, url_content, url_action]).


%% ------------------------------API-----------------------------------

%% Start server
-spec start_link() -> 'ok'.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec start_link(atom()) -> 'ok'.
start_link(ServiceName) ->
	gen_server:start_link({local, ServiceName}, ?MODULE, [], []).

%% Add project with port
-spec add_project(atom(), integer()) -> 'ok'.
add_project(Project, Port) ->
	gen_server:call(?MODULE, {add_project, [Project, Port]}).

%% Delete project
-spec delete_project(atom(), list()) -> 'ok'.
delete_project(Project, Option) ->
	gen_server:call(?MODULE, {delete_project, [Project, Option]}).

%% Clean all project
-spec clean_all_project() -> 'ok'.
clean_all_project() ->
	gen_server:cast(?MODULE, clean_all_project),
	1.

%% Add project url
-spec add_project_url(atom(), list()) -> 'ok'.
add_project_url(Project, Url) ->
	gen_server:call(?MODULE, {add_project_url, [Project, Url]}).

%% Import response
-spec import_response(atom(), list(), atom(), list()) -> 'ok'.
import_response(Project, Uri, ResType, ResSrc) ->
	gen_server:call(?MODULE, {import_response, [Project, Uri, ResType, ResSrc]}),
	1.

%% Add local component (parser, user_script, etc)
-spec add_component(atom(), list()) -> 'ok'.
add_component(CompName, MFA) ->
  [Module, Func, Args] = MFA,
  case elas_sup:start_child_sup(
    CompName, {Module, Func, Args}) of %% Add child supervisor
    {ok, _Pid} -> ok;
    E -> E
  end.

%% Add remote component..
add_remoteComponent(NodeName, CompName, MFA) ->
  [Module, Func, Args] = MFA,
  case elas:try_connect(NodeName) of
    ok ->
      elas_sup:start_child_sup(
        CompName, {Module, Func, Args});
    E -> E
  end.

%% Add response from command line interface.
%% User provide the type of response, and elas
%% will load different parsers to parse response.
%% Clarify response type and response file source.
-spec add_response(atom(), atom()) -> 'ok'.
add_response(HttpUri, ResType, ResSrc) ->
  case filelib:is_file(ResSrc) of
    true ->
      %% check uri format
      case elas_parser:parse_uri(HttpUri) of
        true ->
          %% add uri record in data manager
          case elas_meman:register_uri(HttpUri) of
            ok ->
              %% parse and store response
              elas_parser:parse_response(ResType, ResSrc);
            E -> E
          end;
        false -> {error, invalid_uri}
      end;
    false -> {error, invalid_response}
  end,
  1.

init([]) ->
	prepare_tables(),
	
	{ok, #state{project = none}}.

%% Prepare all related tables when the process starts
%% project table (project), url table (project_url), url content table
%% (url_content) 
-spec prepare_tables() -> list().
prepare_tables() ->
	lists:map(
	  fun(ETS) -> ets:new(ETS, [named_table, bag]) end, ?PROJECT_ETS).

%% Add project
handle_call({add_project, [Project, Port]}, _From,
    S = #state{project = _Project}) ->
  Reply = case ets:lookup(ets_project, Project) of
            [] -> ets:insert_new(ets_project, {Project, Port});
            _ -> project_exists
          end,
  {reply, Reply, S#state{project = Project}};
%% Delete project
handle_call({delete_project, [Project, Option]}, _From,
    S = #state{project = _Project}) ->
  case Option of
    all ->
      clean_all_project(),
      Reply = {ok, all_project_cleaned},
      {reply, Reply, S#state{project = none}};
    [branch, BranchName] ->
      clean_project(Project, BranchName),
      Reply = {ok, project_branch_cleaned},
      {reply, Reply, S#state{project = Project}}
  end;
%% Add project url path
handle_call({add_project_url, [Project, Url]}, _From, State) ->
	P = find_project(Project),
	if
		Project =:= P ->
			case elas_meman:store_resource_path(Project, Url) of
				ok -> {reply, url_added, State};
				E -> {reply, E, State}
			end;
		true -> {error, project_not_exist}
	end;
%% Add project response
handle_call({import_response, [Project, Uri, ResType, ResSrc]},
    _From, S = #state{project = _Project}) ->
  add_response(),
  1.


handle_cast(clean_all_project, State) ->
  elas_meman:delete_table(all),
  {noreply, State}.

%% Check project and prject port duplication
%% true -> project and port is available to use
%% false -> project and port are not available to use
-spec check_project_port(atom(), integer()) -> true | false.
check_project_port(Project, Port) ->
	1.

%% check if project exist
-spec find_project(atom()) -> atom() | false.
find_project(Project) -> gen_server:call(elas_meman, {check_project, Project}).

%% Get project port
-spec get_service_port() -> integer().
get_service_port() -> #state.service_port.

