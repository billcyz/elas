%% @author billcyz
%% @doc @todo Add description to elas_server.


%% Main server of ELAS
%% Standalone server, which is able to run at anytime, anywhere without any dependency

-module(elas_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0,
  add_project/2]).



-record(state, {project}).

%% ------------------------------API-----------------------------------

%% Start server
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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

%% Import response
-spec import_response(atom(), list(), atom(), list()) -> 'ok'.
import_response(Project, Uri, ResType, ResSrc) ->
  gen_server:call(?MODULE, {import_response, [Project, Uri, ResType, ResSrc]}),
  1.

init([]) ->
  {ok, #state{project = []}}.


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

handle_call({add_project, [Project, Port]}, _From,
    S = #state{project = _Project}) ->
  Reply = case ets:lookup(ets_project, Project) of
            [] -> ets:insert_new(ets_project, {Project, Port});
            _ -> project_exists
          end,
  {reply, Reply, S#state{project = Project}};
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
handle_call({import_response, [Project, Uri, ResType, ResSrc]},
    _From, S = #state{project = _Project}) ->
  add_response(),
  1.

handle_cast(clean_all_project, State) ->
  elas_meman:delete_table(all),
  {noreply, State}.

