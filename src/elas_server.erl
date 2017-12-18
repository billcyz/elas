%% @author billcyz
%% @doc @todo Add description to elas_server.


%% Main server of ELAS
%% Standalone server, which is able to run at anytime, anywhere without any dependency

-module(elas_server).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([start_link/0, start_link/1,
		 add_project/2]).



-record(state, {project}).

%% -----------------------------------------------------------------

%% Start server
-spec start_link() -> 'ok'.
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec start_link(atom()) -> 'ok'.
start_link(ServiceName) ->
	gen_server:start_link({local, ServiceName}, ?MODULE, [], []).

init([]) ->
	{ok, #state{project = none}}.


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
	end.


