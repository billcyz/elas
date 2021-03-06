%% @author billcyz
%% @doc @todo Add description to elas_project.



%% Provide ETS related interfaces to other process
%% Send request to ETS owner

-module(elas_project).
-behaviour(gen_server).
-export([check_project_info/1, check_project_info/2,
		 create_new_project/1, create_new_project/2,
		 delete_project/1, delete_project/2,
		 define_project_url/2, define_project_url/3,
		 add_http_action/3, add_http_action/4,
		 add_url_content/4, add_url_content/5]).

-record(project_status, {project = "",
						 status = ""}).

-include("ets_config.hrl").

%% --------------------------------------------------------

start_link() ->
	gen_server:start_link(?MODULE, ?MODULE, [], []).

init([]) ->
	ets:new(?PROJECT_STATUS_TABLE, [named_table, bag]),
	{ok, ?PROJECT_STATUS_TABLE}.

%% Check project basic info
-spec check_project_info(atom()) -> true | false.
check_project_info(Project) ->
	check_project_info(node(), Project).

check_project_info(Node, Project) ->
	gen_server:call({elas_meman, Node}, {check_project, Project}).

%% Create new project
create_new_project(Project) ->
	create_new_project(node(), Project).

create_new_project(Node, Project) ->
	gen_server:call({elas_meman, Node}, {create_project, Project}).

%% Delete project
delete_project(Project) ->
	delete_project(node(), Project).

delete_project(Node, Project) ->
	gen_server:cast({elas_meman, Node}, {delete_project, Project}).

%% Define project url
define_project_url(Project, Url) ->
	define_project_url(node(), Project, Url).

define_project_url(Node, Project, Url) ->
	gen_server:call({elas_meman, Node}, {define_url, [Project, Url]}).

%% Define url HTTP action (GET, POST, etc....)
add_http_action(Project, Url, Action) ->
	add_http_action(node(), Project, Url, Action).

add_http_action(Node, Project, Url, Action) ->
	gen_server:call({elas_meman, Node}, {add_url_action, [Project, Url, Action]}).

%% Add url content (response)
add_url_content(Project, Url, Action, Content) ->
	add_url_content(node(), Project, Url, Action, Content).

add_url_content(Node, Project, Url, Action, Content) ->
	gen_server:call({elas_meman, Node}, 
					{add_url_content, [Project, Url, Action, Content]}).

%% Register project, and set the project status
-spec register(atom()) -> 'ok'.
register(Project) ->
	R = #project_status{project = Project,
						status = init},
	gen_server:call(?MODULE, {register_project, R}).

%% Update status for project
-spec update_project_status(atom(), atom()) -> 'ok'.
update_project_status(Project, NewStatus) ->
	gen_server:call(?MODULE, {update_project_status, Project,
							  NewStatus}).

%% Handle status register
handle_call({register_project, R}, _From, Table) ->
	ets:insert(Table, R),
	{reply, ok, Table}.
%% Handle status update
handle_call({update_project_status, Project, Status},
			_From, Table) ->
	R = ets:lookup(Table, project_status),
	{reply, ok, Table}.

