%% @author billcyz
%% @doc @todo Add description to elas_server.


%% Main server of ELAS
%% Standalone server, which is able to run at anytime, anywhere without any dependency

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



