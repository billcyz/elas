%% @author billcyz
%% @doc @todo Add description to elas_sup.


-module(elas_sup).
-behaviour(supervisor).
-export([init/1]).


-export([]).

%% -----------------------------------------------------------------

%% Start supervisor
stat_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, [{one_for_one, 0, 5}]}.



%% socket supervisor (optional) -> for remote connection

%% request parser supervisor

%% request responser supervisor

%% memory management supervisor

