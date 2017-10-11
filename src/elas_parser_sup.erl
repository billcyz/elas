%% @author billcyz
%% @doc @todo Add description to elas_parser_sup.


%% Split file into pieces, and assign each
%% piece to each parsing supervisor 

-module(elas_parser_sup).
-behaviour(supervisor).
-export([init/1]).

-export([start_link/0]).

-define(SERVER, elas_parser_sup).

%% -----------------------------------------------------------------

%% Start supervisor
%% -spec start_link(integer()) -> any().
%% start_link(Num) ->
%% 	supervisor:start_link({local, ?SERVER}, ?MODULE, [Num]).

%% Start beta supervisor
start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	{ok, {{one_for_all, 0, 60},
		  [{elas_parser,
			{elas_parser, start_link, []},
			permanent, worker, 1000, [elas_parser]}]}}.


%% init([Num]) ->
%% 	1.
%
%% Spawn child supervisor, which one of
%% each process file pieces
-spec assign_parser(integer(), any()) -> 'ok'.
assign_parser() ->
	1.



