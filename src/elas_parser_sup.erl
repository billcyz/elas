%% @author billcyz
%% @doc @todo Add description to elas_parser_sup.


%% Split file into pieces, and assign each
%% piece to each parsing supervisor 

-module(elas_parser_sup).
-behaviour(supervisor).
-export([init/1]).

-export([]).

-define(SERVER, elas_parser_sup).

%% -----------------------------------------------------------------

%% Start supervisor
-spec start_link(integer()) -> any().
start_link(Num) ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, [Num]).

init([Num]) ->
	1.

%% Spawn child supervisor, which one of
%% each process file pieces
-spec assign_parser(integer(), any()) -> 'ok'.
assign_parser() ->
	1.



