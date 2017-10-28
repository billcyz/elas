%% @author billcyz
%% @doc @todo Add description to elas_http.

%% HTTP module for ELAS
%% Manage http request

-module(elas_http).

-export([add_resource_path/1,
		 parse_resource_path/1]).

-include("elas_include.hrl").

%% -----------------------------------------------------------------

%% Add resource path
-spec add_resource_path(list()) -> 'ok'.
add_resource_path(Path) ->
	io:format("Final resource path is: ~p~n", [?SERVER_URL ++ Path]).

%% Parse resource path and redirect traffic
-spec parse_resource_path(list()) -> 'ok'.
parse_resource_path(Path) ->
	string:tokens(Path, "$e$l$a$s").
