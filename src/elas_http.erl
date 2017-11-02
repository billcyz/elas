%% @author billcyz
%% @doc @todo Add description to elas_http.

%% HTTP module for ELAS
%% Manage http request

-module(elas_http).

-export([add_resource_path/1,
		 parse_resource_path/1]).

-include("elas_include.hrl").

%% -----------------------------------------------------------------

%% Store process
%% Add resource path
%% **** string, binary, json
-spec add_resource_path(list()) -> 'ok'.
add_resource_path(Path) ->
	io:format("Final resource path is: ~p~n", [?SERVER_URL ++ Path]),
	elas_meman:store_resource_path(Path).

%% Find resource data
-spec find_resource_data(binary() | list()) -> any().
find_resource_data(Path) ->
	ResPath = elas_parser:transfer_path(Path),
	

%% Retrieve process
%% Parse resource path and redirect traffic
%% string, binary
%% -spec parse_resource_path(list()) -> 'ok'.
%% parse_resource_path(Path) ->
%% 	%% not useful currently
%% 	string:tokens(Path, "/").