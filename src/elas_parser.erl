%% @author billcyz
%% @doc @todo Add description to elas_parser.


-module(elas_parser).

-export([]).

%% -----------------------------------------------------------------

%% Check dataset file types (plaintext, json, xml, .....) (file extension)
check_dataset_types(File) -> 1.


%% Parse input dataset (file)
get_input(File) ->
	{ok, IoD} = file:open(File, [read]),
	1.

%% Parse user request url
parse_url() ->
	1.