%% @author billcyz
%% @doc @todo Add description to elas_test.


-module(elas_test).

-export([start/0]).

-record(example, {}).

-include("elas_include.hrl").

%% -----------------------------------------------------------

%% sample_data_01 action: GET
%% sample_data_02 action: GET
%% sample_data_03 action: POST
%% sample_data_04 action: POST

%% Start prepare elas test environment
-spec start() -> 'ok'.
start() ->
	case is_pid(elas_meman) of
		true ->
			
		false -> internal_error
	end.

%% Add sample data to ets manager
-spec prepare_sample_data() -> 'ok'.
prepare_sample_data() ->
	Port = elas_server:get_service_port(),
	elas_server:add_project(?SAMPLE_SERVICE, Port),
	[add_url(Url) || ],
	1.

%% Add url
-spec add_url(list()) -> 'ok'.
add_url(Url) ->
	elas_server:add_project_url(?SAMPLE_SERVICE, Url).




