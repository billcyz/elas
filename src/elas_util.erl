%% @author billcyz
%% @doc @todo Add description to elas_util.


-module(elas_util).


-export([save_config/1, get_config/0,
		 start_messageNode/0]).


%% -----------------------------------------------------------------

%% Save elas config info
-spec save_config(tuple()) -> 'ok'.
save_config(Config) ->
	case elas_meman:check_table(elas_config) of
		undefined ->
			case elas_meman:create_table(
				   elas_config, [named_table]) of
				elas_config -> ok;
				E -> E
			end;
		_ ->
			elas_meman:delete_table(elas_config),
			elas_meman:create_table(elas_config, [named_table])
	end,

	1.

%% Get config parameters
-spec get_config() -> list().
get_config() ->
	[1].

%% Spawn message node 
%% (created by supervisor to communicate with other node)
-spec start_messageNode() -> atom().
start_messageNode() ->
	node().




