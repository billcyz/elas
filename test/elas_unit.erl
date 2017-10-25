%% @author billcyz
%% @doc @todo Add description to elas_unit.


%% Unit (component) test template

-module(elas_unit).

-export([parse_file/1,
		 test_server/1]).

-define(PROCESS_NUM, 1).

%% -----------------------------------------------------------------


%% Bind project (find available project in data management console)
%% Extract relative data
bind_project(ProjectId) ->
	1.

%% Test function (currently for parsing input file)
test(Fun) ->
	2.

%% Test parsing file
%% {file_info,106,regular,read_write,
%%            {{2017,10,5},{16,50,0}},
%%            {{2017,10,5},{16,50,0}},
%%            {{2017,10,5},{16,50,0}},
%%            33204,1,2058,0,137057,1000,1000}
-spec parse_file(any()) -> list().
parse_file(File) ->
	{ok, FileInfo} = file:read_file_info(File),
	[file_info, FileSize| _] = tuple_to_list(FileInfo), %% get file size in bytes
	%% calculate size for each supervisor
	AvgSize = FileSize div ?PROCESS_NUM,
	RstSize = FileSize rem ?PROCESS_NUM,
	io:format("File size is: ~p, average size is: ~p, rest size is: ~p~n",
			  [FileSize, AvgSize, RstSize]),
	
	{ok, FileBin} = file:read_file(File),
	
	<<>> = FileBin,
	
	1.


%% tcp server test
-spec test_server(atom()) -> 'ok'.
test_server(SrvType) ->
	case SrvType of
		tcp ->
			case start_tcp_server(10000) of
				E -> E
			end
	end.


%% tcp server template
-spec start_tcp_server(integer()) -> 'ok'.
start_tcp_server(Port) ->
	elas_unit_tcpserver_sup:start_link(Port).



%% ----------------------------------------------------------------------------
%% ----------------------------------------------------------------------------

%% parsing file pieces
-spec parse_file_piece(binary()) -> 'ok'.
parse_file_piece() ->
	1.





