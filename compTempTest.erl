-module(compTempTest).
-compile(export_all).

yeccFilename() -> "listTest".
testFilename() -> "test.ssidsp".

keywordList() -> ['else', 'call', 'while', 'proc', 'thread', 'dim', 'int', 'rat',
						'true', 'false', 'for', 'return', 'stack', 'break'].

inputByTerminalReparse() ->
	yecc:file(yeccFilename()),
	{ok,_} = compile:file(yeccFilename(), [{report_warnings, false}]),
	code:purge(listTest),
	code:load_file(listTest),
	{ok,L,_} = erl_scan:string(io:get_line("Input> ")),
	LL = fk(L),
	io:format("~w~n",[LL]),
	R = listTest:parse(LL),
	io:format("~p~n",[R]).

inputByFileReparse() ->
	yecc:file(yeccFilename()),
	{ok,_} = compile:file(yeccFilename(), [{report_warnings, false}]),
	code:purge(listTest),
	code:load_file(listTest),
	{ok,BF} = file:read_file(testFilename()),
	{ok,L,_} = erl_scan:string(binary_to_list(BF)),
	LL = fk(L),
	io:format("~w~n",[LL]),
	R = listTest:parse(LL),
	io:format("~p~n",[R]).

inputByFileNamedReparse() ->
	yecc:file(yeccFilename()),
	{ok,_} = compile:file(yeccFilename(), [{report_warnings, false}]),
	code:purge(listTest),
	code:load_file(listTest),
	[Fname] = string:tokens(io:get_line("FileName> ")," \n"),
	{ok,BF} = file:read_file(Fname),
	{ok,L,_} = erl_scan:string(binary_to_list(BF)),
	LL = fk(L),
	io:format("~w~n",[LL]),
	R = listTest:parse(LL),
	io:format("~p~n",[R]).

inputByTerminal() ->
	{ok,L,_} = erl_scan:string(io:get_line("Input> ")),
	LL = fk(L),
	io:format("~w~n",[LL]),
	R = listTest:parse(LL),
	io:format("~p~n",[R]).

inputByFile() ->
	{ok,BF} = file:read_file(testFilename()),
	{ok,L,_} = erl_scan:string(binary_to_list(BF)),
	LL = fk(L),
	io:format("~w~n",[LL]),
	R = listTest:parse(LL),
	io:format("~p~n",[R]).

inputByFileNamed() ->
	[Fname] = string:tokens(io:get_line("FileName> ")," \n"),
	{ok,BF} = file:read_file(Fname),
	{ok,L,_} = erl_scan:string(binary_to_list(BF)),
	LL = fk(L),
	io:format("~w~n",[LL]),
	R = listTest:parse(LL),
	io:format("~p~n",[R]).

semInputByTerminalRecompile() ->
	{ok,_} = compile:file("ssidsp4", [{report_warnings, false}, debug_info]),
	code:purge(ssidsp4),
	code:load_file(ssidsp4),
	{ok,L,_} = erl_scan:string(io:get_line("Program> ")),
	LL = fk(L),
	{ok,R} = listTest:parse(LL),
	io:format("~p~n",[R]),
	{ok,IN} = io:read("Input List> "),
	Res = ssidsp4:sem({R,IN}),
	io:format("~w~n",[Res]).

semInputByFileRecompile() ->
	{ok,_} = compile:file("ssidsp4", [{report_warnings, false}, debug_info]),
	code:purge(ssidsp4),
	code:load_file(ssidsp4),
	{ok,BF} = file:read_file(testFilename()),
	{ok,L,_} = erl_scan:string(binary_to_list(BF)),
	LL = fk(L),
	{ok,R} = listTest:parse(LL),
	io:format("~p~n",[R]),
	{ok,IN} = io:read("Input List> "),
	Res = ssidsp4:sem({R,IN}),
	io:format("~w~n",[Res]).

semInputByFileNamedRecompile() ->
	{ok,_} = compile:file("ssidsp4", [{report_warnings, false}, debug_info]),
	code:purge(ssidsp4),
	code:load_file(ssidsp4),
	[Fname] = string:tokens(io:get_line("FileName> ")," \n"),
	{ok,BF} = file:read_file(Fname),
	{ok,L,_} = erl_scan:string(binary_to_list(BF)),
	LL = fk(L),
	{ok,R} = listTest:parse(LL),
	io:format("~p~n",[R]),
	{ok,IN} = io:read("Input List> "),
	Res = ssidsp4:sem({R,IN}),
	io:format("~w~n",[Res]).

semInputByTerminal() ->
	% {ok,_} = compile:file("ssidsp4", [{report_warnings, false}, debug_info]),
	% code:purge(ssidsp4),
	% code:load_file(ssidsp4),
	{ok,L,_} = erl_scan:string(io:get_line("Program> ")),
	LL = fk(L),
	{ok,R} = listTest:parse(LL),
	io:format("~p~n",[R]),
	{ok,IN} = io:read("Input List> "),
	Res = ssidsp4:sem({R,IN}),
	io:format("~w~n",[Res]).

semInputByFile() ->
	% {ok,_} = compile:file("ssidsp4", [{report_warnings, false}, debug_info]),
	% code:purge(ssidsp4),
	% code:load_file(ssidsp4),
	{ok,BF} = file:read_file(testFilename()),
	{ok,L,_} = erl_scan:string(binary_to_list(BF)),
	LL = fk(L),
	{ok,R} = listTest:parse(LL),
	io:format("~p~n",[R]),
	{ok,IN} = io:read("Input List> "),
	Res = ssidsp4:sem({R,IN}),
	io:format("~w~n",[Res]).

semInputByFileNamed() ->
	% {ok,_} = compile:file("ssidsp4", [{report_warnings, false}, debug_info]),
	% code:purge(ssidsp4),
	% code:load_file(ssidsp4),
	[Fname] = string:tokens(io:get_line("FileName> ")," \n"),
	{ok,BF} = file:read_file(Fname),
	{ok,L,_} = erl_scan:string(binary_to_list(BF)),
	LL = fk(L),
	{ok,R} = listTest:parse(LL),
	io:format("~p~n",[R]),
	{ok,IN} = io:read("Input List> "),
	Res = ssidsp4:sem({R,IN}),
	io:format("~w~n",[Res]).


fk(L) -> lists:map(fun compTempTest:filter_keywaord/1,L).

filter_keywaord({atom,N,T}) -> 
	case lists:member(T,keywordList()) of 
		true -> {T,N};
		false -> {atom,N,T}
	end;
filter_keywaord(T) -> T.


