-module(compTest).
-compile(export_all).

yeccFilename() -> 'grammerTest'.
testFilename() -> 'test.ssidsp'.
semFilename() -> 'ssidsp7_1'.

keywordList() -> ['else', 'call', 'while', 'proc', 'thread', 'dim', 'new', 'program',
						'return', 'trace', 'wait', 'elseif', 'switch', 'default',
%						'int', 'rat', 'stack', 
						'true', 'false', 'for', 'return', 'break', 'func'].

inputByTerminalReparse() ->
	yecc:file(yeccFilename()),
	{ok,_} = compile:file(yeccFilename(), [{report_warnings, false}]),
	code:purge(yeccFilename()),
	code:load_file(yeccFilename()),
	{ok,L,_} = erl_scan:string(io:get_line("Input> ")),
	LL = fk(L),
	io:format("~w~n",[LL]),
	R = apply(yeccFilename(), parse, [LL]),
	io:format("~p~n",[R]).

inputByFileReparse() ->
	yecc:file(yeccFilename()),
	{ok,_} = compile:file(yeccFilename(), [{report_warnings, false}]),
	code:purge(yeccFilename()),
	code:load_file(yeccFilename()),
	{ok,BF} = file:read_file(testFilename()),
	{ok,L,_} = erl_scan:string(binary_to_list(BF)),
	LL = fk(L),
	io:format("~w~n",[LL]),
	R = apply(yeccFilename(), parse, [LL]),
	io:format("~p~n",[R]).

inputByFileNamedReparse() ->
	yecc:file(yeccFilename()),
	{ok,_} = compile:file(yeccFilename(), [{report_warnings, false}]),
	code:purge(yeccFilename()),
	code:load_file(yeccFilename()),
	[Fname] = string:tokens(io:get_line("FileName> ")," \n"),
	{ok,BF} = file:read_file(Fname),
	{ok,L,_} = erl_scan:string(binary_to_list(BF)),
	LL = fk(L),
	io:format("~w~n",[LL]),
	R = apply(yeccFilename(), parse, [LL]),
	io:format("~p~n",[R]).

inputByTerminal() ->
	{ok,L,_} = erl_scan:string(io:get_line("Input> ")),
	LL = fk(L),
	io:format("~w~n",[LL]),
	R = apply(yeccFilename(), parse, [LL]),
	io:format("~p~n",[R]).

inputByFile() ->
	{ok,BF} = file:read_file(testFilename()),
	{ok,L,_} = erl_scan:string(binary_to_list(BF)),
	LL = fk(L),
	io:format("~w~n",[LL]),
	R = apply(yeccFilename(), parse, [LL]),
	io:format("~p~n",[R]).

inputByFileNamed() ->
	[Fname] = string:tokens(io:get_line("FileName> ")," \n"),
	{ok,BF} = file:read_file(Fname),
	{ok,L,_} = erl_scan:string(binary_to_list(BF)),
	LL = fk(L),
	io:format("~w~n",[LL]),
	R = apply(yeccFilename(), parse, [LL]),
	io:format("~p~n",[R]).

semInputByTerminalRecompile() ->
	{ok,_} = compile:file(semFilename(), [{report_warnings, false}, debug_info]),
	code:purge(semFilename()),
	code:load_file(semFilename()),
	{ok,L,_} = erl_scan:string(io:get_line("Program> ")),
	LL = fk(L),
	{ok,R} = apply(yeccFilename(), parse, [LL]),
%	io:format("~p~n",[R]),
	{ok,IN} = io:read("Input List> "),
	Res = apply(semFilename(), sem, [{R,IN}]),
	io:format("~w~n",[Res]).

semInputByFileRecompile() ->
	{ok,_} = compile:file(semFilename(), [{report_warnings, false}, debug_info]),
	code:purge(semFilename()),
	code:load_file(semFilename()),
	{ok,BF} = file:read_file(testFilename()),
	{ok,L,_} = erl_scan:string(binary_to_list(BF)),
	LL = fk(L),
	{ok,R} = apply(yeccFilename(), parse, [LL]),
%	io:format("~p~n",[R]),
	{ok,IN} = io:read("Input List> "),
	Res = apply(semFilename(), sem, [{R,IN}]),
	io:format("~w~n",[Res]).

semInputByFileNamedRecompile() ->
	{ok,_} = compile:file(semFilename(), [{report_warnings, false}, debug_info]),
	code:purge(semFilename()),
	code:load_file(semFilename()),
	[Fname] = string:tokens(io:get_line("FileName> ")," \n"),
	{ok,BF} = file:read_file(Fname),
	{ok,L,_} = erl_scan:string(binary_to_list(BF)),
	LL = fk(L),
	{ok,R} = apply(yeccFilename(), parse, [LL]),
%	io:format("~p~n",[R]),
	{ok,IN} = io:read("Input List> "),
	Res = apply(semFilename(), sem, [{R,IN}]),
	io:format("~w~n",[Res]).

semInputByTerminal() ->
	{ok,L,_} = erl_scan:string(io:get_line("Program> ")),
	LL = fk(L),
	{ok,R} = apply(yeccFilename(), parse, [LL]),
%	io:format("~p~n",[R]),
	{ok,IN} = io:read("Input List> "),
	Res = apply(semFilename(), sem, [{R,IN}]),
	io:format("~w~n",[Res]).

semInputByFile() ->
	{ok,BF} = file:read_file(testFilename()),
	{ok,L,_} = erl_scan:string(binary_to_list(BF)),
	LL = fk(L),
	{ok,R} = apply(yeccFilename(), parse, [LL]),
%	io:format("~p~n",[R]),
	{ok,IN} = io:read("Input List> "),
	Res = apply(semFilename(), sem, [{R,IN}]),
	io:format("~w~n",[Res]).

semInputByFileNamed() ->
	[Fname] = string:tokens(io:get_line("FileName> ")," \n"),
	{ok,BF} = file:read_file(Fname),
	{ok,L,_} = erl_scan:string(binary_to_list(BF)),
	LL = fk(L),
	{ok,R} = apply(yeccFilename(), parse, [LL]),
%	io:format("~p~n",[R]),
	{ok,IN} = io:read("Input List> "),
	Res = apply(semFilename(), sem, [{R,IN}]),
	io:format("~w~n",[Res]).


fk(L) -> lists:map(fun compTest:filter_keyword/1,L).

filter_keyword({atom,N,T}) -> 
	case lists:member(T,keywordList()) of 
		true -> {T,N};
		false -> {atom,N,T}
	end;
filter_keyword(T) -> T.


