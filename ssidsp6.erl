-module(ssidsp6).
-compile(export_all).

loggingFileName() -> 'ssidsp.log'.

sem({P,IN}) ->
	put(trace,off), put(traceOPT,[]), put(traceSPY,all), 
	{A1,A2,A3} = now(), random:seed(A1, A2, A3),
	STA = sem(P, {IN,[],[[]]}), userLogging([], STA), STA.

sem([], STA) -> STA;
sem([S|SS], STA) -> userLogging({exec,S}, STA), STA1 = sem(S, STA), userLogging({exit,S}, STA1), sem(SS, STA1);
sem({blk,DL,S}, {IN,OUT,EST}) ->
	STA2 = decVars(DL, {IN,OUT,[[]|EST]}),
	userLogging({exec,S}, STA2),
	{IN2,OUT2,[_|EST2]} = STA3 = sem(S, STA2),
	userLogging({exit,S}, STA3),
	{IN2,OUT2,EST2};
sem({iff,E,S}, STA) ->
	userLogging({eval,E}, STA),
	{Val,STA1} = evalExp(E, STA),
	case Val of
		{int,0} -> STA1;
		_ -> userLogging({exec,S}, STA1), STA2 = sem(S, STA1), userLogging({exit,S}, STA2)
	end;
sem({ifelse,E,S1,S2}, STA) ->
	userLogging({eval,E}, STA),
	{Val,STA1} = evalExp(E, STA),
	case Val of
		{int,0} -> sem(S2, STA1);
		_ -> sem(S1, STA1)
	end;
sem({while,E,S}, STA) ->
	userLogging({eval,E}, STA),
	{Val,STA1} = evalExp(E, STA),
	case Val of
		{int,0} -> STA1;
		_ -> case S of
			{blk,_,_} -> sem({while,E,S}, sem(S, STA1));
			_ -> userLogging({exec,S}, STA1), sem({while,E,S}, sem(S, STA1))
			end
	end;
sem({for,S1,E,S2,S3}, STA) -> sem({blk,[],[S1,{while,E,[S3,S2]}]}, STA);
% sem({dim,{ary,V,L}}, STA) -> semAry({V,L}, STA);
sem({dec,{type,int},IntDecL}, STA) -> semIntDecL(IntDecL, STA);
sem({procDec,ProcName,PL,S}, STA) -> semProcDec({ProcName,PL,S}, STA);
% sem({funcDec,RetType,FuncName,PL,S}, STA) -> semFuncDec({FuncName,RetType,PL,S}, STA);
sem({procCall,N,PL}, STA) -> procCall(N, PL, STA);
sem({def,V,E}, STA) -> 
	userLogging({eval,E}, STA),
	{Val,STA1} = evalExp(E, STA),
	userLogging({val,Val}, STA1),
	semDef(V, Val, STA1);
sem({trace,OPT}, STA) -> userTrace(OPT, STA);

sem(S, _) -> fail(["Illegal Statement", S]).

semDef({var,_}=Var, {TypeVal,Val}, {IN,OUT,EST}) -> 
	NewEST = case chkDefESTbyKey(Var, EST) of
		true -> {{Type,_},Env}  = envESTbyKey(Var, EST),
			case Type==TypeVal of
				true -> true;
				false -> fail(["Type mismatch", Var, Type, {TypeVal,Val}])
			end,
			replaceESTbyKey(Var, EST, [{Var,{TypeVal,Val}}|del(Var, Env)]);
		false -> fail(["Not Declared", Var])
		% false -> [Env1|EST1] = EST,
		%	 [[{{var,V},Val}|Env1]|EST1]
	end,	
	{IN,OUT,NewEST};
semDef({ary,V,[E]}, {TypeVal,Val}, STA) -> 
	{{TypeInd,Ind},{IN1,OUT1,EST1}} = evalExp(E, STA),
	case TypeInd of
		int -> true;
		_ -> fail(["Illegal Type Index", {V,1,{TypeInd,Ind}}])
	end,
	Ary = {ary,V,1},
	{{Type,[MaxAry],ValAry},Env} = case chkDefESTbyKey(Ary, EST1) of
		true -> envESTbyKey(Ary, EST1);
		false -> fail(["Array Not Declared", {V,1}])
	end,
	case Type==TypeVal of
		true -> true;
		false -> fail(["Type mismatch", Ary, Type, {TypeVal,Val}])
	end,
	NewValAry = case (Ind < MaxAry) and (Ind >= 0) of
		true  -> replaceNth(Ind, {TypeVal,Val}, ValAry);
		false -> fail(["Index out of range", {V,Type,1,Ind}])
	end,
	NewEST = replaceESTbyKey(Ary, EST1, [{Ary,{Type,[MaxAry],NewValAry}}|del(Ary, Env)]),
	{IN1,OUT1,NewEST};
semDef(Var, Val, _) -> fail(["Illegal Assignment", {Var,Val}]).

semIntDecL([], STA) -> STA;
semIntDecL([IntDec|L], STA) -> semIntDecL(L, semIntDec(IntDec, STA)).

semIntDec({init,{def,{var,_}=Var,E}}, STA) -> 
	{{Type,Val},STA1} = evalExp(E, STA),
	case Type==int of
		true -> true;
		false -> fail(["Type mismatch", Var, int, {Type,Val}])
	end,
	STA2 = decVars([Var], STA1),
	semDef(Var, {Type,Val}, STA2);
semIntDec({init,{def,{ary,V,DM},E}}, STA) ->
	{Val,STA1} = evalExp(E, STA),
	decVars([{ary,V,DM,Val}], STA1);
semIntDec(Var, STA) -> decVars([Var], STA).

evalExp({int,Num}, STA) -> {{int,Num},STA};
evalExp({var,_}=Var, {_,_,EST}=STA) -> 
	{{Type,VarVal},_} = case chkDefESTbyKey(Var, EST) of
		true -> envESTbyKey(Var, EST);
		false -> fail(["Not defined", Var])
	end,
	case VarVal of
		undefined -> fail(["Not defined", Var]);
		_ -> {{Type,VarVal},STA}
	end;
evalExp({ary,V,[E]}, STA) -> 
	{{TypeInd,Ind},{_,_,EST1}=STA1} = evalExp(E, STA),
	case TypeInd of
		int -> true;
		_ -> fail(["Illegal Type Index", {V,1,[E]}, {TypeInd,Ind}])
	end,
	Ary = {ary,V,1},
	{{Type,[MaxAry],ValAry},_} = case chkDefESTbyKey(Ary, EST1) of
		true -> envESTbyKey(Ary, EST1);
		false -> fail(["Array Not Declared", Ary])
	end,
	{TypeVal,VarVal} = case (Ind < MaxAry) and (Ind >= 0) of
		true  -> lists:nth(Ind+1, ValAry);
		false -> fail(["Index out of range", {V,1,[Ind]}])
	end,
	if
	 	TypeVal /= Type ->
	 		fail(["Type mismatch in evalExp", {V,Type,1,Ind}, {TypeVal,VarVal}]);
	 	VarVal == undefined ->
			fail(["Not defined", {V,Type,1,Ind}]);
		true -> {{Type,VarVal},STA1}
	end;
evalExp({add,E1,E2}, STA) -> {{int,Num1},STA1} = evalExp(E1, STA), {{int,Num2},STA2} = evalExp(E2, STA1), {{int,Num1+Num2},STA2};
evalExp({sub,E1,E2}, STA) -> {{int,Num1},STA1} = evalExp(E1, STA), {{int,Num2},STA2} = evalExp(E2, STA1), {{int,Num1-Num2},STA2};
evalExp({mul,E1,E2}, STA) -> {{int,Num1},STA1} = evalExp(E1, STA), {{int,Num2},STA2} = evalExp(E2, STA1), {{int,Num1*Num2},STA2};
evalExp({divd,E1,E2}, STA) -> {{int,Num1},STA1} = evalExp(E1, STA), {{int,Num2},STA2} = evalExp(E2, STA1), {{int,Num1 div Num2},STA2};
evalExp({remm,E1,E2}, STA) -> {{int,Num1},STA1} = evalExp(E1, STA), {{int,Num2},STA2} = evalExp(E2, STA1), {{int,Num1 rem Num2},STA2};
evalExp({eq,E1,E2}, STA) -> {{int,Num1},STA1} = evalExp(E1, STA), {{int,Num2},STA2} = evalExp(E2, STA1), {{int,eq(Num1, Num2)},STA2};
evalExp({neq,E1,E2}, STA) -> {{int,Num1},STA1} = evalExp(E1, STA), {{int,Num2},STA2} = evalExp(E2, STA1), {{int,neq(Num1, Num2)},STA2};
evalExp({gt,E1,E2}, STA) -> {{int,Num1},STA1} = evalExp(E1, STA), {{int,Num2},STA2} = evalExp(E2, STA1), {{int,gt(Num1, Num2)},STA2};
evalExp({gte,E1,E2}, STA) -> {{int,Num1},STA1} = evalExp(E1, STA), {{int,Num2},STA2} = evalExp(E2, STA1), {{int,gte(Num1, Num2)},STA2};
evalExp({lt,E1,E2}, STA) -> {{int,Num1},STA1} = evalExp(E1, STA), {{int,Num2},STA2} = evalExp(E2, STA1), {{int,lt(Num1, Num2)},STA2};
evalExp({lte,E1,E2}, STA) -> {{int,Num1},STA1} = evalExp(E1, STA), {{int,Num2},STA2} = evalExp(E2, STA1), {{int,lte(Num1, Num2)},STA2};
evalExp({andd,E1,E2}, STA) -> 
	{{int,Num1},STA1} = evalExp(E1, STA), 
	case Num1==0 of
		true -> {{int,0},STA1};
		false -> {{int,Num2},STA2} = evalExp(E2, STA1), 
			case Num2==0 of
				true -> {{int,0},STA2};
				false -> {{int,1},STA2}
			end
	end;
evalExp({orr,E1,E2}, STA) -> 
	{{int,Num1},STA1} = evalExp(E1, STA), 
	case Num1==0 of
		true -> {{int,Num2},STA2} = evalExp(E2, STA1), 
			case Num2==0 of
				true -> {{int,0},STA2};
				false -> {{int,1},STA2}
			end;
		false -> {{int,1},STA1}
	end;
evalExp({nott,E}, STA) -> {{int,Num},STA1} = evalExp(E, STA), 
	case Num==0 of
		true -> {{int,1},STA1}; 
		false -> {{int,0},STA1}
	end;
evalExp({funcCall,N,PL}, STA) -> funcCall(N, PL, STA);
evalExp(E, _) -> fail(["Bad Expression", E]).

eq(V1, V2) -> case V1 == V2 of true -> 1; false -> 0 end.
neq(V1, V2) -> case V1 /= V2 of true -> 1; false -> 0 end.
gt(V1, V2) -> case V1 > V2 of true -> 1; false -> 0 end.
gte(V1, V2) -> case V1 >= V2 of true -> 1; false -> 0 end.
lt(V1, V2) -> case V1 < V2 of true -> 1; false -> 0 end.
lte(V1, V2) -> case V1 =< V2 of true -> 1; false -> 0 end.

semProcDec({ProcName,PL,S}, {IN,OUT,[Env|EST]}) ->
	Proc = {proc,ProcName,length(PL)},
	case chkDef(Proc, Env) of
		true -> fail(["Double defined in block!!",Proc]);
		false -> {IN,OUT,[[{Proc,{rmPar(PL),S}}|Env]|EST]}
	end.

semFuncDec({FuncName,{type,RetType},PL,S}, {IN,OUT,[Env|EST]}) ->
	Func = {func,FuncName,length(PL)},
	case chkDef(Func, Env) of
		true -> fail(["Double defined in block!!",Func]);
		false -> {IN,OUT,[[{Func,{RetType,rmPar(PL),S}}|Env]|EST]}
	end.

rmPar(ParL) -> lists:map(fun({par,{type,Type},Par}) -> {Type,Par} end, ParL).

procCall(write, PL, STA) -> writeStream(PL, STA);
procCall(print, PL, STA) -> writeConsle(PL, STA);
procCall(printStatus, [], STA) -> printStatus(STA);
procCall(halt, [], STA) -> userHalt(STA);
procCall(ProcName, PL, {_,_,EST}=STA) -> 
	Proc = {proc,ProcName,length(PL)},
	{{VPL,ST},_} = case chkDefESTbyKey(Proc, EST) of
		true -> envESTbyKey(Proc, EST);
		false -> fail(["Undefined Procedure", Proc])
	end,
	STA1 = parMatch(PL, VPL, {[],STA}),
	{IN2,OUT2,[_|EST2]} = case ST of
		{blk,_,ST1} -> sem(ST1, STA1);
		_ -> userLogging({exec,ST}, STA1),
			 STA2 = sem(ST, STA1), 
			 userLogging({exit,ST}, STA2),
			 STA2
	end,
	{IN2,OUT2,EST2}.

parMatch([], [], {Env,{IN,OUT,EST}}) -> {IN,OUT,[Env|EST]};
parMatch([{pary,_,_}=PAry|PL], [{_,{pary,_}}=VPAry|VPL], {Env,STA}) -> 
	{Env1,STA1} = parMatchAry(PAry, VPAry, {Env,STA}),
	parMatch(PL, VPL, {Env1,STA1});
parMatch([E|PL], [{_,{var,_}}=VPVar|VPL], {Env,STA}) -> 
	{Env1,STA1} = parMatchVar(E, VPVar, {Env,STA}),
	parMatch(PL, VPL, {Env1,STA1});
parMatch(PL, VPL, _) -> fail(["Parameter List Mismatch in Procedure", {PL,VPL}]).

parMatchVar(E, {Type,{var,_}=Var}, {Env,STA}) ->
	{{ValType,Val},STA1} = evalExp(E,STA),
	case Type==ValType of
		true -> true;
		false -> fail(["Parameter Type mismatch", Var, E, {ValType,Val}])
	end,
	case chkDef(Var, Env) of
		true -> fail(["Double defined in block!!", Var]);
		false -> {[{Var,{ValType,Val}}|Env],STA1}
	end.

parMatchAry({pary,PName,D}, {VPType,{pary,VPName}}, {Env,{_,_,EST}=STA}) ->
	Ary = {ary,PName,D},
	case chkDef({ary,VPName,D}, Env) of
		true -> fail(["Double defined in Procedure", {ary,VPName,D}]);
		false -> false
	end,
	{{Type,[MaxAry],ValAry},_} = case chkDefESTbyKey(Ary, EST) of
		true -> envESTbyKey(Ary, EST);
		false -> fail(["Parameter Array Not Declared", Ary])
	end,
	case Type == VPType of
	 	true -> {[{{ary,VPName,D},{Type,[MaxAry],ValAry}}|Env],STA};
	 	false -> fail(["Type mismatch in paramter", Ary, {pary,VPName,VPType}])
	end.


userHalt(STA) -> fail(["USER HALT!!", STA]).

userTrace(on, STA) -> 
	case  get(trace) of
		off ->
			Res = file:open(loggingFileName(), [append]),
			case Res of
				{ok,Dev} -> 
					put(trace, on), 
					put(traceDev, Dev),
					STA;
				_ -> fail(["Can't open logfile !!", loggingFileName()])
			end;
		on -> STA
	end;
% userTrace(all, STA) -> put(traceSPY, all), STA;
% userTrace([_|_]=L, STA) -> put(traceSPY, L), STA;
% userTrace([], STA) -> put(traceSPY, []), STA;
userTrace(off, STA) ->  
	case  get(trace) of
		on ->
			put(trace, off), 
			Dev = get(traceDev),
			file:close(Dev),
			STA;
		off -> STA
	end.


userLogging(S, {IN,OUT,_}=STA) -> 
	case  get(trace) of
		on ->
			Dev = get(traceDev),
			io:format(Dev, "[ ", []),
			io:write(Dev, calendar:universal_time()),
			io:format(Dev, " , ", []),
			io:write(Dev, S),
			io:format(Dev, " , ", []),
			io:write(Dev, IN),
			io:format(Dev, " , ", []),
			io:write(Dev, OUT),
			io:format(Dev, " ]~n", []);
		off -> false
	end,
	STA.



writeStream([], STA) -> STA;
writeStream([{string,S}|L], {IN,OUT,EST}) -> writeStream(L, {IN,[list_to_atom(S)|OUT],EST});
writeStream([{pary,PName,Dim}|L], {IN,OUT,EST}) -> 
	Ary = {ary,PName,Dim},
	{{_,_,ValAry},_} = case chkDefESTbyKey(Ary, EST) of
		true -> envESTbyKey(Ary, EST);
		false -> fail(["Parameter Array Not Declared", Ary])
	end,
	writeStream(L, {IN,[lists:map(fun format/1,ValAry)|OUT],EST});
writeStream([E|L], STA) -> {Val,{IN1,OUT1,EST1}} = evalExp(E, STA), writeStream(L, {IN1,[format(Val)|OUT1],EST1}).


writeConsle([], STA) -> io:format("~n"), STA;
writeConsle([{string,S}|L], STA) -> io:format(S,[]), writeConsle(L, STA);
writeConsle([{pary,PName,Dim}|L], {_,_,EST}=STA) -> 
	Ary = {ary,PName,Dim},
	{{Type,_,ValAry},_} = case chkDefESTbyKey(Ary, EST) of
		true -> envESTbyKey(Ary, EST);
		false -> fail(["Parameter Array Not Declared", Ary])
	end,
	io:format("~p ",[{Type,lists:map(fun format/1,ValAry)}]), 
	writeConsle(L, STA);
writeConsle([E|L], STA) -> {Val,STA1} = evalExp(E, STA), io:format("~p ",[format(Val)]), writeConsle(L, STA1).

format({int,N}) -> N;
format({var,V}) -> V;
format(Val) -> Val.

printStatus(STA) -> io:format("~p~n", [STA]), STA.

funcCall(read, [], STA) -> readStream(STA);
funcCall(input, [Prompt], STA) -> readConsle(Prompt, STA);
funcCall(random, [E], STA) ->
	{Val,STA1} = evalExp(E, STA),
	getRandom(Val, STA1);

funcCall(N, PL, _) -> fail(["Undefined Function", N, PL]).

% funcCall(FuncName, PL, {_,_,EST}=STA) -> 
% 	Func = {func,FuncName,length(PL)},
% 	{{TypeFunc,VPL,ST},_} = case chkDefESTbyKey(Func, EST) of
% 		true -> envESTbyKey(Func, EST);
% 		false -> fail(["Undefined Function", Func])
% 	end,
% 	STA1 = parMatch(PL, VPL, {[],STA}),
% 	{IN2,OUT2,[_|EST2]} = case ST of
% 		{blk,_,ST1} -> sem(ST1, STA1);
% 		_ -> sem(ST, STA1)
% 	end,
% 	{IN2,OUT2,EST2}.

readStream({[],_,_}) -> fail(["Nothing to Read"]);
readStream({[T|IN],OUT,Env}) -> 
	case T of
		Num when is_integer(Num) -> {{int,Num},{IN,OUT,Env}};
		_ -> fail(["Illigal Input Term", T])
	end.

readConsle({string,Pr}, STA) -> 
	{Res, Num} = io:read(Pr),
	case Res of
		ok when is_integer(Num) -> {{int,Num},STA};
		_ -> fail(["Illigal Format", Num])
	end.

getRandom({int,Val}, STA) -> {{int,random:uniform(Val)},STA};
getRandom(Val, _) -> fail(["Illigal Parameter in random", Val]).

decVars([], STA) -> STA;
decVars([{var,V}=Var|DL], {IN,OUT,[Env|L]}) -> 
	case chkDef(Var, Env) of
		true -> fail(["Double defined in block!!",{var,V}]);
		false -> decVars(DL, {IN,OUT,[[{Var,{int,undefined}}|Env]|L]})
	end;
decVars([{ary,V,[E]}|DL], STA) -> 
	{{Type,Val},{IN1,OUT1,[Env1|L]}} = evalExp(E, STA),
	case Type of
		int -> Val;
		_ -> fail(["Illegal Maxbound!! in dim", {ary,V,[E]}, Val])
	end,
	Ary = {{ary,V,1},{int,[Val],makeAry(Val,{int,undefined})}},
	case chkDef({ary,V,1}, Env1) of
		true -> fail(["Double defined in block!!", {ary,V,1}]);
		false -> decVars(DL, {IN1,OUT1,[[Ary|Env1]|L]})
	end;
decVars([{ary,V,[E],Default}|DL], STA) -> 
	{{Type,Val},{IN1,OUT1,[Env1|L]}} = evalExp(E, STA),
	case Type of
		int -> Val;
		_ -> fail(["Illegal Maxbound!! in dim", {{ary,V,[E]},Val}])
	end,
	Ary = {{ary,V,1},{int,[Val],makeAry(Val,Default)}},
	case chkDef({ary,V,1}, Env1) of
		true -> fail(["Double defined in block!!", {ary,V,1}]);
		false -> decVars(DL, {IN1,OUT1,[[Ary|Env1]|L]})
	end.

semAry({V,[E]}, STA) ->
	{{Type,Val},{IN1,OUT1,[Env1|EST1]}} = evalExp(E, STA),
	case Type of
		int -> Val;
		_ -> fail(["Illegal Maxbound!! in dim", {ary,V,[E]}, Val])
	end,
	Ary = {{ary,V,1},{int,[Val],makeAry(Val,{int,undefined})}},
	case chkDef({ary,V,1}, Env1) of
		true -> fail(["Already Declared", {ary,V,1}]);
		false -> {IN1,OUT1,[[Ary|Env1]|EST1]}
	end;
semAry(Ary, _) -> fail(["Illegal Dimensions!! in dim", Ary]).

makeAry(0, _) -> [];
makeAry(N, Default) -> [Default|makeAry(N-1, Default)].

chkDef(V, Env) -> proplists:is_defined(V, Env).

del(V, Env) -> proplists:delete(V, Env).

env(V, Env) -> proplists:get_value(V, Env).

replaceNth(0, V, [_|L]) -> [V|L];
replaceNth(N, V, [T|L]) -> [T|replaceNth(N-1, V, L)].

pushEST(Env,ST) -> [Env|ST].

popEST([Env|ST]) -> {Env,ST};
popEST([]) -> empty.

topEST([Env|_]) -> Env;
topEST([]) -> empty.

replaceESTbyKey(Key, [], _) -> fail(["Not found in EnvStack", Key]);
replaceESTbyKey(Key, [Env|EST], NewEnv) ->
	case chkDef(Key, Env) of
		true -> [NewEnv|EST];
		false -> [Env|replaceESTbyKey(Key, EST, NewEnv)]
	end.

chkDefESTbyKey(_, []) -> false;
chkDefESTbyKey(Key, [Env|EST]) ->
	case chkDef(Key, Env) of
		true -> true;
		false -> chkDefESTbyKey(Key, EST)
	end.

envESTbyKey(Key, []) -> fail(["Not found in EnvStack", Key]);
envESTbyKey(Key, [Env|EST]) ->
	case chkDef(Key, Env) of
		true -> {env(Key, Env),Env};
		false -> envESTbyKey(Key, EST)
	end.
	

fail([]) -> io:format("~n", []), exit('FAIL');
fail([M|L]) -> io:format("~p ", [M]), fail(L).