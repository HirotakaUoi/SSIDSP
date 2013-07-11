-module(ssidsp8_1).
-compile(export_all).

loggingFileName() -> 'ssidsp.log'.
scheduleSW() -> d.

sem({P,IN}) ->	put(trace,off), put(traceOPT,[]), put(traceSPY,[]), 
	{A1,A2,A3} = now(), random:seed(A1, A2, A3),
	{CEnv,_} = Env = getNewEmptyEnv([]),
	ExQ = pushST2ExQ(getNewEmptyExQ([]), {P,CEnv}),
	STA = {_,_,OUT,_} = exec({[ExQ],IN,[],[Env]}), 
	userLogging({stat,{CEnv,STA}}, STA), 
	STA.

exec({[{CExQ,{PRef,ChL,[{{program,_,P},CEnv}]}}],IN,OUT,EST}) -> exec({[{CExQ,{PRef,ChL,[{P,CEnv}]}}],IN,OUT,EST});
exec({[],_,_,_}=STA) -> STA;
exec({[{CExQ,{PRef,_,[]}}|EQL],IN,OUT,EST}) ->
	EQL1 = rmThreadRefFormParentExQinEQL(PRef, CExQ, EQL),
	exec({EQL1,IN,OUT,EST});
exec({[{CExQ,{PRef,ChL,[S|Q]}}|EQL],IN,OUT,EST}=STA) -> 
	userLogging({exec,S}, STA),
	STA1 = sem(S, {[{CExQ,{PRef,ChL,Q}}|EQL],IN,OUT,EST}), 
	userLogging({exit,S}, STA1),
	STA2 = shchedule(scheduleSW(), STA1),
	exec(STA2).

shchedule(_, {[],_,_,_}=STA) -> STA;
shchedule(_, {[_],_,_,_}=STA) -> STA;
shchedule(a, STA) -> STA;
shchedule(b, {EQL,IN,OUT,EST}) -> {lists:reverse(EQL),IN,OUT,EST};
shchedule(c, {[Q|EQL],IN,OUT,EST}) -> {EQL++[Q],IN,OUT,EST};
shchedule(d, {EQL,IN,OUT,EST}) -> 
	Q = lists:nth(random:uniform(length(EQL)), EQL),
	{[Q|lists:delete(Q,EQL)],IN,OUT,EST};
shchedule(e, {[Q1,Q2|EQL],IN,OUT,EST}) -> {[Q2,Q1|EQL],IN,OUT,EST}.


sem({[],_}, STA) -> STA;
sem({[_|_]=P,CEnv}, STA) -> pushST2ExQinSTA(lists:map(fun(X) -> {X,CEnv} end, P), STA);
sem({{blk,DL,S},CEnv}, {EQL,IN,OUT,EST}) ->
	{ERef,{_,_}} = NewEnv = getNewEmptyEnv(CEnv),
	STA1 = decVars(DL, {ERef,{EQL,IN,OUT,[NewEnv|EST]}}),
	sem({S++[{eraseEnv}],ERef}, STA1);
sem({{iff,E,S},CEnv}, STA) ->
	userLogging({eval,E,CEnv}, STA),
	{Val,STA1} = evalExp(E, {CEnv,STA}),
	userLogging({valu,Val}, STA1),
	case Val of
		{int,0} -> STA1;
		_ -> pushST2ExQinSTA({S,CEnv}, STA1)
	end;
sem({{ifelse,E,S1,S2},CEnv}, STA) ->
	userLogging({eval,E,CEnv}, STA),
	{Val,STA1} = evalExp(E, {CEnv,STA}),
	userLogging({valu,Val}, STA1),
	case Val of
		{int,0} -> pushST2ExQinSTA({S2,CEnv}, STA1);
		_ -> pushST2ExQinSTA({S1,CEnv}, STA1)
	end;
sem({{while,E,S},CEnv}, STA) ->
	userLogging({eval,E,CEnv}, STA),
	{Val,STA1} = evalExp(E, {CEnv,STA}),
	userLogging({valu,Val}, STA1),
	case Val of
		{int,0} -> STA1;
		_ -> pushST2ExQinSTA([{S,CEnv},{{while,E,S},CEnv},{{breakPt},CEnv}], STA1)
	end;
sem({{breakPt},_}, STA) -> STA;
sem({{eraseEnv},CEnv}, STA) -> userLogging({eEnv,CEnv}, STA), rmEnvByRefFromSTA(CEnv, STA);
sem({{break},_}, {[{CExQ,{PRef,ChL,Q}}|EQL],IN,OUT,EST}) -> 
		{PL,BL} = lists:splitwith(fun(X) -> (not is_tuple(element(1,X))) orelse element(1,element(1,X))/=breakPt end, Q),
		{_,Q1} = lists:splitwith(fun(X) -> element(1,element(1,X))==breakPt end, BL), 
		EL = lists:filter(fun(X) -> is_tuple(element(1,X)) andalso element(1,element(1,X))==eraseEnv end, PL),
		EST1 = lists:foldl(fun(X,Y) -> rmEnvByRef(element(2,X), Y) end, EST, EL),
		{[{CExQ,{PRef,ChL,Q1}}|EQL],IN,OUT,EST1};
sem({{for,S1,E,S2,S3},CEnv}, STA) -> sem({{blk,[],[S1,{while,E,[S3,S2]}]},CEnv}, STA);
% sem({{dim,{ary,V,L}},CEnv}, STA) -> semAry({V,L}, {CEnv,STA});
sem({{dec,{type,int},IntDecL},CEnv}, STA) -> semIntDecL(IntDecL, {CEnv,STA});
sem({{procDec,ProcName,PL,S},CEnv}, STA) -> semProcDec({ProcName,PL,S}, {CEnv,STA});
sem({{funcDec,RetType,FuncName,PL,S},CEnv}, STA) -> semFuncDec({FuncName,RetType,PL,S}, {CEnv,STA});
sem({{procCall,N,PL},CEnv}, STA) -> procCall(N, PL, {CEnv,STA});
sem({{return},_}, {[{CExQ,{PRef,ChL,Q}}|EQL],IN,OUT,EST}=STA) -> 
	{PL,Q1} = lists:splitwith(fun(X) -> (not is_tuple(element(1,X))) orelse element(1,element(1,X))/=returnPt end, Q),
	EL = lists:filter(fun(X) -> is_tuple(element(1,X)) andalso element(1,element(1,X))==eraseEnv end, PL),
	userLogging({eEnv,lists:map(fun(X) -> element(2,X) end, EL)}, STA),
	EST1 = lists:foldl(fun(X,Y) -> rmEnvByRef(element(2,X), Y) end, EST, EL),
	{[{CExQ,{PRef,ChL,Q1}}|EQL],IN,OUT,EST1};
sem({{returnPt,Proc},CEnv}, STA) -> userLogging({eEnv,CEnv}, STA), userLogging({retn,Proc}, STA), rmEnvByRefFromSTA(CEnv, STA);
sem({{return,E},CEnv}, STA) ->
	userLogging({eval,E,CEnv}, STA),
	{Val,{[{CExQ,{PRef,ChL,Q1}}|EQL1],IN1,OUT1,EST1}=STA1} = evalExp(E, {CEnv,STA}),
	userLogging({valu,Val}, STA1),
	{PL,[{{functionEndPt,Func},_}=F|Q2]} = lists:splitwith(fun(X) -> (not is_tuple(element(1,X))) orelse element(1,element(1,X))/=functionEndPt end, Q1), 
	EL = lists:filter(fun(X) -> is_tuple(element(1,X)) andalso element(1,element(1,X))==eraseEnv end, PL),
	userLogging({eEnv,lists:map(fun(X) -> element(2,X) end, [F|EL])}, STA1),
	EST2 = lists:foldl(fun(X,Y) -> rmEnvByRef(element(2,X), Y) end, EST1, [F|EL]),
	userLogging({retn,Func,Val}, {{CExQ,{PRef,ChL,Q2}},IN1,OUT1,EST2}),
	{[{CExQ,{PRef,ChL,Q2}}|EQL1],IN1,OUT1,[{returnVal,Val}|EST2]};
sem({{functionEndPt,Func},_}, _) -> fail(["No return statement in", Func]);
sem({{thread,S},CEnv}, {[{CExQ,{PRef,ChL,Q}}|EQL],IN,OUT,EST}=STA) -> 
	userLogging({thrd,S,CEnv}, STA), 
	{ExRef,_} = ExQ = pushST2ExQ(getNewEmptyExQ(CExQ), {S,CEnv}),
	{[ExQ,{CExQ,{PRef,[ExRef|ChL],Q}}|EQL],IN,OUT,EST};
sem({{wait},CEnv}, {[{_,{_,ChL,_}}|_],_,_,_}=STA) -> 
	case ChL of
		[] -> STA;
		_ -> pushST2ExQinSTA({{wait},CEnv}, STA)
	end;
sem({{def,V,E},CEnv}, STA) -> 
	userLogging({eval,E,CEnv}, STA),
	{Val,STA1} = evalExp(E, {CEnv,STA}),
	userLogging({valu,Val}, STA1),
	semDef(V, Val, {CEnv,STA1});
sem({{trace,OPT},_}, STA) -> userTrace(OPT, STA);
sem(S, _) -> fail(["Illegal Statement", S]).


semDef({var,_}=Var, {TypeVal,Val}, {CEnv,{EQ,IN,OUT,EST}}) -> 
	NewEST = case chkDefESTbyKey(Var, {CEnv,EST}) of
		true -> {{Type,_},{VEnvRef,{PEnvRef,Env}}}  = envESTbyKey(Var, {CEnv,EST}),
			case Type==TypeVal of
				true -> true;
				false -> fail(["Type mismatch", Var, Type, {TypeVal,Val}])
			end,
			replaceESTbyKey(VEnvRef, EST, {VEnvRef,{PEnvRef,[{Var,{TypeVal,Val}}|del(Var, Env)]}});
		false -> fail(["Not Declared", Var])
	end,	
	{EQ,IN,OUT,NewEST};
semDef({ary,V,[E]}, {TypeVal,Val}, {CEnv,STA}) -> 
	userLogging({eval,E,CEnv}, STA),
	{{TypeInd,Ind},{EQ1,IN1,OUT1,EST1}=STA1} = evalExp(E, {CEnv,STA}),
	userLogging({valu,{TypeInd,Ind}}, STA1),
	Ary = {ary,V,1},
	case TypeInd of
		int -> true;
		_ -> fail(["Illegal Type Index", Ary, {TypeInd,Ind}])
	end,
	{{Type,[MaxAry],ValAry},{AEnvRef,{PEnvRef,Env}}} = case chkDefESTbyKey(Ary, {CEnv,EST1}) of
		true -> envESTbyKey(Ary, {CEnv,EST1});
		false -> fail(["Array Not Declared", {V,1}])
	end,
	case Type==TypeVal of
		true -> true;
		false -> fail(["Type mismatch", Ary, Type, {TypeVal,Val}])
	end,
	NewValAry = case (Ind < MaxAry) and (Ind >= 0) of
		true  -> replaceNth(Ind, {TypeVal,Val}, ValAry);
		false -> fail(["Index out of range", Ary, Ind])
	end,
	NewEST = replaceESTbyKey(AEnvRef, EST1, {AEnvRef,{PEnvRef,[{Ary,{Type,[MaxAry],NewValAry}}|del(Ary, Env)]}}),
	{EQ1,IN1,OUT1,NewEST};
semDef(Var, Val, _) -> fail(["Illegal Assignment", Var, Val]).

semIntDecL([], {_,STA}) -> STA;
semIntDecL([IntDec|L], {CEnv,STA}) -> STA1 = semIntDec(IntDec, {CEnv,STA}), semIntDecL(L, {CEnv,STA1}).

semIntDec({init,{def,{var,_}=Var,E}}, {CEnv,STA}) -> 
	userLogging({eval,E,CEnv}, STA),
	{{Type,Val},STA1} = evalExp(E, {CEnv,STA}),
	userLogging({valu,{Type,Val}}, STA1),
	case Type==int of
		true -> true;
		false -> fail(["Type mismatch", Var, int, {Type,Val}])
	end,
	STA2 = decVars([Var], {CEnv,STA1}),
	semDef(Var, {Type,Val}, {CEnv,STA2});
semIntDec({init,{def,{ary,V,DM},E}}, {CEnv,STA}) ->
	userLogging({eval,E,CEnv}, STA),
	{Val,STA1} = evalExp(E, {CEnv,STA}),
	userLogging({valu,Val}, STA1),
	decVars([{ary,V,DM,Val}], {CEnv,STA1});
semIntDec(Var, STA) -> decVars([Var], STA).

evalExp({int,Num}, {_,STA}) -> {{int,Num},STA};
evalExp({var,_}=Var, {CEnv,STA}) -> 
	{{Type,VarVal},_} = case chkDefESTbyKeyFromSTA(Var, {CEnv,STA}) of
		true -> envESTbyKeyFromSTA(Var, {CEnv,STA});
		false -> fail(["Not defined", Var])
	end,
	case VarVal of
		undefined -> fail(["Not defined", Var]);
		_ -> {{Type,VarVal},STA}
	end;
evalExp({ary,V,[E]}, {CEnv,STA}) -> 
	{{TypeInd,Ind},STA1} = evalExp(E, {CEnv,STA}),
	Ary = {ary,V,1},
	case TypeInd of
		int -> true;
		_ -> fail(["Illegal Type Index", {Ary,[E]}, {TypeInd,Ind}])
	end,
	{{Type,[MaxAry],ValAry},_} = case chkDefESTbyKeyFromSTA(Ary, {CEnv,STA1}) of
		true -> envESTbyKeyFromSTA(Ary, {CEnv,STA1});
		false -> fail(["Array Not Declared", Ary])
	end,
	{TypeVal,VarVal} = case (Ind < MaxAry) and (Ind >= 0) of
		true  -> lists:nth(Ind+1, ValAry);
		false -> fail(["Index out of range", {Ary,[Ind]}])
	end,
	if
	 	TypeVal /= Type ->
	 		fail(["Type mismatch in evalExp", {V,Type,1,Ind}, {TypeVal,VarVal}]);
	 	VarVal == undefined ->
			fail(["Not defined", {V,Type,1,Ind}]);
		true -> {{Type,VarVal},STA1}
	end;
evalExp({add,E1,E2}, {CEnv,_}=STA) -> {{int,Num1},STA1} = evalExp(E1, STA), {{int,Num2},STA2} = evalExp(E2, {CEnv,STA1}), {{int,Num1+Num2},STA2};
evalExp({sub,E1,E2}, {CEnv,_}=STA) -> {{int,Num1},STA1} = evalExp(E1, STA), {{int,Num2},STA2} = evalExp(E2, {CEnv,STA1}), {{int,Num1-Num2},STA2};
evalExp({mul,E1,E2}, {CEnv,_}=STA) -> {{int,Num1},STA1} = evalExp(E1, STA), {{int,Num2},STA2} = evalExp(E2, {CEnv,STA1}), {{int,Num1*Num2},STA2};
evalExp({divd,E1,E2}, {CEnv,_}=STA) -> {{int,Num1},STA1} = evalExp(E1, STA), {{int,Num2},STA2} = evalExp(E2, {CEnv,STA1}), {{int,Num1 div Num2},STA2};
evalExp({remm,E1,E2}, {CEnv,_}=STA) -> {{int,Num1},STA1} = evalExp(E1, STA), {{int,Num2},STA2} = evalExp(E2, {CEnv,STA1}), {{int,Num1 rem Num2},STA2};
evalExp({eq,E1,E2}, {CEnv,_}=STA) -> {{int,Num1},STA1} = evalExp(E1, STA), {{int,Num2},STA2} = evalExp(E2, {CEnv,STA1}), {{int,eq(Num1, Num2)},STA2};
evalExp({neq,E1,E2}, {CEnv,_}=STA) -> {{int,Num1},STA1} = evalExp(E1, STA), {{int,Num2},STA2} = evalExp(E2, {CEnv,STA1}), {{int,neq(Num1, Num2)},STA2};
evalExp({gt,E1,E2}, {CEnv,_}=STA) -> {{int,Num1},STA1} = evalExp(E1, STA), {{int,Num2},STA2} = evalExp(E2, {CEnv,STA1}), {{int,gt(Num1, Num2)},STA2};
evalExp({gte,E1,E2}, {CEnv,_}=STA) -> {{int,Num1},STA1} = evalExp(E1, STA), {{int,Num2},STA2} = evalExp(E2, {CEnv,STA1}), {{int,gte(Num1, Num2)},STA2};
evalExp({lt,E1,E2}, {CEnv,_}=STA) -> {{int,Num1},STA1} = evalExp(E1, STA), {{int,Num2},STA2} = evalExp(E2, {CEnv,STA1}), {{int,lt(Num1, Num2)},STA2};
evalExp({lte,E1,E2}, {CEnv,_}=STA) -> {{int,Num1},STA1} = evalExp(E1, STA), {{int,Num2},STA2} = evalExp(E2, {CEnv,STA1}), {{int,lte(Num1, Num2)},STA2};
evalExp({andd,E1,E2}, {CEnv,_}=STA) -> 
	{{int,Num1},STA1} = evalExp(E1, STA), 
	case Num1==0 of
		true -> {{int,0},STA1};
		false -> {{int,Num2},STA2} = evalExp(E2, {CEnv,STA1}), 
			case Num2==0 of
				true -> {{int,0},STA2};
				false -> {{int,1},STA2}
			end
	end;
evalExp({orr,E1,E2}, {CEnv,_}=STA) -> 
	{{int,Num1},STA1} = evalExp(E1, STA), 
	case Num1==0 of
		true -> {{int,Num2},STA2} = evalExp(E2, {CEnv,STA1}), 
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
evalExp({funcCall,N,PL}=S, {CEnv,STA}) -> userLogging({exec,S,CEnv}, STA), {Val,STA1} = funcCall(N, PL, {CEnv,STA}), userLogging({exit,S,Val}, STA1), {Val,STA1} ;
evalExp(E, _) -> fail(["Bad Expression", E]).

eq(V1, V2) -> case V1 == V2 of true -> 1; false -> 0 end.
neq(V1, V2) -> case V1 /= V2 of true -> 1; false -> 0 end.
gt(V1, V2) -> case V1 > V2 of true -> 1; false -> 0 end.
gte(V1, V2) -> case V1 >= V2 of true -> 1; false -> 0 end.
lt(V1, V2) -> case V1 < V2 of true -> 1; false -> 0 end.
lte(V1, V2) -> case V1 =< V2 of true -> 1; false -> 0 end.

semProcDec({ProcName,PL,S}, {CEnv,STA}) ->
	Proc = {proc,ProcName,length(PL)},
	{CEnv,{PEnvRef,Env}} = getEnvByRefFromSTA(CEnv, STA),
	ST1 = case chkDef(Proc, Env) of
		true -> fail(["Double defined in block!!",Proc]);
		false -> case S of 
				{blk,_,ST} -> ST++[{returnPt,Proc}];
				ST -> [ST,{returnPt,Proc}]
			end
	end,
	replaceESTbyKeyInSTA(CEnv, STA, {CEnv,{PEnvRef,[{Proc,{rmPar(PL),ST1}}|Env]}}).


semFuncDec({FuncName,{type,RetType},PL,S}, {CEnv,STA}) ->
	Func = {func,FuncName,length(PL)},
	{CEnv,{PEnvRef,Env}} = getEnvByRefFromSTA(CEnv, STA),
	ST1 = case chkDef(Func, Env) of
		true -> fail(["Double defined in block!!",Func]);
		false -> case S of 
				{blk,_,ST} -> ST++[{functionEndPt,Func}];
				ST -> [ST,{functionEndPt,Func}]
			end
	end,
	replaceESTbyKeyInSTA(CEnv, STA, {CEnv,{PEnvRef,[{Func,{RetType,rmPar(PL),ST1}}|Env]}}).


rmPar(ParL) -> lists:map(fun({par,{type,Type},Par}) -> {Type,Par} end, ParL).

procCall(write, PL, STA) -> writeStream(PL, STA);
procCall(print, PL, STA) -> writeConsle(PL, STA);
procCall(printStatus, [], STA) -> printStatus(STA);
procCall(halt, [], STA) -> userHalt(STA);
procCall(ProcName, PL, {CEnv,STA}) -> 
	Proc = {proc,ProcName,length(PL)},
	{{VPL,ST},_} = case chkDefESTbyKeyFromSTA(Proc, {CEnv,STA}) of
			true -> envESTbyKeyFromSTA(Proc, {CEnv,STA});
			false -> fail(["Undefined Procedure", Proc])
		end,
	{ERef,_} = NewEnv = getNewEmptyEnv(CEnv),
	STA1 = parMatch(PL, VPL, ERef, CEnv, addEnv2ESTinSTA(NewEnv, STA)),
	sem({ST,ERef}, STA1);
procCall(ProcName, PL, _) -> Proc = {proc,ProcName,length(PL)}, fail(["Illegal Procedure Call", Proc]).


parMatch([], [], _, _, STA) -> STA;
parMatch([{pary,_,_}=PAry|PL], [{_,{pary,_}}=VPAry|VPL], ERef, CEnv, STA) -> 
	STA1 = parMatchAry(PAry, VPAry, ERef, CEnv, STA),
	parMatch(PL, VPL, ERef, CEnv, STA1);
parMatch([E|PL], [{_,{var,_}}=VPVar|VPL], ERef, CEnv, STA) -> 
	STA1 = parMatchVar(E, VPVar, ERef, CEnv, STA),
	parMatch(PL, VPL, ERef, CEnv, STA1);
parMatch(PL, VPL, _, _, _) -> fail(["Parameter List Mismatch in Procedure", {PL,VPL}]).

parMatchVar(E, {Type,{var,_}=Var}, ERef, CEnv, STA) ->
	userLogging({eval,E, CEnv}, STA),
	{{ValType,Val},STA1} = evalExp(E, {CEnv,STA}),
	userLogging({valu,{ValType,Val}}, STA1),
	case Type==ValType of
		true -> true;
		false -> fail(["Parameter Type mismatch", Var, E, {ValType,Val}])
	end,
	{ERef,{PEnvRef,Env}} = getEnvByRefFromSTA(ERef, STA),
	case chkDef(Var, Env) of
		true -> fail(["Double defined in block!!", Var]);
		false -> replaceESTbyKeyInSTA(ERef, STA, {ERef,{PEnvRef,[{Var,{ValType,Val}}|Env]}})
	end.

parMatchAry({pary,PName,D}, {VPType,{pary,VPName}}, ERef, CEnv, STA) ->
	Ary = {ary,PName,D},
	{ERef,{PEnvRef,Env}} = getEnvByRefFromSTA(ERef, STA),
	case chkDef({ary,VPName,D}, Env) of
		true -> fail(["Double defined in Procedure", {ary,VPName,D}]);
		false -> false
	end,
	{{Type,[MaxAry],ValAry},Env} = case chkDefESTbyKeyFromSTA(Ary, {CEnv,STA}) of
		true -> envESTbyKeyFromSTA(Ary, {CEnv,STA});
		false -> fail(["Parameter Array Not Declared", Ary])
	end,
	case Type == VPType of
	 	true -> replaceESTbyKeyInSTA(ERef, STA,  {ERef,{PEnvRef,[{{ary,VPName,D},{Type,[MaxAry],ValAry}}|Env]}});
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
userTrace(full, STA) -> 
	OPT = get(traceOPT),
	case lists:member(full, OPT) of
		true -> OPT;
		false -> put(traceOPT, [full|OPT])
	end,
	STA;
userTrace(normal, STA) -> put(traceOPT,lists:delete(full, get(traceOPT))), STA;
userTrace([_|_]=L, STA) -> put(traceSPY, L), STA;
userTrace([], STA) -> put(traceSPY, []), STA;
userTrace(console, STA) ->
	OPT = get(traceOPT),
	case lists:member(file, OPT) of
		true -> OPT;
		false -> put(traceOPT, [console|OPT])
	end,
	STA;
userTrace(file, STA) -> put(traceOPT,lists:delete(console, get(traceOPT))), STA;
userTrace(off, STA) ->  
	case  get(trace) of
		on ->
			put(trace, off), 
			Dev = get(traceDev),
			file:close(Dev),
			STA;
		off -> STA
	end.

userLogging(S, STA) -> 
	case get(trace) of
		on -> case get(traceSPY) of
				[] -> makeLog(S, STA);
				SPY -> case isTraceKey(S, SPY) of
						true -> makeLog(S, STA);
						false -> false
					end
			end;
		off -> false
	end,
	STA.

isTraceKey(S, SPY) ->
	case tuple_to_list(S) of
		[TTYPE,SS|_] when (not is_list(SS)) andalso ((TTYPE==exec) orelse (TTYPE==exit)) ->
			case lists:member(TTYPE, SPY) of
				true -> true;
				false -> STYPE=element(1, SS),
					case lists:member(STYPE, SPY) of
						true -> true;
						false when (STYPE==procCall) orelse (STYPE==funcCall) -> lists:member(element(2,SS), SPY);
						false -> false
					end
			end;
		[TTYPE|_] -> lists:member(TTYPE, SPY)
	end.

makeLog(S, STA) ->
	case lists:member(console, get(traceOPT)) of 
		true ->
			io:format("[ ", []),
			io:write(calendar:universal_time()),
			io:format(" , ", []),
			io:write(S),
			case lists:member(full, get(traceOPT)) of	
				true -> io:format(" , ", []),
						io:write(STA);
				false -> false
			end,
			io:format(" ]~n", []);
		false ->
			Dev = get(traceDev),
			io:format(Dev, "[ ", []),
			io:write(Dev, calendar:universal_time()),
			io:format(Dev, " , ", []),
			io:write(Dev, S),
			case lists:member(full, get(traceOPT)) of	
				true -> io:format(Dev, " , ", []),
						io:write(Dev, STA);
				false -> false
			end,
			io:format(Dev, " ]~n", [])
	end.


writeStream([], {_,STA}) -> STA;
writeStream([{string,S}|L], {CEnv,STA}) -> 
	userLogging({writ,list_to_atom(S)}, STA), 
	writeStream(L, {CEnv,appendOUTinSTA(list_to_atom(S), STA)});
writeStream([{pary,PName,Dim}|L], {CEnv,STA}) -> 
	Ary = {ary,PName,Dim},
	{{_,_,ValAry},_} = case chkDefESTbyKeyFromSTA(Ary, {CEnv,STA}) of
		true -> envESTbyKeyFromSTA(Ary, {CEnv,STA});
		false -> fail(["Parameter Array Not Declared", Ary])
	end,
	userLogging({writ,{Ary,lists:map(fun format/1,ValAry)}}, STA), 
	writeStream(L, {CEnv,appendOUTinSTA(lists:map(fun format/1,ValAry), STA)});
writeStream([E|L], {CEnv,STA}) -> 
	userLogging({eval,E,CEnv}, STA),
	{Val,STA1} = evalExp(E, {CEnv,STA}), 
	userLogging({valu,Val}, STA1),
	userLogging({writ,format(Val)}, STA1),
	writeStream(L, {CEnv,appendOUTinSTA(format(Val), STA1)}).

appendOUTinSTA(A, {Q,IN,OUT,EST}) -> {Q,IN,[A|OUT],EST}.

writeConsle([], {_,STA}) -> io:format("~n"), STA;
writeConsle([{string,S}|L], {CEnv,STA}) -> 
	userLogging({prin,list_to_atom(S)}, STA), 
	io:format(S,[]), writeConsle(L, {CEnv,STA});
writeConsle([{pary,PName,Dim}|L], {CEnv,STA}) -> 
	Ary = {ary,PName,Dim},
	{{Type,_,ValAry},_} = case chkDefESTbyKeyFromSTA(Ary, {CEnv,STA}) of
		true -> envESTbyKeyFromSTA(Ary, {CEnv,STA});
		false -> fail(["Parameter Array Not Declared", Ary])
	end,
	userLogging({prin,{Ary,lists:map(fun format/1,ValAry)}}, STA), 
	io:format("~p ",[{Type,lists:map(fun format/1,ValAry)}]), 
	writeConsle(L, {CEnv,STA});
writeConsle([E|L], {CEnv,STA}) -> 
	userLogging({eval,E,CEnv}, STA),
	{Val,STA1} = evalExp(E, {CEnv,STA}), 
	userLogging({valu,Val}, STA1),
	userLogging({prin,format(Val)}, STA1),
	io:format("~p",[format(Val)]), 
	writeConsle(L, {CEnv,STA1}).

format({int,N}) -> N;
format({var,V}) -> V;
format(Val) -> Val.

printStatus({CEnv,STA}) -> 
	userLogging({stat,{CEnv,STA}}, STA),
	io:format("~p~n", [STA]), STA.

funcCall(read, [], {_,STA}) -> readStream(STA);
funcCall(input, [Prompt], {_,STA}) -> readConsle(Prompt, STA);
funcCall(random, [E], {CEnv,STA}) ->
	userLogging({eval,E,CEnv}, STA),
	{Val,STA1} = evalExp(E, {CEnv,STA}),
	userLogging({valu,Val}, STA1),
	getRandom(Val, STA1);


funcCall(FuncName, PL, {CEnv,STA}) -> 
	Func = {func,FuncName,length(PL)},
	{{TypeFunc,VPL,ST},_} = case chkDefESTbyKeyFromSTA(Func, {CEnv,STA}) of
		true -> envESTbyKeyFromSTA(Func, {CEnv,STA});
		false -> fail(["Undefined Function", Func])
	end,
	{ERef,_} = NewEnv = getNewEmptyEnv(CEnv),
	{[Q1|EQ1],IN1,OUT1,EST1} = parMatch(PL, VPL, ERef, CEnv, addEnv2ESTinSTA(NewEnv, STA)),
	{EQ2,IN2,OUT2,[{returnVal,{TypeVal,Val}}|EST2]} = exec({[[{ST,ERef}]|EQ1], IN1, OUT1, EST1}),
	case TypeFunc==TypeVal of 
		true -> userLogging({exit,ST}, {[Q1|EQ2],IN2,OUT2,EST2}), {{TypeVal,Val},{[Q1|EQ2],IN2,OUT2,EST2}};
		false -> fail(["Return Value Tpe Mismatch", Func, {TypeVal,Val}])
	end;
funcCall(FuncName, PL, _) -> Func = {func,FuncName,length(PL)}, fail(["Illegal Function Call", Func]).

readStream({_,[],_,_}) -> fail(["Nothing to Read"]);
readStream({EQ,[T|IN],OUT,EST}=STA) -> 
	case T of
		Num when is_integer(Num) -> userLogging({read,{int,Num}}, STA), {{int,Num},{EQ,IN,OUT,EST}};
		_ -> fail(["Illigal Form from Input Stream", T])
	end.

readConsle({string,Pr}, STA) -> 
	{Res, Num} = io:read(Pr),
	case Res of
		ok when is_integer(Num) -> userLogging({inpt,{int,Num}}, STA), {{int,Num},STA};
		_ -> fail(["Illigal Input Form", Num])
	end.

getRandom({int,Val}, STA) -> {{int,random:uniform(Val)},STA};
getRandom(Val, _) -> fail(["Illigal Parameter in random function", Val]).


decVars([], {_,STA}) -> STA;
decVars([{var,_}=Var|DL], {CEnv,STA}) -> 
	{CEnv,{PEnvRef,Env}} = getEnvByRefFromSTA(CEnv, STA),
	case chkDef(Var, Env) of
		true -> fail(["Double defined in block!!", Var]);
		false -> STA1 = replaceESTbyKeyInSTA(CEnv, STA, {CEnv,{PEnvRef,[{Var,{int,undefined}}|Env]}}),
			decVars(DL, {CEnv,STA1})
	end;
decVars([{ary,V,[E]}|DL], {CEnv,STA}) -> 
	userLogging({eval,E,CEnv}, STA),
	{{Type,Val},STA1} = evalExp(E, {CEnv,STA}),
	userLogging({valu,{Type,Val}}, STA1),
	case Type of
		int -> Val;
		_ -> fail(["Illegal Maxbound!! in dim", {ary,V,[E]}, Val])
	end,
	Ary = {ary,V,1},
	{CEnv,{PEnvRef,Env1}} = getEnvByRefFromSTA(CEnv, STA1),
	case chkDef(Ary, Env1) of
		true -> fail(["Double defined in block!!", Ary]);
		false -> STA2 = replaceESTbyKeyInSTA(CEnv, STA1, {CEnv,{PEnvRef,[{Ary,{int,[Val],makeAry(Val,{int,undefined})}}|Env1]}}),
				decVars(DL, {CEnv,STA2})
	end;
decVars([{ary,V,[E],Default}|DL], {CEnv,STA}) -> 
	userLogging({eval,E,CEnv}, STA),
	{{Type,Val},STA1} = evalExp(E, {CEnv,STA}),
	userLogging({valu,{Type,Val}}, STA1),
	case Type of
		int -> Val;
		_ -> fail(["Illegal Maxbound!! in dim", {{ary,V,[E]},Val}])
	end,
	Ary = {ary,V,1},
	{CEnv,{PEnvRef,Env1}} = getEnvByRefFromSTA(CEnv, STA1),
	case chkDef({ary,V,1}, Env1) of
		true -> fail(["Double defined in block!!", Ary]);
		false -> STA2 = replaceESTbyKeyInSTA(CEnv, STA1, {CEnv,{PEnvRef,[{Ary,{int,[Val],makeAry(Val,Default)}}|Env1]}}),
				decVars(DL, {CEnv,STA2})
	end;
decVars([Dec|_], {_,_}) -> fail(["Illegal Declearation!!", Dec]).


% semAry({V,[E]}, {CEnv,STA}) ->
% 	userLogging({eval,E,CEnv}, STA),
% 	{{Type,Val},STA1} = evalExp(E, {CEnv,STA}),
% 	userLogging({valu,{Type,Val}}, STA1),
% 	case Type of
% 		int -> Val;
% 		_ -> fail(["Illegal Maxbound!! in dim", {ary,V,[E]}, Val])
% 	end,
% 	Ary = {{ary,V,1},{int,[Val],makeAry(Val,{int,undefined})}},
% 	case chkDef({ary,V,1}, Env1) of
% 		true -> fail(["Already Declared", {ary,V,1}]);
% 		false -> {IN1,OUT1,[[Ary|Env1]|EST1]}
% 	end;
% semAry(Ary, _) -> fail(["Illegal Dimensions!! in dim", Ary]).

makeAry(0, _) -> [];
makeAry(N, Default) -> [Default|makeAry(N-1, Default)].

chkDef(V, Env) -> proplists:is_defined(V, Env).

del(V, Env) -> proplists:delete(V, Env).

env(V, Env) -> proplists:get_value(V, Env).

replaceNth(0, V, [_|L]) -> [V|L];
replaceNth(N, V, [T|L]) -> [T|replaceNth(N-1, V, L)].

getNewEmptyEnv(RefParent) ->
	Ref = make_ref(),
	{Ref,{RefParent,[]}}.

getEnvByRef(Ref, EST) -> {Ref,proplists:get_value(Ref, EST)}.
getEnvByRefFromSTA(Ref, {_,_,_,EST}) -> {Ref,proplists:get_value(Ref, EST)}.


pushEST(Env,ST) -> [Env|ST].

popEST([Env|ST]) -> {Env,ST};
popEST([]) -> empty.

topEST([Env|_]) -> Env;
topEST([]) -> empty.

rmEnvByRefFromSTA(CEnv, {EQ,IN,OUT,EST}) -> {EQ,IN,OUT,del(CEnv, EST)}.
rmEnvByRef(CEnv, EST) -> del(CEnv, EST).
% rmEnvByRefFromSTA(_, {EQ,IN,OUT,EST}) -> {EQ,IN,OUT,EST}.
% rmEnvByRef(_, EST) -> EST.

addEnv2ESTinSTA(Env, {EQ,IN,OUT,EST}) -> {EQ,IN,OUT,[Env|EST]}.

replaceESTbyKeyInSTA(Key, {EQ,IN,OUT,EST}, NewEnv) -> 
	{EQ,IN,OUT,replaceESTbyKey(Key, EST, NewEnv)}.

replaceESTbyKey(Key, EST, NewEnv) ->
	case chkDef(Key, EST) of
		true -> [NewEnv|del(Key, EST)];
		false -> fail(["Not found in EnvStack", Key])
	end.

chkDefESTbyKeyFromSTA(Key, {CEnv,{_,_,_,EST}}) -> chkDefESTbyKey(Key, {CEnv,EST}).

chkDefESTbyKey(_, {[],_}) -> false;
chkDefESTbyKey(Key, {CEnv,EST}) ->
	{PEnvRef,Env} = case proplists:get_value(CEnv, EST) of
		undefined -> fail(["Not found specified Env in EST", {CEnv,EST}]);
		E -> E
	end,
	case chkDef(Key, Env) of
		true -> true;
		false -> chkDefESTbyKey(Key, {PEnvRef,EST})
	end.

envESTbyKeyFromSTA(Key, {CEnv,{_,_,_,EST}}) -> envESTbyKey(Key, {CEnv,EST}).

envESTbyKey(Key, {[],_}) -> fail(["Not found in EnvStack", Key]);
envESTbyKey(Key, {CEnv,EST}) ->
	{PEnvRef,Env} = proplists:get_value(CEnv, EST),
	case chkDef(Key, Env) of
		true -> {env(Key, Env),{CEnv,{PEnvRef,Env}}};
		false -> envESTbyKey(Key, {PEnvRef,EST})
	end.


getNewEmptyExQ(RefParent) ->
	Ref = make_ref(),
	{Ref,{RefParent,[],[]}}.

getExQByRef(Ref, EQL) -> {Ref,proplists:get_value(Ref, EQL)}.
getExQByRefFromSTA(Ref, {EQL,_,_,_}) -> {Ref,proplists:get_value(Ref, EQL)}.

pushST2ExQinSTA(ST, {[Q|EQL],IN,OUT,EST}) -> 
	case is_list(ST) of
		true -> {[pushSTL2ExQ(Q, ST)|EQL],IN,OUT,EST};
		false -> {[pushST2ExQ(Q, ST)|EQL],IN,OUT,EST}
	end.

pushST2ExQ({CExQ,{PRef,ChL,Q}},ST) -> {CExQ,{PRef,ChL,[ST|Q]}}.
pushSTL2ExQ({CExQ,{PRef,ChL,Q}},STL) -> {CExQ,{PRef,ChL,STL++Q}}.

popExQ({CExQ,{PRef,ChL,[ST|Q]}}) -> {ST,{CExQ,{PRef,ChL,Q}}};
popExQ({_,{_,_,[]}}) -> fail("Pop from empty ExQ").

topEQL([ExQ|_]) -> ExQ;
topEQL([]) -> fail("Pop from empty ExQ").

rmThreadRefFormParentExQinEQL([], _, EQL) -> EQL;
rmThreadRefFormParentExQinEQL(ExQRef, ChRef, EQL) ->
	{ExQRef,{PRef,ChList,Q}} = getExQByRef(ExQRef, EQL),
	replaceEQLbyKey(ExQRef, EQL, {ExQRef,{PRef,lists:delete(ChRef, ChList),Q}}).

rmExQByRefFromSTA(CExQ, {EQL,IN,OUT,EST}) -> {proplists:delete(CExQ, EQL),IN,OUT,EST}.
rmExQByRef(CExQ, EQL) -> proplists:delete(CExQ, EQL).

addExQ2EQLinSTA(ExQ, {EQL,IN,OUT,EST}) -> {[ExQ|EQL],IN,OUT,EST}.

replaceEQLbyKeyInSTA(Key, {EQL,IN,OUT,EST}, NewExQ) -> {replaceEQLbyKey(Key, EQL, NewExQ),IN,OUT,EST}.

replaceEQLbyKey(Key, EQL, NewExQ) ->
	case proplists:is_defined(Key, EQL) of
		true -> [NewExQ|del(Key, EQL)];
		false -> fail(["Not found in ExQList", Key])
	end.


fail([]) -> io:format("~n", []), exit('FAIL');
fail([M|L]) -> io:format("~p ", [M]), fail(L).