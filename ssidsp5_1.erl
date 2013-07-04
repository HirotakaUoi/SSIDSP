-module(ssidsp5_1).
-compile(export_all).

sem({P,IN}) -> sem(P, {IN,[],[[]]}).

sem([], STA) -> STA;
sem([S|SS], STA) -> STA1 = sem(S, STA), sem(SS, STA1);
sem({blk,DL,S}, {IN,OUT,EST}) ->
	STA2 = decVars(DL, {IN,OUT,[[]|EST]}),
	{IN2,OUT2,[_|EST2]} = sem(S, STA2),
	{IN2,OUT2,EST2};
sem({iff,E,S}, STA) ->
	{Val,STA1} = evalExp(E, STA),
	case Val of
		0 -> STA1;
		_ -> sem(S, STA1)
	end;
sem({ifelse,E,S1,S2}, STA) ->
	{Val,STA1} = evalExp(E, STA),
	case Val of
		0 -> sem(S2, STA1);
		_ -> sem(S1, STA1)
	end;
sem({while,E,S}, STA) ->
	{Val,STA1} = evalExp(E, STA),
	case Val of
		0 -> STA1;
		_ -> sem({while,E,S}, sem(S, STA1))
	end;
sem({for,S1,E,S2,S3}, STA) -> sem([S1,{while,E,[S3,S2]}], STA);
% sem({dim,{ary,V,L}}, STA) -> semAry({V,L}, STA);
sem({dec,{type,int},IntDecL}, STA) -> semIntDecL(IntDecL, STA);
sem({procCall,N,PL}, STA) -> procCall(N, PL, STA);
sem({def,V,E}, STA) -> 
	{Val,STA1} = evalExp(E, STA),
	semDef(V, Val, STA1);
sem(S, _) -> fail(["Illegal Statement", S]).

semDef({var,V}, Val, {IN,OUT,EST}) -> 
	NewEST = case chkDefESTbyKey({var,V}, EST) of
		true -> {_,Env}  = envESTbyKey({var,V}, EST),
			replaceESTbyKey({var,V}, EST, [{{var,V},Val}|del({var,V}, Env)]);
%		false -> fail(["Not Declared", {var,V}])
		false -> [Env1|EST1] = EST,
			[[{{var,V},Val}|Env1]|EST1]
	end,
	{IN,OUT,NewEST};
semDef({ary,V,[E]}, Val, STA) -> 
	{Ind,{IN1,OUT1,EST1}} = evalExp(E, STA),
	Ary = {ary,V,1},
	{{[MaxAry],ValAry},Env} = case chkDefESTbyKey(Ary, EST1) of
		true -> envESTbyKey(Ary, EST1);
		false -> fail(["Array Not Declared", {V,1}])
	end,
	NewValAry = case (Ind < MaxAry) and (Ind >= 0) of
		true  -> replaceNth(Ind, Val, ValAry);
		false -> fail(["Index out of range", {V,1,Ind}])
	end,
	NewEST = replaceESTbyKey(Ary, EST1, [{Ary,{[MaxAry],NewValAry}}|del(Ary, Env)]),
	{IN1,OUT1,NewEST};
semDef(Var, Val, _) -> fail(["Illegal Assignment", {Var,Val}]).

semIntDecL([], STA) -> STA;
semIntDecL([IntDec|L], STA) -> semIntDecL(L, semIntDec(IntDec, STA)).

semIntDec({init,{def,{var,V},E}}, STA) -> 
	{Val,STA1} = evalExp(E, STA),
	STA2 = decVars([{var,V}], STA1),
	semDef({var,V}, Val, STA2);
semIntDec({init,{def,{ary,V,DM},E}}, STA) ->
	{Val,STA1} = evalExp(E, STA),
	decVars([{ary,V,DM,Val}], STA1);
semIntDec(Var, STA) -> decVars([Var], STA).

evalExp({int,Num}, STA) -> {Num,STA};
evalExp({var,V}, {_,_,EST}=STA) -> 
	{VarVal,_} = case chkDefESTbyKey({var,V}, EST) of
		true -> envESTbyKey({var,V}, EST);
		false -> fail(["Not defined", {var,V}])
	end,
	{VarVal,STA};
evalExp({ary,V,[E]}, STA) -> 
	{Ind,{_,_,EST1}=STA1} = evalExp(E, STA),
	Ary = {ary,V,1},
	{{[MaxAry],ValAry},_} = case chkDefESTbyKey(Ary, EST1) of
		true -> envESTbyKey(Ary, EST1);
		false -> fail(["Array Not Declared", {V,1}])
	end,
	Val = case (Ind < MaxAry) and (Ind >= 0) of
		true  -> lists:nth(Ind+1, ValAry);
		false -> fail(["Index out of range", {V,1,Ind}])
	end,
	{Val,STA1};
evalExp({add,E1,E2}, STA) -> {Num1,STA1} = evalExp(E1, STA), {Num2,STA2} = evalExp(E2, STA1), {Num1+Num2,STA2};
evalExp({sub,E1,E2}, STA) -> {Num1,STA1} = evalExp(E1, STA), {Num2,STA2} = evalExp(E2, STA1), {Num1-Num2,STA2};
evalExp({mul,E1,E2}, STA) -> {Num1,STA1} = evalExp(E1, STA), {Num2,STA2} = evalExp(E2, STA1), {Num1*Num2,STA2};
evalExp({divd,E1,E2}, STA) -> {Num1,STA1} = evalExp(E1, STA), {Num2,STA2} = evalExp(E2, STA1), {Num1 div Num2,STA2};
evalExp({remm,E1,E2}, STA) -> {Num1,STA1} = evalExp(E1, STA), {Num2,STA2} = evalExp(E2, STA1), {Num1 rem Num2,STA2};
evalExp({eq,E1,E2}, STA) -> {Num1,STA1} = evalExp(E1, STA), {Num2,STA2} = evalExp(E2, STA1), {eq(Num1, Num2),STA2};
evalExp({neq,E1,E2}, STA) -> {Num1,STA1} = evalExp(E1, STA), {Num2,STA2} = evalExp(E2, STA1), {neq(Num1, Num2),STA2};
evalExp({gt,E1,E2}, STA) -> {Num1,STA1} = evalExp(E1, STA), {Num2,STA2} = evalExp(E2, STA1), {gt(Num1, Num2),STA2};
evalExp({gte,E1,E2}, STA) -> {Num1,STA1} = evalExp(E1, STA), {Num2,STA2} = evalExp(E2, STA1), {gte(Num1, Num2),STA2};
evalExp({lt,E1,E2}, STA) -> {Num1,STA1} = evalExp(E1, STA), {Num2,STA2} = evalExp(E2, STA1), {lt(Num1, Num2),STA2};
evalExp({lte,E1,E2}, STA) -> {Num1,STA1} = evalExp(E1, STA), {Num2,STA2} = evalExp(E2, STA1), {lte(Num1, Num2),STA2};
evalExp({andd,E1,E2}, STA) -> {Num1,STA1} = evalExp(E1, STA), 
	case Num1==0 of
		true -> {0,STA1};
		false -> {Num2,STA2} = evalExp(E2, STA1), 
			case Num2==0 of
				true -> {0,STA2};
				false -> {1,STA2}
			end
	end;
evalExp({orr,E1,E2}, STA) -> {Num1,STA1} = evalExp(E1, STA), 
	case Num1==0 of
		true -> {Num2,STA2} = evalExp(E2, STA1), 
			case Num2==0 of
				true -> {0,STA2};
				false -> {1,STA2}
			end;
		false -> {1,STA1}
	end;
evalExp({nott,E}, STA) -> {Num,STA1} = evalExp(E, STA), 
	case Num==0 of
		true -> {1,STA1}; 
		false -> {0,STA1}
	end;
evalExp({funcCall,N,PL}, STA) -> funcCall(N, PL, STA);
evalExp(E, _) -> fail(["Bad Expression", E]).

eq(V1, V2) -> case V1 == V2 of true -> 1; false -> 0 end.
neq(V1, V2) -> case V1 /= V2 of true -> 1; false -> 0 end.
gt(V1, V2) -> case V1 > V2 of true -> 1; false -> 0 end.
gte(V1, V2) -> case V1 >= V2 of true -> 1; false -> 0 end.
lt(V1, V2) -> case V1 < V2 of true -> 1; false -> 0 end.
lte(V1, V2) -> case V1 =< V2 of true -> 1; false -> 0 end.

procCall(write, PL, STA) -> writeStream(PL, STA);
procCall(print, PL, STA) -> writeConsle(PL, STA);
procCall(printStatus, [], STA) -> printStatus(STA);
procCall(N, PL, _) -> fail(["Undefined Procedure", N, PL]).

writeStream([], STA) -> STA;
writeStream([{string,S}|L], {IN,OUT,EST}) -> writeStream(L, {IN,[list_to_atom(S)|OUT],EST});
writeStream([E|L], STA) -> {Val,{IN1,OUT1,EST1}} = evalExp(E, STA), writeStream(L, {IN1,[Val|OUT1],EST1}).


writeConsle([], STA) -> io:format("~n"), STA;
writeConsle([{string,S}|L], STA) -> io:format(S,[]), writeConsle(L, STA);
writeConsle([E|L], STA) -> {Val,STA1} = evalExp(E, STA), io:format("~p ",[format(Val)]), writeConsle(L, STA1).

format({int,N}) -> N;
format({var,V}) -> V;
format(Val) -> Val.

printStatus(STA) -> io:format("~p~n", [STA]), STA.

funcCall(read, [], STA) -> readStream(STA);
funcCall(N, PL, _) -> fail(["Undefined Function", N, PL]).

readStream({[],_,_}) -> fail(["Nothing to Read"]);
readStream({[T|IN],OUT,Env}) -> 
	case T of
		Num when is_integer(Num) -> {Num,{IN,OUT,Env}};
		_ -> fail(["Illigal Input Term", T])
	end.

decVars([], STA) -> STA;
decVars([{var,V}|DL], {IN,OUT,[Env|L]}) -> 
	case chkDef({var,V}, Env) of
		true -> fail(["Double defined in block!!",{var,V}]);
		false -> decVars(DL, {IN,OUT,[[{{var,V},0}|Env]|L]})
	end;
decVars([{ary,V,[E]}|DL], STA) -> 
	{Val,{IN1,OUT1,[Env1|L]}} = evalExp(E, STA),
	case is_integer(Val) of
		true -> Val;
		false -> fail(["Illegal Maxbound!! in dim", {V,Val}])
	end,
	Ary = {{ary,V,1},{[Val],makeAry(Val,0)}},
	case chkDef({ary,V,1}, Env1) of
		true -> fail(["Double defined in block!!", {ary,V,1}]);
		false -> decVars(DL, {IN1,OUT1,[[Ary|Env1]|L]})
	end;
decVars([{ary,V,[E],Default}|DL], STA) -> 
	{Val,{IN1,OUT1,[Env1|L]}} = evalExp(E, STA),
	case is_integer(Val) of
		true -> Val;
		false -> fail(["Illegal Maxbound!! in dim", {V,Val}])
	end,
	Ary = {{ary,V,1},{[Val],makeAry(Val,Default)}},
	case chkDef({ary,V,1}, Env1) of
		true -> fail(["Double defined in block!!", {ary,V,1}]);
		false -> decVars(DL, {IN1,OUT1,[[Ary|Env1]|L]})
	end.

semAry({V,[E]}, STA) ->
	{Val,{IN1,OUT1,[Env1|EST1]}} = evalExp(E, STA),
	case is_integer(Val) of
		true -> Val;
		false -> fail(["Illegal Maxbound!! in dim", {V,Val}])
	end,
	Ary = {{ary,V,1},{[Val],makeAry(Val,0)}},
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