-module(ssidsp4).
-compile(export_all).

sem({P,IN}) -> sem(P, {IN,[],[]}).

sem([], STA) -> STA;
sem([S|SS], STA) -> STA1 = sem(S, STA), sem(SS, STA1);
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
sem({dim,{ary,V,L}}, STA) -> semAry({V,L}, STA);
sem({procCall,N,PL}, STA) -> procCall(N, PL, STA);
sem({def,V,E}, STA) -> 
	{Val,STA1} = evalExp(E, STA),
	semDef(V, Val, STA1);
sem(S, _) -> fail(["Illegal Statement", S]).

semDef({var,V}, Val, {IN,OUT,Env}) -> 
	case chkDef({var,V}, Env) of
		true -> {IN,OUT,[{{var,V},Val}|del({var,V}, Env)]};
		false -> {IN,OUT,[{{var,V},Val}|Env]}
	end;
semDef({ary,V,[E]}, Val, {IN,OUT,Env}) -> 
	{Ind,{IN1,OUT1,Env1}} = evalExp(E, {IN,OUT,Env}),
	Ary = {ary,V,1},
	{[MaxAry],ValAry} = case chkDef(Ary, Env1) of
		true -> env(Ary, Env1);
		false -> fail(["Array Not Declared", Ary])
	end,
	NewValAry = case (Ind < MaxAry) and (Ind >= 0) of
		true  -> replaceNth(Ind, Val, ValAry);
		false -> fail(["Index out of range", {Ary,Ind}])
	end,
	{IN1,OUT1,[{Ary,{[MaxAry],NewValAry}}|del(Ary, Env1)]};
semDef(Var, Val, _) -> fail(["Illegal Assignment", {Var,Val}]).

evalExp({int,Num}, STA) -> {Num,STA};
evalExp({var,V}, {IN,OUT,Env}) -> 
	case chkDef({var,V}, Env) of
		true -> {env({var,V}, Env),{IN,OUT,Env}};
		false -> fail(["Not defined", {var,V}])
	end;
evalExp({ary,V,[E]}, STA) -> 
	{Ind,{IN1,OUT1,Env1}=STA1} = evalExp(E, STA),
	Ary = {ary,V,1},
	{[MaxAry],ValAry} = case chkDef(Ary, Env1) of
		true -> env(Ary, Env1);
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
writeStream([{string,S}|L], {IN,OUT,Env}) -> writeStream(L, {IN,[list_to_atom(S)|OUT],Env});
writeStream([E|L], STA) -> {Val,{IN1,OUT1,Env1}} = evalExp(E, STA), writeStream(L, {IN1,[Val|OUT1],Env1}).

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

semAry({V,[E]}, STA) ->
	{Val,{IN1,OUT1,Env1}} = evalExp(E, STA),
	case is_integer(Val) of
		true -> Val;
		false -> fail(["Illegal Maxbound!! in dim", {V,Val}])
	end,
	Ary = {{ary,V,1},{[Val],makeAry(Val,0)}},
	case chkDef({ary,V,1}, Env1) of
		true -> fail(["Already Declared", {ary,V,1}]);
		false -> {IN1,OUT1,[Ary|Env1]}
	end;
semAry(Ary, _) -> fail(["Illegal Dimensions!! in dim", Ary]).

makeAry(0, _) -> [];
makeAry(N, Default) -> [Default|makeAry(N-1, Default)].

chkDef(V, Env) -> proplists:is_defined(V, Env).

del(V, Env) -> proplists:delete(V, Env).

env(V, Env) -> proplists:get_value(V, Env).

replaceNth(0, V, [_|L]) -> [V|L];
replaceNth(N, V, [T|L]) -> [T|replaceNth(N-1, V, L)].


fail([]) -> io:format("~n", []), exit('FAIL');
fail([M|L]) -> io:format("~p ", [M]), fail(L).