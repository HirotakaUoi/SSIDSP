-module(ssidsp3).
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
sem({procCall,N,PL}, STA) -> procCall(N, PL, STA);
sem({def,V,E}, STA) -> 
	{Val,{IN1,OUT1,Env1}} = evalExp(E, STA),
	case chkDef(V, Env1) of
		true -> {IN1,OUT1,[{V,Val}|del(V, Env1)]};
		false -> {IN1,OUT1,[{V,Val}|Env1]}
	end.

evalExp({int,Num}, STA) -> {Num,STA};
evalExp({var,V}, {IN,OUT,Env}) -> 
	case chkDef({var,V}, Env) of
		true -> {env({var,V}, Env),{IN,OUT,Env}};
		false -> fail(["Not defined", {var,V}])
	end;
evalExp({add,E1,E2}, STA) -> {Num1,STA1} = evalExp(E1, STA), {Num2,STA2} = evalExp(E2, STA1), {Num1+Num2,STA2};
evalExp({sub,E1,E2}, STA) -> {Num1,STA1} = evalExp(E1, STA), {Num2,STA2} = evalExp(E2, STA1), {Num1-Num2,Env2};
evalExp({mul,E1,E2}, STA) -> {Num1,STA1} = evalExp(E1, STA), {Num2,STA2} = evalExp(E2, STA1), {Num1*Num2,Env2};
evalExp({divd,E1,E2}, STA) -> {Num1,STA1} = evalExp(E1, STA), {Num2,STA2} = evalExp(E2, STA1), {Num1 div Num2,Env2};
evalExp({remm,E1,E2}, STA) -> {Num1,STA1} = evalExp(E1, STA), {Num2,STA2} = evalExp(E2, STA1), {Num1 rem Num2,Env2};
evalExp({eq,E1,E2}, STA) -> {Num1,STA1} = evalExp(E1, STA), {Num2,STA2} = evalExp(E2, STA1), {eq(Num1, Num2),Env2};
evalExp({neq,E1,E2}, STA) -> {Num1,STA1} = evalExp(E1, STA), {Num2,STA2} = evalExp(E2, STA1), {neq(Num1, Num2),Env2};
evalExp({gt,E1,E2}, STA) -> {Num1,STA1} = evalExp(E1, STA), {Num2,STA2} = evalExp(E2, STA1), {gt(Num1, Num2),Env2};
evalExp({gte,E1,E2}, STA) -> {Num1,STA1} = evalExp(E1, STA), {Num2,STA2} = evalExp(E2, STA1), {gte(Num1, Num2),Env2};
evalExp({lt,E1,E2}, STA) -> {Num1,STA1} = evalExp(E1, STA), {Num2,STA2} = evalExp(E2, STA1), {lt(Num1, Num2),Env2};
evalExp({lte,E1,E2}, STA) -> {Num1,STA1} = evalExp(E1, STA), {Num2,STA2} = evalExp(E2, STA1), {lte(Num1, Num2),Env2};
evalExp({funcCall,N,PL}, STA) -> funcCall(N, PL, STA);
evalExp(E, _) -> fail(["Bad Expression", E]).

eq(V1, V2) -> case V1 == V2 of true -> 1; false -> 0 end.
neq(V1, V2) -> case V1 /= V2 of true -> 1; false -> 0 end.
gt(V1, V2) -> case V1 > V2 of true -> 1; false -> 0 end.
gte(V1, V2) -> case V1 >= V2 of true -> 1; false -> 0 end.
lt(V1, V2) -> case V1 < V2 of true -> 1; false -> 0 end.
lte(V1, V2) -> case V1 =< V2 of true -> 1; false -> 0 end.

procCall(write, PL, STA) -> writeStream(PL, STA);
procCall(N, PL, _) -> fail(["Undefined Procedure", N, PL]).

writeStream([], STA) -> STA;
writeStream([{string,S}|L], {IN,OUT,Env}) -> writeStream(L, {IN,[S|OUT],Env});
writeStream([E|L], STA) -> {Num,{IN1,OUT1,Env1}} = evalExp(E, STA), writeStream(L, {IN,[Num|OUT],Env1}).

funcCall(read, [], STA) -> readStream(STA);
funcCall(N, PL, _) -> fail(["Undefined Function", N, PL]).

readStream({[],_,_}) -> fail(["Nothing to Read"]);
readStream({[T|IN],OUT,Env}) -> 
	case T of
		Num when is_integer(Num) -> {Num,{IN,OUT,Env}};
		_ -> fail(["Illigal Input Term", T])
	end.

chkDef(V, L) -> proplists:is_defined(V, L).

del(V, L) -> proplists:delete(V, L).

env(V, L) -> proplists:get_value(V, L).

fail([]) -> io:format("~n", []), exit('FAIL');
fail([M|L]) -> io:format("~p ", [M]), fail(L).