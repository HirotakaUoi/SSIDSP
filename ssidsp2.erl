-module(ssidsp2).
-compile(export_all).

sem({P,_}) -> sem(P, []);
sem(P) -> sem(P, []).

sem([], Env) -> Env;
sem([S|SS], Env) -> Env1 = sem(S, Env), sem(SS, Env1);
sem({iff,E,S}, Env) ->
	Val = evalExp(E, Env),
	case Val of
		0 -> Env;
		_ -> sem(S, Env)
	end;
sem({ifelse,E,S1,S2}, Env) ->
	Val = evalExp(E, Env),
	case Val of
		0 -> sem(S2, Env);
		_ -> sem(S1, Env)
	end;
sem({while,E,S}, Env) ->
	Val = evalExp(E, Env),
	case Val of
		0 -> Env;
		_ -> sem({while,E,S}, sem(S, Env))
	end;
sem({def,V,E}, Env) -> 
	Val = evalExp(E, Env),
	case chkDef(V, Env) of
		true -> [{V,Val}|del(V, Env)];
		false -> [{V,Val}|Env]
	end.

evalExp({int,Num}, _) -> Num;
evalExp({var,V}, Env) -> 
	case chkDef({var,V}, Env) of
		true -> env({var,V}, Env);
		false -> fail(["Not defined", {var,V}])
	end;
evalExp({add,E1,E2}, Env) -> evalExp(E1, Env) + evalExp(E2, Env);
evalExp({sub,E1,E2}, Env) -> evalExp(E1, Env) - evalExp(E2, Env);
evalExp({mul,E1,E2}, Env) -> evalExp(E1, Env) * evalExp(E2, Env);
evalExp({divd,E1,E2}, Env) -> evalExp(E1, Env) div evalExp(E2, Env);
evalExp({remm,E1,E2}, Env) -> evalExp(E1, Env) rem evalExp(E2, Env);
evalExp({eq,E1,E2}, Env) -> eq(evalExp(E1, Env), evalExp(E2, Env));
evalExp({neq,E1,E2}, Env) -> neq(evalExp(E1, Env), evalExp(E2, Env));
evalExp({gt,E1,E2}, Env) -> gt(evalExp(E1, Env), evalExp(E2, Env));
evalExp({gte,E1,E2}, Env) -> gte(evalExp(E1, Env), evalExp(E2, Env));
evalExp({lt,E1,E2}, Env) -> lt(evalExp(E1, Env), evalExp(E2, Env));
evalExp({lte,E1,E2}, Env) -> lte(evalExp(E1, Env), evalExp(E2, Env));
evalExp(E, _) -> fail(["Bad Expression", E]).

eq(V1, V2) -> case V1 == V2 of true -> 1; false -> 0 end.
neq(V1, V2) -> case V1 /= V2 of true -> 1; false -> 0 end.
gt(V1, V2) -> case V1 > V2 of true -> 1; false -> 0 end.
gte(V1, V2) -> case V1 >= V2 of true -> 1; false -> 0 end.
lt(V1, V2) -> case V1 < V2 of true -> 1; false -> 0 end.
lte(V1, V2) -> case V1 =< V2 of true -> 1; false -> 0 end.

chkDef(V, L) -> proplists:is_defined(V, L).

del(V, L) -> proplists:delete(V, L).

env(V,L) -> proplists:get_value(V, L).

fail([]) -> io:format("~n", []), exit('FAIL');
fail([M|L]) -> io:format("~p ", [M]), fail(L).