-module(ssidsp1).
-compile(export_all).

sem({P,_}) -> sem(P, []);
sem(P) -> sem(P, []).

sem({blk,_,L}, Env) -> sem(L, Env);
sem([], Env) -> Env;
sem([S|SS], Env) -> Env1 = sem(S, Env), sem(SS, Env1);
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
evalExp(E, _) -> fail(["Bad Expression", E]).

% chkDef(V, L) -> proplists:is_defined(V, L).
chkDef(_, []) -> false;
chkDef({var,N}, [{{var,N},_}|L]) -> true;
chkDef(V, [_|L]) -> chkDef(V, L).

% del(V, L) -> proplists:delete(V, L).
del(_, []) -> [];
del({var,N}, [{{var,N},_}|L]) -> L;
del(V, [B|L]) -> [B|del(V, L)].

% env(V,L) -> proplists:get_value(V, L).
env({var,N}, [{{var,N},V}|L]) -> V;
env(V, [_|L]) -> val(V, L).

fail([]) -> io:format("~n", []), exit('FAIL');
fail([M|L]) -> io:format("~p ", [M]), fail(L).
