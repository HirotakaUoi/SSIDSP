-module(double). 
-compile(export_all).

% This is a comment.
% Everything on a line after % is ignored.
start() -> double(20).

double(Value) -> times(Value, 2).
times(X,Y) -> temp(), X*Y.

test(X) ->
	try double(X) of
		Val -> {normal, Val}
	catch
		throw:{aa,Error} -> {throw,Error};
		throw:Error -> {throw,Error}
	end.

temp() -> throw({return,{int,40},
                        {[],[],
                         [[{{var,a},{int,2}}],
                          [{{func,abc,1},
                            {int,
                                [{int,{var,a}}],
                                {blk,[],[{return,{mul,{int,20},{var,a}}}]}}}],

                          []]}}).

t2() ->
  if
    lists:member(10,[10]) ->
      double(20);
    true -> 50
  end.