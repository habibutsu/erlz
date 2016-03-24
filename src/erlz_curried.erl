-module('erlz_curried').

%% API exports
-export([
    curried/1
]).


currying(Fn, [A1], 1) ->
    Fn(A1);
currying(Fn, [A1,A2], 2) ->
    Fn(A1,A2);
currying(Fn, [A1,A2,A3], 3) ->
    Fn(A1,A2,A3);
currying(Fn, [A1,A2,A3,A4], 4) ->
    Fn(A1,A2,A3,A4);
currying(Fn, [A1,A2,A3,A4,A5], 5) ->
    Fn(A1,A2,A3,A4,A5);
currying(Fn, Args, Arity) when length(Args) == Arity ->
    erlang:apply(Fn, Args);
currying(Fn, Args, Arity) ->
    fun(Arg) ->
        currying(Fn, Args ++ [Arg], Arity)
    end.

% % Example of currying
% Fn = fun(A, B, C, D) ->
%     A + B + C + D
% end,
% CFn = pipeline:make_curried_fun(Fn),
% (((CFn(1))(2))(3))(4).
curried(Fn) ->
    {arity, Arity} = erlang:fun_info(Fn, arity),
    currying(Fn, [], Arity).