-module('erlz_curried').

%% API exports
-export([
    curried/1
]).


-spec currying(function(), list(), integer()) -> function() | any().
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
-spec curried(function()) -> function().
curried(Fn) ->
    {arity, Arity} = erlang:fun_info(Fn, arity),
    currying(Fn, [], Arity).