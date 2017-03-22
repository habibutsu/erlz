-module(erlz_curried).

-export([curried/1]).


%%% Module API

-spec curried(function()) -> function().
curried(Fn) ->
    {arity, Arity} = erlang:fun_info(Fn, arity),
    currying(Fn, [], Arity).


%%% Inner functions

-spec currying(function(), list(), integer()) -> function() | any().
currying(Fn, Args, Arity) when length(Args) == Arity ->
    erlang:apply(Fn, Args);
currying(Fn, Args, Arity) ->
    fun(Arg) ->
        currying(Fn, Args ++ [Arg], Arity)
    end.

