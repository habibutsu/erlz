-module(erlz).
-include("erlz.hrl").

-export([
    partial/2, curried/1,
    do/1, do/2,
    maybe_do/1, maybe_do/2,
    maybe_traverse/2, maybe_foldlM/3, maybe_foldrM/3,
    either_do/1, either_do/2,
    either_traverse/2, either_foldlM/3, either_foldrM/3,
    error_do/1, error_do/2,
    error_traverse/2, error_foldlM/3, error_foldrM/3
]).


%%% Module API

-spec partial(function(), list()) -> function().
partial(Fn, Args) ->
    erlz_partial:partial(Fn, Args).


-spec curried(function()) -> function().
curried(Fn) ->
    erlz_curried:curried(Fn).


-spec do([function()]) -> any().
do(Pipeline) ->
    do(undefined, Pipeline).


-spec do(any(), [function()]) -> any().
do(InitValue, Pipeline) ->
    i_do(InitValue, fun i_fbind/2, Pipeline).


-spec maybe_do([fun_maybe()]) -> maybe().
maybe_do([Fn|Fns]) ->
    i_maybe_do(Fn(), Fns).


-spec maybe_do(any(), [fun_maybe()]) -> maybe().
maybe_do(InitValue, Fns) ->
    i_maybe_do(erlz_monad_maybe:return(InitValue), Fns).


-spec maybe_traverse(fun_maybe(), list()) -> maybe().
maybe_traverse(Fn, Items) when is_list(Items) ->
    i_list_traverse(erlz_monad_maybe, Fn, Items).


-spec maybe_foldlM(function(), any(), [maybe()]) -> maybe().
maybe_foldlM(Fn, Acc, Xs) ->
    i_foldM(erlz_monad_maybe, fun lists:foldl/3, Fn, Acc, Xs).


-spec maybe_foldrM(function(), any(), [maybe()]) -> maybe().
maybe_foldrM(Fn, Acc, Xs) ->
    i_foldM(erlz_monad_error, fun lists:foldr/3, Fn, Acc, Xs).


either_do([Fn|Fns]) ->
    i_either_do(Fn(), Fns).


either_do(InitValue, Fns) ->
    i_either_do(erlz_monad_either:return(InitValue), Fns).


either_traverse(Fn, Items) when is_list(Items) ->
    i_list_traverse(erlz_monad_either, Fn, Items).


either_foldlM(Fn, Acc, Xs) ->
    i_foldM(erlz_monad_either, fun lists:foldl/3, Fn, Acc, Xs).


either_foldrM(Fn, Acc, Xs) ->
    i_foldM(erlz_monad_either, fun lists:foldr/3, Fn, Acc, Xs).


error_do([Fn|Fns]) ->
    i_error_do(Fn(), Fns).


error_do(InitValue, Fns) ->
    i_error_do(erlz_monad_error:return(InitValue), Fns).


error_traverse(Fn, Items) when is_list(Items) ->
    i_list_traverse(erlz_monad_error, Fn, Items).


error_foldlM(Fn, Acc, Xs) ->
    i_foldM(erlz_monad_error, fun lists:foldl/3, Fn, Acc, Xs).


error_foldrM(Fn, Acc, Xs) ->
    i_foldM(erlz_monad_error, fun lists:foldr/3, Fn, Acc, Xs).



%%% Inner functions

-spec i_do(any(), function(), [function()]) -> any().
i_do(undefined, BindFn, [F|Fns]) ->
    i_do(F(), BindFn, Fns);
i_do(InitValue, BindFn, Fns) ->
    lists:foldl(
        fun(Fn, State) ->
            BindFn(State, Fn)
        end,
        InitValue, Fns).


-spec i_fbind(any(), function()) -> any().
i_fbind(V, Fn) ->
    Fn(V).


-spec i_list_traverse(module(), function(), list()) -> any().
i_list_traverse(Monad, Fn, Items) when is_list(Items) ->
    lists:foldl(
        fun(X, Acc) ->
            Monad:'>>='(Acc,
                fun(Ys) ->
                    Monad:fmap(
                        fun(Y)-> Ys ++ [Y] end,
                        Fn(X))
                end)
        end,
        Monad:return([]), Items).


-spec i_foldM(atom(), function(), function(), any(), list()) -> any().
i_foldM(Monad, FoldFn, Fn, InAcc, Xs) ->
    FoldFn(
        fun(X, MAcc) ->
            Monad:'>>='(
                MAcc,
                fun(Acc) ->
                    Fn(X, Acc)
                end)
        end,
        Monad:return(InAcc),
        Xs).


-spec i_maybe_do(maybe(), [fun_maybe()]) -> maybe().
i_maybe_do(InitValue, Fns) ->
    i_do(InitValue, fun erlz_monad_maybe:'>>='/2, Fns).


i_either_do(InitValue, Fns) ->
    i_do(InitValue, fun erlz_monad_either:'>>='/2, Fns).


i_error_do(InitValue, Fns) ->
    i_do(InitValue, fun erlz_monad_error:'>>='/2, Fns).
