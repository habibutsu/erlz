-module('erlz_partial').

%% API exports
-export([
    partial/2
]).

%%====================================================================
%% API functions
%%====================================================================

-spec partial(function(), list()) -> function().
partial(Fn, InArgs) ->
    {arity, Arity} = erlang:fun_info(Fn, arity),
    AppArgs = lists:filter(fun(A) -> A =/= '_' end, InArgs),
    PartialArity = Arity - length(AppArgs),
    make_lamda(PartialArity, InArgs, Fn).

%%====================================================================
%% Internal functions
%%====================================================================

-spec make_lamda(non_neg_integer(), list(), function()) -> function().
make_lamda(0, AppArgs, Fn) ->
    fun() ->
        erlang:apply(Fn, AppArgs)
    end;
make_lamda(1, AppArgs, Fn) ->
    fun(A1) ->
        Args = merge_partial_args(AppArgs, [A1], []),
        erlang:apply(Fn, Args)
    end;
make_lamda(2, AppArgs, Fn) ->
    fun(A1,A2) ->
        Args = merge_partial_args(AppArgs, [A1, A2], []),
        erlang:apply(Fn, Args)
    end;
make_lamda(3, AppArgs, Fn) ->
    fun(A1,A2,A3) ->
        Args = merge_partial_args(AppArgs, [A1, A2, A3], []),
        erlang:apply(Fn, Args)
    end;
make_lamda(4, AppArgs, Fn) ->
    fun(A1,A2,A3,A4) ->
        Args = merge_partial_args(AppArgs, [A1, A2, A3, A4], []),
        erlang:apply(Fn, Args)
    end;
make_lamda(5, AppArgs, Fn) ->
    fun(A1,A2,A3,A4,A5) ->
        Args = merge_partial_args(AppArgs, [A1, A2, A3, A4, A5], []),
        erlang:apply(Fn, Args)
    end;
make_lamda(6, AppArgs, Fn) ->
    fun(A1,A2,A3,A4,A5,A6) ->
        Args = merge_partial_args(AppArgs, [A1, A2, A3, A4, A5, A6], []),
        erlang:apply(Fn, Args)
    end;
make_lamda(7, AppArgs, Fn) ->
    fun(A1,A2,A3,A4,A5,A6,A7) ->
        Args = merge_partial_args(AppArgs, [A1, A2, A3, A4, A5, A6, A7], []),
        erlang:apply(Fn, Args)
    end;
make_lamda(Arity, AppArgs, Fn) ->
    throw({not_implemented, {Fn, Arity, AppArgs}}).


-spec merge_partial_args(list(), list(), list()) -> list().
merge_partial_args([], [], Acc) ->
    Acc;
merge_partial_args([], Args, Acc) ->
    Acc ++ Args;
merge_partial_args(AppArgs, [], Acc) ->
    Acc ++ AppArgs;
merge_partial_args(['_' | AppArgs], [Arg | Args], Acc) ->
    merge_partial_args(AppArgs, Args, Acc ++ [Arg]);
merge_partial_args([Arg| AppArgs], Args, Acc) ->
    merge_partial_args(AppArgs, Args, Acc ++ [Arg]).
