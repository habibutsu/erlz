-module('erlz_tests').

-include_lib("eunit/include/eunit.hrl").

-compile([export_all]).

partial_test() ->
    Fn = fun(A, B, C, D, E) ->
        1 = A,
        2 = B,
        3 = C,
        4 = D,
        5 = E,
        A + B + C + D + E
    end,
    PFn = erlz:partial(Fn, [1, '_', '_', 4, 5]),
    {arity, 2} = erlang:fun_info(PFn, arity),
    15 = PFn(2, 3).

carried_test() ->
    Fn = fun(A, B, C, D) ->
        A + B + C + D
    end,
    CFn = erlz:curried(Fn),
    10 = (((CFn(1))(2))(3))(4).

do_test() ->
    <<"16.0">> = erlz:do(0, [
        fun(X) -> X + 1 end,
        fun(X) -> X + 3 end,
        (erlz:curried(fun math:pow/2))(2),
        erlz:partial(fun erlang:float_to_binary/2, ['_', [{decimals, 4}, compact]])
    ]).

monad_laws_test() ->
    lists:foreach(fun(MM) ->
        A = 1,
        M = MM:return(A),

        % Left identity:
        % return a >>= f ≡ f a
        Fn = fun(X) -> X end,
        ?assert((MM:'>>='(MM:return(A), Fn)) =:= Fn(A)),

        % Right identity:
        % m >>= return ≡ m
        ?assert(M =:= MM:'>>='(M, fun MM:return/1)),

        % Associativity
        % (m >>= f) >>= g ≡ m >>= (\x -> f x >>= g)
        F = fun(X) -> MM:return(X + 1) end,
        G = fun(X) -> MM:return(X + 10) end,
        ?assert(
            MM:'>>='(MM:'>>='(M, F), G) =:=
            MM:'>>='(M, fun(X) -> MM:'>>='(F(X), G) end))
    end, [erlz_monad_either, erlz_monad_error, erlz_monad_maybe]).


either_do_test() ->
    {left,{stop,1}} = erlz:either_do(0, [
        fun(X) -> {right, X+1} end,
        fun(X) -> {left, {stop, X}} end,
        fun(X) -> {right, X+100} end
    ]).

either_foldM_test() ->
    Fn = fun(X, Sum) ->
        case X > 5 of
            true -> {left, "Contains value greater than 5"};
            _ -> {right, Sum + X}
        end
    end,
    {right,3} = erlz:either_foldlM(Fn, 0, [1,1,1]),
    {left,"Contains value greater than 5"} = erlz:either_foldlM(Fn, 0, [6,1,1]),

    {right, 6} = erlz:either_foldlM(
        erlz:partial(fun(X, Sum, C) -> {right, Sum + C*X} end, ['_', '_', 2]),
        0, [1,1,1]),
    ok.

error_do_test() ->
    io:format("error_do_test~n"),
    {ok, [1,2,3]} = erlz:error_do([
        fun() -> {ok, []} end,
        fun([] = State) -> {ok, State ++ [1]} end,
        fun([1] = State) -> {ok, State ++ [2]} end,
        fun([1,2] = State) -> {ok, State ++ [3]} end
    ]),
    {error, "reason"} = erlz:error_do([], [
        fun([] = State) -> {ok, State ++ [1]} end,
        fun([1] = _State) -> {error, "reason"} end,
        fun([1,2] = State) -> {ok, State ++ [3]} end
    ]).

error_traverse_test() ->
    Fn = fun(Item) ->
        case Item > 5 of
            true -> {error, "Contains value greater than 5"};
            _ -> {ok, Item * 2}
        end
    end,
    {ok,[2,4,6,8]} = erlz:error_traverse(Fn, [1,2,3,4]),
    {error,"Contains value greater than 5"} = erlz:error_traverse(Fn, [6,1,2,3,4]),
    ok.
