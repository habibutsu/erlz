-module(either_tests).
-include_lib("eunit/include/eunit.hrl").

-import(erlz_monad_either, [return/1, '>>='/2, fmap/2]).

monad_either_test() ->
    ?assertEqual({right, 5}, return(5)),

    F = fun
            (V) when V < 5 -> {left, "less than 5"};
            (V) -> {right, V + 5}
        end,
    ?assertEqual({right, 10}, '>>='({right, 5}, F)),
    ?assertEqual({left, "less than 5"}, '>>='({right, 1}, F)),
    ?assertEqual({left, "some"}, '>>='({left, "some"}, F)),

    ?assertThrow({bad_value, 5}, '>>='(5, F)),
    ok.


functor_either_test() ->
    F = fun(V) -> V + 5 end,
    ?assertEqual({right, 10}, fmap(F, {right, 5})),
    ?assertEqual({left, "some"}, fmap(F, {left, "some"})),
    ok.


either_do1_test() ->
    ?assertEqual({left,{stop,1}},
        erlz:either_do(0, [
            fun(X) -> {right, X + 1} end,
            fun(X) -> {left, {stop, X}} end,
            fun(X) -> {right, X + 100} end
        ])
    ),
    ok.


either_do2_test() ->
    ?assertEqual({right, <<"The answer is 42">>},
        erlz:either_do(42, [
            fun(V) -> {right, integer_to_binary(V)} end,
            fun(Bin) -> {right, <<"The answer is ", Bin/binary>>} end
        ])
    ),
    ?assertEqual({left, <<"Unknown">>},
        erlz:either_do(42, [
            fun(V) -> {right, integer_to_binary(V)} end,
            fun(_Bin) -> {left, <<"Unknown">>} end,
            fun(Bin) -> {right, <<"The answer is ", Bin/binary>>} end
        ])
    ),
    ok.


either_do3_test() ->
    Fns = [
        fun(V) -> {right, integer_to_binary(V)} end,
        fun
            (Bin) when byte_size(Bin) < 2 -> {left, ""};
            (Bin) -> {right, Bin}
        end,
        fun(Bin) -> {right, <<"The answer is ", Bin/binary>>} end
    ],
    ?assertEqual({right, <<"The answer is 42">>}, erlz:either_do(42, Fns)),
    ?assertEqual({left, ""}, erlz:either_do(1, Fns)),
    ok.


either_do4_test() ->
    ?assertEqual({right, <<"The answer is 42">>},
        erlz:either_do([
            fun() -> {right, 42} end,
            fun(V) -> {right, integer_to_binary(V)} end,
            fun(Bin) -> {right, <<"The answer is ", Bin/binary>>} end
        ])
    ),
    ok.


either_traverse_test() ->
    Fn = fun
             (V) when V < 5 -> {right, V * 2};
             (_) -> {left, "more than 5"}
         end,
    ?assertEqual({right, [2,4,6,8]}, erlz:either_traverse(Fn, [1,2,3,4])),
    ?assertEqual({left, "more than 5"}, erlz:either_traverse(Fn, [1,2,6,3,4])),
    ok.


either_foldM1_test() ->
    Fn = fun
             (X, Sum) ->
                 case X > 5 of
                     true -> {left, "Contains value greater than 5"};
                     _ -> {right, Sum + X}
                 end
         end,
    ?assertEqual({right, 3}, erlz:either_foldlM(Fn, 0, [1, 1, 1])),
    ?assertEqual({left, "Contains value greater than 5"},
        erlz:either_foldlM(Fn, 0, [6, 1, 1])),

    ?assertEqual({right, 6},
        erlz:either_foldlM(
            erlz:partial(fun(X, Sum, C) -> {right, Sum + C*X} end, ['_', '_', 2]),
            0, [1,1,1])
    ),
    ok.


either_foldM2_test() ->
    Fn = fun
             (Char, Str) when Char > $Z -> {right, [Char | Str]};
             (_, _) -> {left, "uppercase char"}
         end,
    ?assertEqual({right, "dcba"}, erlz:either_foldlM(Fn, "", "abcd")),
    ?assertEqual({right, "abcd"}, erlz:either_foldrM(Fn, "", "abcd")),
    ?assertEqual({left, "uppercase char"}, erlz:either_foldlM(Fn, "", "aBc")),
    ?assertEqual({left, "uppercase char"}, erlz:either_foldrM(Fn, "", "abC")),
    ok.
