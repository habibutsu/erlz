-module(maybe_tests).
-include_lib("eunit/include/eunit.hrl").

-import(erlz_monad_maybe, [return/1, fail/1, '>>='/2, fmap/2]).

monad_maybe_test() ->
    ?assertEqual({just, 5}, return(5)),
    ?assertEqual(nothing, fail(5)),
    F = fun
            (V) when V < 5 -> nothing;
            (V) -> {just, V + 5}
        end,
    ?assertEqual({just, 10}, '>>='({just, 5}, F)),
    ?assertEqual(nothing, '>>='({just, 1}, F)),
    ?assertEqual(nothing, '>>='(nothing, F)),

    ?assertThrow({bad_value, 5}, '>>='(5, F)),
    ok.


functor_maybe_test() ->
    F = fun(V) -> V + 5 end,
    ?assertEqual({just, 10}, fmap(F, {just, 5})),
    ?assertEqual(nothing, fmap(F, nothing)),
    ok.


maybe_do1_test() ->
    ?assertEqual({just, <<"The answer is 42">>},
        erlz:maybe_do(42, [
            fun(V) -> {just, integer_to_binary(V)} end,
            fun(Bin) -> {just, <<"The answer is ", Bin/binary>>} end
        ])
    ),
    ?assertEqual(nothing,
        erlz:maybe_do(42, [
            fun(V) -> {just, integer_to_binary(V)} end,
            fun(_Bin) -> nothing end,
            fun(Bin) -> {just, <<"The answer is ", Bin/binary>>} end
        ])
    ),
    ok.


maybe_do2_test() ->
    Fns = [
        fun(V) -> {just, integer_to_binary(V)} end,
        fun
            (Bin) when byte_size(Bin) < 2 -> nothing;
            (Bin) -> {just, Bin}
        end,
        fun(Bin) -> {just, <<"The answer is ", Bin/binary>>} end
    ],
    ?assertEqual({just, <<"The answer is 42">>}, erlz:maybe_do(42, Fns)),
    ?assertEqual(nothing, erlz:maybe_do(1, Fns)),
    ok.


maybe_do3_test() ->
    ?assertEqual({just, <<"The answer is 42">>},
        erlz:maybe_do([
            fun() -> {just, 42} end,
            fun(V) -> {just, integer_to_binary(V)} end,
            fun(Bin) -> {just, <<"The answer is ", Bin/binary>>} end
        ])
    ),
    ok.


maybe_traverse_test() ->
    Fn = fun
             (V) when V < 5 -> {just, V * 2};
             (_) -> nothing
         end,
    ?assertEqual({just, [2,4,6,8]}, erlz:maybe_traverse(Fn, [1,2,3,4])),
    ?assertEqual(nothing, erlz:maybe_traverse(Fn, [1,2,6,3,4])),
    ok.


maybe_foldM_test() ->
    Fn = fun
             (Char, Str) when Char > $Z -> {just, [Char | Str]};
             (_, _) -> nothing
         end,
    ?assertEqual({just, "dcba"}, erlz:maybe_foldlM(Fn, "", "abcd")),
    ?assertEqual({just, "abcd"}, erlz:maybe_foldrM(Fn, "", "abcd")),
    ?assertEqual(nothing, erlz:maybe_foldlM(Fn, "", "aBc")),
    ?assertEqual(nothing, erlz:maybe_foldrM(Fn, "", "abC")),
    ok.
