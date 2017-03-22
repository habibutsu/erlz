-module(error_tests).
-include_lib("eunit/include/eunit.hrl").

-import(erlz_monad_error, [return/1, '>>='/2, fmap/2]).

monad_error_test() ->
    ?assertEqual({ok, 5}, return(5)),

    F = fun
            (V) when V < 5 -> {error, "less than 5"};
            (V) -> {ok, V + 5}
        end,
    ?assertEqual({ok, 10}, '>>='({ok, 5}, F)),
    ?assertEqual({error, "less than 5"}, '>>='({ok, 1}, F)),
    ?assertEqual({error, "some"}, '>>='({error, "some"}, F)),

    ?assertThrow({bad_value, 5}, '>>='(5, F)),
    ok.


functor_error_test() ->
    F = fun(V) -> V + 5 end,
    ?assertEqual({ok, 10}, fmap(F, {ok, 5})),
    ?assertEqual({error, "some"}, fmap(F, {error, "some"})),
    ok.


error_do1_test() ->
    ?assertEqual({ok, [1,2,3]},
        erlz:error_do([
            fun() -> {ok, []} end,
            fun([] = State) -> {ok, State ++ [1]} end,
            fun([1] = State) -> {ok, State ++ [2]} end,
            fun([1,2] = State) -> {ok, State ++ [3]} end
        ])
    ),
    ?assertEqual({error, "reason"},
        erlz:error_do([], [
            fun([] = State) -> {ok, State ++ [1]} end,
            fun([1] = _State) -> {error, "reason"} end,
            fun([1,2] = State) -> {ok, State ++ [3]} end
        ])
    ),
    ok.


error_do2_test() ->
    ?assertEqual({ok, <<"The answer is 42">>},
        erlz:error_do(42, [
            fun(V) -> {ok, integer_to_binary(V)} end,
            fun(Bin) -> {ok, <<"The answer is ", Bin/binary>>} end
        ])
    ),
    ?assertEqual({error, <<"Unknown">>},
        erlz:error_do(42, [
            fun(V) -> {ok, integer_to_binary(V)} end,
            fun(_Bin) -> {error, <<"Unknown">>} end,
            fun(Bin) -> {ok, <<"The answer is ", Bin/binary>>} end
        ])
    ),
    ok.


error_do3_test() ->
    Fns = [
        fun(V) -> {ok, integer_to_binary(V)} end,
        fun
            (Bin) when byte_size(Bin) < 2 -> {error, "very small"};
            (Bin) -> {ok, Bin}
        end,
        fun(Bin) -> {ok, <<"The answer is ", Bin/binary>>} end
    ],
    ?assertEqual({ok, <<"The answer is 42">>}, erlz:error_do(42, Fns)),
    ?assertEqual({error, "very small"}, erlz:error_do(1, Fns)),
    ok.


error_do4_test() ->
    ?assertEqual({ok, <<"The answer is 42">>},
        erlz:error_do([
            fun() -> {ok, 42} end,
            fun(V) -> {ok, integer_to_binary(V)} end,
            fun(Bin) -> {ok, <<"The answer is ", Bin/binary>>} end
        ])
    ),
    ok.


error_traverse_test() ->
    Fn = fun
             (Item) ->
                 case Item > 5 of
                     true -> {error, "Contains value greater than 5"};
                     _ -> {ok, Item * 2}
                 end
         end,
    ?assertEqual({ok,[2,4,6,8]},
        erlz:error_traverse(Fn, [1,2,3,4])),
    ?assertEqual({error, "Contains value greater than 5"},
        erlz:error_traverse(Fn, [6,1,2,3,4])),
    ok.


error_foldM_test() ->
    Fn = fun
             (Char, Str) when Char > $Z -> {ok, [Char | Str]};
             (_, _) -> {error, "uppercase char"}
         end,
    ?assertEqual({ok, "dcba"}, erlz:error_foldlM(Fn, "", "abcd")),
    ?assertEqual({ok, "abcd"}, erlz:error_foldrM(Fn, "", "abcd")),
    ?assertEqual({error, "uppercase char"}, erlz:error_foldlM(Fn, "", "aBc")),
    ?assertEqual({error, "uppercase char"}, erlz:error_foldrM(Fn, "", "abC")),
    ok.
