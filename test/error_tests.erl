-module(error_tests).
-include_lib("eunit/include/eunit.hrl").

%% -import(erlz_monad_error, [return/1, '>>='/2, fmap/2]).

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
