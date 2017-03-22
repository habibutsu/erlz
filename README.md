# Overview

Set of helpers functions for advanced functional programming on Erlang:

* currying
* partial application
* monadic computation


## Currying

```erlang
Wrapper = erlz:curried(fun(Prefix, Suffix, Str) -> Prefix ++ Str ++ Suffix end),
ParenWrapper = (Wrapper("{"))("}"),
?assertEqual("{Hello}", ParenWrapper("Hello")),

CommentOpen = Wrapper("/* "),
CommentWrapper = CommentOpen(" */"),
?assertEqual("/* Hello */", CommentWrapper("Hello")),
```

## Partial application

```erlang
Wrapper = fun(Prefix, Suffix, Str) -> Prefix ++ Str ++ Suffix end,
HelloWrapper = erlz:partial(Wrapper, ['_', '_', "Hello"]),
?assertEqual("{Hello}", HelloWrapper("{", "}")),
?assertEqual("/* Hello */", HelloWrapper("/* ", " */")),

Options = [{decimals, 4}, compact],
Formatter = erlz:partial(fun erlang:float_to_binary/2, ['_', Options]),
?assertEqual(<<"16.0">>, Formatter(16.0)),
?assertEqual(<<"3.1416">>, Formatter(3.1415926)),
```

Atom '_' specifies a place for substituted value.


## do-notation

Simple functions pipeline:
```error
Res = erlz:do("  hello there!  ", [
    fun string:strip/1,
    fun string:to_upper/1,
    erlz:partial(fun string:tokens/2, ['_', " "]),
    fun hd/1
]),
?assertEqual("HELLO", Res),
```

Pipeline with monad Maybe:
```error
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
```

Pipeline with monad Error:
```error
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
```

Example from real project:
```erlang
handle_notification(Obj) ->
    erlz:error_do(Obj, [
        fun validate_expiration_date/1,
        fun validate_content/1,
        fun validate_links/1,
        fun compose_message/1,
        fun check_is_canceled/1,
        fun send_notification/1
    ]).
```

## Traversable

Map list of simple values over a monadic function to get monadic result.

Monad Maybe example:
```erlang
Fn = fun
         (V) when V < 5 -> {just, V * 2};
         (_) -> nothing
     end,
?assertEqual({just, [2,4,6,8]}, erlz:maybe_traverse(Fn, [1,2,3,4])),
?assertEqual(nothing, erlz:maybe_traverse(Fn, [1,2,6,3,4])),
```

Monad Either example:
```erlang
Fn = fun
         (V) when V < 5 -> {right, V * 2};
         (_) -> {left, "more than 5"}
    end,
?assertEqual({right, [2,4,6,8]}, erlz:either_traverse(Fn, [1,2,3,4])),
?assertEqual({left, "more than 5"}, erlz:either_traverse(Fn, [1,2,6,3,4])),
```

## foldM

Fold list of monadic values over simple function to get monadic result.

Monad Maybe example:
```erlang
Fn = fun
         (Char, Str) when Char > $Z -> {just, [Char | Str]};
         (_, _) -> nothing
     end,
?assertEqual({just, "dcba"}, erlz:maybe_foldlM(Fn, "", "abcd")),
?assertEqual({just, "abcd"}, erlz:maybe_foldrM(Fn, "", "abcd")),
?assertEqual(nothing, erlz:maybe_foldlM(Fn, "", "aBc")),
?assertEqual(nothing, erlz:maybe_foldrM(Fn, "", "abC")),
```

Monad Error example:
```erlang
Fn = fun
         (Char, Str) when Char > $Z -> {ok, [Char | Str]};
         (_, _) -> {error, "uppercase char"}
     end,
?assertEqual({ok, "dcba"}, erlz:error_foldlM(Fn, "", "abcd")),
?assertEqual({ok, "abcd"}, erlz:error_foldrM(Fn, "", "abcd")),
?assertEqual({error, "uppercase char"}, erlz:error_foldlM(Fn, "", "aBc")),
?assertEqual({error, "uppercase char"}, erlz:error_foldrM(Fn, "", "abC")),
```
