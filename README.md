# Overview

Set of helpers functions for more convenient functional programming on Erlang

Provide following ability:

* currying
* partial applications
* simplest monadic computation

## Examples

### Currying

```erlang
Fn = fun(A, B, C, D) ->
    A + B + C + D
end,
CFn = erlz:curried(Fn),
10 = (((CFn(1))(2))(3))(4).
```

### Partial application

```erlang
Fn = fun(A, B, C, D, E) ->
    1 = A,
    2 = B,
    3 = C,
    4 = D,
    5 = E,
    A + B + C + D + E
end,
% Atom '_' is used for specifying place where value must be substituted
PFn = erlz:partial(Fn, [1, '_', '_', 4, 5]),
{arity, 2} = erlang:fun_info(PFn, arity),
15 = PFn(2, 3).

[3,7,1,5] = lists:map(
    erlz:partial(fun lists:nth/2, [3]),
    [
        [1,2,3,4],
        [5,6,7,8],
        [9,0,1,2],
        [3,4,5,6]
    ]).
```

### do-notation

```erlang
<<"16.0">> = erlz:do(0, [
    fun(X) -> X+1 end,
    fun(X) -> X+3 end,
    % using currying
    (erlz:curried(fun math:pow/2))(2),
    erlz:partial(
        fun erlang:float_to_binary/2,
        ['_', [{decimals, 4}, compact]])
]).

{left,{stop,1}} = erlz:either_do(0, [
    fun(X) -> {right, X+1} end,
    fun(X) -> {left, {stop, X}} end,
    fun(X) -> {right, X+100} end
]).

{ok, [1,2,3]} = erlz:error_do([
    fun() -> {ok, []} end,
    fun([] = State) -> {ok, State ++ [1]} end,
    fun([1] = State) -> {ok, State ++ [2]} end,
    fun([1,2] = State) -> {ok, State ++ [3]} end
]).

{error, "reason"} = erlz:error_do([
    fun() -> {ok, []} end,
    fun([] = State) -> {ok, State ++ [1]} end,
    fun([1] = State) -> {error, "reason"} end,
    fun([1,2] = State) -> {ok, State ++ [3]} end
]).
```

### Traversable

Example of traverse on Erlang

```erlang
Fn = fun(Item) ->
    case Item > 5 of
        true -> {left, "Contains value greater than 5"};
        _ -> {right, Item * 2}
    end
end,

{left,"Contains value greater than 5"} = erlz:either_traverse(Fn, [6,1,2,3,4]).

{right,[2,4,6,8]} = erlz:either_traverse(Fn, [1,2,3,4]).
```

A similar example as above but on Haskell

```haskell
> (traverse 
    (\x -> if x > 5 then Left ("Contains value greater than 5") else Right (x * 2))
    [6, 1..5])
Left "Contains value greater than 5"


> (traverse 
    (\x -> if x > 5 then Left ("Contains value greater than 5") else Right (x * 2))
    [1..5])
Right [2,4,6,8,10]
```

### foldM

Simple example

```erlang
Fn = fun(X, Sum) ->
    case X > 5 of
        true -> {left, "Contains value greater than 5"};
        _ -> {right, Sum + X}
    end
end,
{right,3} = erlz:either_foldlM(Fn, 0, [1,1,1]),
{left,"Contains value greater than 5"} = erlz:either_foldlM(Fn, 0, [6,1,1]).
```
and analogue on Haskell

```haskell
> let f = (\acc x -> if x > 5 then Left ("Contains value greater than 5") else Right (acc + x)) in foldlM f 0 [1,1,1]
Right 3
> let f = (\acc x -> if x > 5 then Left ("Contains value greater than 5") else Right (acc + x)) in foldlM f 0 [6,1,1]
Left "Contains value greater than 5"
```
