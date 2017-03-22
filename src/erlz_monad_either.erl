-module(erlz_monad_either).

%% Implementation of Monad and Functor for Either
%%
%% analogue to Haskell:
%%
%% instance Monad (Either e) where
%%     return = Right
%%     Left  l >>= _ = Left l
%%     Right r >>= k = k r
%%
%% instance Functor (Either a) where
%%     fmap _ (Left x) = Left x
%%     fmap f (Right y) = Right (f y)

-include("erlz.hrl").

-export([return/1, '>>='/2, fmap/2]).


-spec return(any()) -> either().
return(V) -> {right, V}.


-spec '>>='(either(), fun_either()) -> either().
'>>='({left, V}, _Fn) -> {left, V};
'>>='({right, V}, Fn) -> Fn(V);
'>>='(V, _Fn) -> throw({bad_value, V}).


-spec fmap(function(), either()) -> either().
fmap(_Fn, {left, V}) -> {left, V};
fmap(Fn, {right, V}) -> {right, Fn(V)}.
