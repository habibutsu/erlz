-module(erlz_monad_maybe).

%% Implementation of Monad and Functor for Maybe
%%
%% analogue to Haskell:
%%
%% instance  Monad Maybe  where
%%     (Just x) >>= k      = k x
%%     Nothing  >>= _      = Nothing
%%     return              = Just
%%     fail _              = Nothing
%%
%% instance  Functor Maybe  where
%%     fmap _ Nothing       = Nothing
%%     fmap f (Just a)      = Just (f a)

-include("erlz.hrl").

-export([return/1, fail/1, '>>='/2, fmap/2]).


-spec return(any()) -> maybe().
return(V) -> {just, V}.


-spec fail(any()) -> maybe().
fail(_V) -> nothing.


-spec '>>='(maybe(), fun_maybe()) -> maybe().
'>>='({just, V}, Fn)    -> Fn(V);
'>>='(nothing, _)       -> nothing;
'>>='(V, _Fn)            -> throw({bad_value, V}).


-spec fmap(function(), maybe()) -> maybe().
fmap(_Fn, nothing)  -> nothing;
fmap(Fn, {just, V}) -> {just, Fn(V)}.
