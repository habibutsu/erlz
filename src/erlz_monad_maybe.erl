-module('erlz_monad_maybe').

-compile([export_all]).

-include("erlz.hrl").

%%====================================================================
%% Maybe (just for fun :)
%%====================================================================

% instance  Monad Maybe  where
%     (Just x) >>= k      = k x
%     Nothing  >>= _      = Nothing
%     return              = Just
%     fail _              = Nothing

-spec return(any()) -> maybe().
return(V) -> {just, V}.

-spec fail(any()) -> maybe().
fail(_V) -> nothing.

-spec '>>='(maybe(), fun_maybe()) -> maybe().
'>>='({just, V}, Fn)    -> Fn(V);
'>>='(nothing, _)       -> nothing;
'>>='(V, Fn)            -> throw({bad_match, "could not prepare value", V, "for", Fn}).

% instance  Functor Maybe  where
%     fmap _ Nothing       = Nothing
%     fmap f (Just a)      = Just (f a)
-spec fmap(fun_maybe(), maybe()) -> maybe().
fmap(_Fn, nothing)  -> nothing;
fmap(Fn, {just, V}) -> {just, Fn(V)}.
