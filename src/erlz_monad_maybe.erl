-module('erlz_monad_maybe').

-compile([export_all]).

%%====================================================================
%% Maybe (just for fun :)
%%====================================================================

% instance  Monad Maybe  where
%     (Just x) >>= k      = k x
%     Nothing  >>= _      = Nothing
%     return              = Just
%     fail _              = Nothing

return(V) -> {just, V}.

fail(_V) -> nothing.

'>>='({just, V}, Fn)    -> Fn(V);
'>>='(nothing, _)       -> nothing;
'>>='(Fn, V)            -> throw({bad_match, "could not prepare value", V, "for", Fn}).

% instance  Functor Maybe  where
%     fmap _ Nothing       = Nothing
%     fmap f (Just a)      = Just (f a)
fmap(_Fn, nothing)  -> nothing;
fmap(Fn, {just, V}) -> {just, Fn(V)}.
