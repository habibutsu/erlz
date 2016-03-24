-module('erlz_monad_either').

-compile([export_all]).

%%====================================================================
%% Either
%%====================================================================

% instance Monad (Either e) where
%     return = Right
%     Left  l >>= _ = Left l
%     Right r >>= k = k r

return(V) -> {right, V}.

'>>='({left, V}, _Fn) -> {left, V};
'>>='({right, V}, Fn) -> Fn(V);
'>>='(Fn, V) -> throw({bad_match, "could not prepare value", V, "for", Fn}).

% instance Functor (Either a) where
%     fmap _ (Left x) = Left x
%     fmap f (Right y) = Right (f y)
fmap(_Fn, {left, V}) -> {left, V};
fmap(Fn, {right, V}) -> {right, Fn(V)}.
