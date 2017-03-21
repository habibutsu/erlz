-module('erlz_monad_error').

-compile([export_all]).

%%====================================================================
%% Error (special case for Either)
%%====================================================================

%% instance Monad

return(V) ->
    {ok, V}.

'>>='({error, Reason}, _) ->
    {error, Reason};
'>>='({ok, V}, Fn) ->
    Fn(V);
'>>='(V, Fn) ->
    throw({bad_match, "could not prepare value", V, "for", Fn}).

%% instance Functor

fmap(_Fn, {error, V}) ->
    {error, V};
fmap(Fn, {ok, V}) ->
    {ok, Fn(V)}.
