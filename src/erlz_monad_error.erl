-module(erlz_monad_error).

%% Implementation of Monad and Functor for Error

-include("erlz.hrl").

-export([return/1, '>>='/2, fmap/2]).


return(V) ->
    {ok, V}.


'>>='({error, Reason}, _) ->
    {error, Reason};
'>>='({ok, V}, Fn) ->
    Fn(V);
'>>='(V, Fn) ->
    throw({bad_match, "could not prepare value", V, "for", Fn}).


fmap(_Fn, {error, V}) ->
    {error, V};
fmap(Fn, {ok, V}) ->
    {ok, Fn(V)}.
