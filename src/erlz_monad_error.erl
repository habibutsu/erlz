-module(erlz_monad_error).

%% Implementation of Monad and Functor for Error

-include("erlz.hrl").

-export([return/1, '>>='/2, fmap/2]).


-spec return(any()) -> ok_or_error().
return(V) -> {ok, V}.


-spec '>>='(ok_or_error(), fun_ok_or_error()) -> ok_or_error().
'>>='({ok, V}, Fn) -> Fn(V);
'>>='({error, Reason}, _) -> {error, Reason};
'>>='(V, _Fn) -> throw({bad_value, V}).


-spec fmap(function(), ok_or_error()) -> ok_or_error().
fmap(Fn, {ok, V}) -> {ok, Fn(V)};
fmap(_Fn, {error, V}) -> {error, V}.
