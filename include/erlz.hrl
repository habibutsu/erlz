-type maybe() :: {just, any()} | nothing.
-type fun_maybe() :: fun((any()) -> maybe()).

-type either() :: {left, any()} | {right, any()}.
-type fun_either() :: fun((any()) -> either()).

-type ok_or_error() :: {ok, any()} | {error, any()}.
-type fun_ok_or_error() :: fun((any()) -> ok_or_error()).