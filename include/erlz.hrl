-type maybe() :: {just, any()} | nothing.
-type fun_maybe() :: fun((any()) -> maybe()).

-type either() :: {left, any()} | {right, any()}.
-type fun_either() :: fun((any()) -> either()).