-module(domain).

-export_type([board/0, coord/0, outcome/0, xo/0]).

-export([board_size/0]).

-type board() :: {[coord()], [coord()]}.
-type coord() :: {integer(), integer()}.
-type outcome() :: xWon
                 | oWon
                 | draw.
-type xo() :: x
            | o.

-spec board_size() -> integer().
board_size() -> 3.
