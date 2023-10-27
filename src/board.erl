-module(board).

-export([free_cells/1, put/2, turn/1, empty/0, winner/1, serialize/1]).

-define(SIZE, 3).

-spec empty() -> domain:board().
empty() -> {[], []}.

-spec turn(domain:board()) -> domain:xo().
turn({Xs, Os}) when length(Xs) =:= length(Os) -> x;
turn(_) -> o.

-spec put(domain:board(), domain:coord()) -> domain:board().
put({Xs, Os} = Board, Coord) ->
  case turn(Board) of
    x -> {[Coord | Xs], Os};
    o -> {Xs, [Coord | Os]}
  end.


-spec free_cells(domain:board()) -> list(domain:coord()).
free_cells({Xs, Os}) ->
  L = lists:seq(1, ?SIZE),
  [{X, Y} || X <- L, Y <- L, not lists:member({X, Y}, Xs ++ Os)].


-spec winner(domain:board()) -> domain:outcome() | undefined.
winner({Xs, Os} = Board) ->
  Xwin = win_coords(Xs),
  Owin = win_coords(Os),
  Full = free_cells(Board) =:= [],
  if
    Xwin -> xWon;
    Owin -> oWon;
    Full -> draw;
    true -> undefined
  end.


-spec win_coords(list(domain:coord())) -> boolean().
win_coords(Coords) ->
  lists:any(
    fun (Vcoords) -> Vcoords -- Coords =:= [] end,
    [
      [{1, 1}, {1, 2}, {1, 3}],
      [{2, 1}, {2, 2}, {2, 3}],
      [{3, 1}, {3, 2}, {3, 3}],
      [{1, 1}, {2, 1}, {3, 1}],
      [{1, 2}, {2, 2}, {3, 2}],
      [{1, 3}, {2, 3}, {3, 3}],
      [{1, 1}, {2, 2}, {3, 3}],
      [{1, 3}, {2, 2}, {3, 1}]
    ]
  ).

-spec serialize(domain:board()) -> <<>>.
serialize({Xs, Os}) ->
  Board =
    lists2:tabulate(
      ?SIZE,
      ?SIZE,
      fun
        (C) ->
          {X, O} = {lists:member(C, Xs), lists:member(C, Os)},
          if
            X -> <<"x">>;
            O -> <<"o">>;
            true -> <<".">>
          end
      end
    ),
  list_to_binary(Board).
