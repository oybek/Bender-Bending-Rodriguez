-module(board).

-export([free_cells/1, put/2, turn/1, empty/0, to_inline_keyboard/1, winner/1]).

-define(SIZE, 3).

empty() -> {[], []}.

turn({Xs, Os}) when length(Xs) =:= length(Os) -> x;
turn(_) -> o.

put({Xs, Os} = Board, Coord) ->
  case turn(Board) of
    x -> {[Coord | Xs], Os};
    o -> {Xs, [Coord | Os]}
  end.


free_cells({Xs, Os}) ->
  L = lists:seq(1, ?SIZE),
  [{X, Y} || X <- L, Y <- L, not lists:member({X, Y}, Xs ++ Os)].


winner({Xs, Os} = _) ->
  case {win_coords(Xs), win_coords(Os)} of
    {true, false} -> x;
    {false, true} -> o;
    _ -> undefined
  end.


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

to_inline_keyboard({Xs, Os}) ->
  InlineKeyboard =
    lists2:tabulate(
      ?SIZE,
      ?SIZE,
      fun
        (C) ->
          case {lists:member(C, Xs), lists:member(C, Os)} of
            {true, false} -> #{text => <<"x">>, callback_data => to_string(C)};
            {false, true} -> #{text => <<"o">>, callback_data => to_string(C)};
            _ -> #{text => <<" ">>, callback_data => to_string(C)}
          end
      end
    ),
  #{inline_keyboard => InlineKeyboard}.


to_string({X, Y}) ->
  L = [integer_to_list(I) || I <- [X, Y]],
  list_to_binary(string:join(L, ",")).
