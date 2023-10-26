-module(ben_brain).

-export([think/1, who_wins/1, init/0]).

think(Board) -> Board.

init() -> {ok, _} = dets:open_file(ben_memory, []).

who_wins(Board) ->
  case dets:lookup(ben_memory, board:serialize(Board)) of
    [{_, Cached}] -> Cached;

    _ ->
      case board:winner(Board) of
        undefined ->
          PossibleOutcomes = [who_wins(board:put(Board, C)) || C <- board:free_cells(Board)],
          DesiredOutcomes =
            case board:turn(Board) of
              x -> [xwon, draw, owon];
              o -> [owon, draw, xwon]
            end,
          Result = hd([X || X <- DesiredOutcomes, lists:member(X, PossibleOutcomes)]),
          dets:insert_new(ben_memory, {board:serialize(Board), Result}),
          Result;

        Some ->
          dets:insert_new(ben_memory, {board:serialize(Board), Some}),
          Some
      end
  end.
