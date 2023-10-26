-module(ben_brain).

-export([think/1, who_wins/1, init/0]).

think(Board) ->
  case board:free_cells(Board) of
    [] -> Board;

    _ ->
      PossibleOutcomes =
        maps:from_list([{who_wins(board:put(Board, C)), C} || C <- board:free_cells(Board)]),
      C =
        case board:turn(Board) of
          x ->
            case PossibleOutcomes of
              #{xwon := X} -> X;
              #{draw := X} -> X
            end;

          o ->
            case PossibleOutcomes of
              #{owon := X} -> X;
              #{draw := X} -> X
            end
        end,
      board:put(Board, C)
  end.


init() ->
  {ok, _} = dets:open_file(ben_memory, []),
  who_wins(board:empty()).


who_wins(Board) ->
  case dets:lookup(ben_memory, board:serialize(Board)) of
    [{_, Cached}] -> Cached;

    _ ->
      case board:winner(Board) of
        undefined ->
          DesiredOutcomes =
            case board:turn(Board) of
              x -> [xwon, draw, owon];
              o -> [owon, draw, xwon]
            end,
          PossibleOutcomes = [who_wins(board:put(Board, C)) || C <- board:free_cells(Board)],
          Result = hd([X || X <- DesiredOutcomes, lists:member(X, PossibleOutcomes)]),
          dets:insert_new(ben_memory, {board:serialize(Board), Result}),
          Result;

        Some ->
          dets:insert_new(ben_memory, {board:serialize(Board), Some}),
          Some
      end
  end.
