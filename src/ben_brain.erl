-module(ben_brain).

-export([think/1, who_wins/1, init/0]).

-define(MEM, '00.mem').

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
              #{xWon := X} -> X;
              #{draw := X} -> X
            end;

          o ->
            case PossibleOutcomes of
              #{oWon := X} -> X;
              #{draw := X} -> X
            end
        end,
      board:put(Board, C)
  end.


init() ->
  {ok, _} = dets:open_file(?MEM, []),
  who_wins(board:empty()).


who_wins(Board) ->
  case dets:lookup(?MEM, board:serialize(Board)) of
    [{_, Cached}] -> Cached;

    _ ->
      case board:winner(Board) of
        undefined ->
          DesiredOutcomes =
            case board:turn(Board) of
              x -> [xWon, draw, oWon];
              o -> [oWon, draw, xWon]
            end,
          PossibleOutcomes = [who_wins(board:put(Board, C)) || C <- board:free_cells(Board)],
          Result = hd([X || X <- DesiredOutcomes, lists:member(X, PossibleOutcomes)]),
          dets:insert_new(?MEM, {board:serialize(Board), Result}),
          Result;

        Some ->
          dets:insert_new(?MEM, {board:serialize(Board), Some}),
          Some
      end
  end.
