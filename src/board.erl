-module(board).

-export([free_cells/1, put/2, turn/1, empty/0, to_inline_keyboard/1]).

-define(SIZE, 3).

empty() ->
    {[], []}.

turn({Xs, Os}) when length(Xs) =:= length(Os) ->
    x;
turn(_) ->
    o.

put({Xs, Os} = Board, Coord) ->
    case turn(Board) of
        x ->
            {[Coord | Xs], Os};
        o ->
            {Xs, [Coord | Os]};
        _ ->
            Board
    end.

free_cells({Xs, Os}) ->
    L = lists:seq(1, ?SIZE),
    Coords = [{X, Y} || X <- L, Y <- L],
    lists:filter(fun(C) -> not lists:member(C, Xs ++ Os) end, Coords).

to_inline_keyboard({Xs, Os}) ->
    InlineKeyboard =
        tabulate(?SIZE,
                 ?SIZE,
                 fun(X, Y) ->
                    C = {X, Y},
                    case {lists:member(C, Xs), lists:member(C, Os)} of
                        {true, false} ->
                            #{text => <<"x">>, callback_data => to_string(C)};
                        {false, true} ->
                            #{text => <<"o">>, callback_data => to_string(C)};
                        _ ->
                            #{text => <<" ">>, callback_data => to_string(C)}
                    end
                 end),
    #{inline_keyboard => InlineKeyboard}.

tabulate(XM, YM, F) ->
    [[F(X, Y) || Y <- lists:seq(1, YM)] || X <- lists:seq(1, XM)].

to_string({X, Y}) ->
    L = [integer_to_list(I) || I <- [X, Y]],
    list_to_binary(string:join(L, ",")).
