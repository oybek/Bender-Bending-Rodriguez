-module(ben_update_handler).

-export([run_update_handler/1]).

run_update_handler(State) ->
    receive
        {pe4kin_update, _, #{<<"message">> := Message}} ->
            logger:notice("got message: ~ts", [jiffy:encode(Message)]),
            NextState = on_message(State, Message);
        {pe4kin_update, _, #{<<"callback_query">> := CallbackQuery}} ->
            logger:notice("got callback_query: ~ts", [jiffy:encode(CallbackQuery)]),
            NextState = on_callback_query(State, CallbackQuery)
    end,
    run_update_handler(NextState).

% private functions

on_message({BotName, Boards} = State,
           #{<<"chat">> := #{<<"id">> := ChatId}, <<"text">> := <<"/start">>}) ->
    case maps:find(ChatId, Boards) of
        {ok, _} ->
            pe4kin:send_message(BotName,
                                #{chat_id => ChatId, text => <<"Finish the game above first!">>}),
            State;
        _ ->
            Board = board:empty(),
            {ok, _} =
                pe4kin:send_message(BotName,
                                    #{chat_id => ChatId,
                                      text => <<"You'll never win!">>,
                                      reply_markup => board:to_inline_keyboard(Board)}),
            {BotName, maps:put(ChatId, Board, Boards)}
    end;
on_message(_, _) ->
    logger:notice("doing nothing").

on_callback_query({Bot, Boards} = State,
                  #{<<"message">> :=
                        #{<<"chat">> := #{<<"id">> := ChatId}, <<"message_id">> := MessageId},
                    <<"data">> := Data}) ->
    [X, Y] = [binary_to_integer(X) || X <- binary:split(Data, <<",">>)],
    Board = maps:get(ChatId, Boards, board:empty()),
    case lists:member({X, Y}, board:free_cells(Board)) of
        true ->
            NextBoard = board:put(Board, {X, Y}),
            pe4kin:api_call(Bot,
                            <<"editMessageText">>,
                            {json,
                             #{chat_id => ChatId,
                               message_id => MessageId,
                               text => <<"You'll never win!">>,
                               reply_markup => board:to_inline_keyboard(NextBoard)}}),
            {Bot, maps:put(ChatId, NextBoard, Boards)};
        false ->
            State
    end.
