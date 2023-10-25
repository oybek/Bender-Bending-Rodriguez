-module(ben_update_handler).

-export([run_update_handler/0]).

run_update_handler() ->
    receive
        {pe4kin_update, BotName, #{<<"message">> := Message}} ->
            logger:notice("message: ~ts", [jiffy:encode(Message)]),
            on_message(BotName, Message);
        {pe4kin_update, BotName, #{<<"callback_query">> := CallbackQuery}} ->
            logger:notice("callback_query: ~ts", [jiffy:encode(CallbackQuery)]),
            on_callback_query(BotName, CallbackQuery)
    end,
    run_update_handler().

% private functions

on_message(BotName, #{<<"chat">> := #{<<"id">> := ChatId}, <<"text">> := Text}) ->
    HeartEmoji = pe4kin_emoji:name_to_char(heart),
    ResponseText = unicode:characters_to_binary([Text, HeartEmoji]),
    Board = board:empty(),
    {ok, _} =
        pe4kin:send_message(BotName,
                            #{chat_id => ChatId,
                              text => ResponseText,
                              reply_markup => board:to_inline_keyboard(Board)});
on_message(_, _) ->
    ok.

on_callback_query(_, _) ->
    ok.
