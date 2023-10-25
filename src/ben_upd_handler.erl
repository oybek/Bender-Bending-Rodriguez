-module(ben_upd_handler).

-export([run_upd_handler/0]).

run_upd_handler() ->
    receive
        {pe4kin_update, BotName, #{<<"message">> := Message}} ->
            logger:notice("message: ~ts", [jiffy:encode(Message)]),
            handle_message(BotName, Message)
    end,
    run_upd_handler().

handle_message(BotName, #{<<"chat">> := #{<<"id">> := ChatId}, <<"text">> := Text}) ->
    HeartEmoji = pe4kin_emoji:name_to_char(heart),
    ResponseText = unicode:characters_to_binary([Text, HeartEmoji]),
    {ok, _} = pe4kin:send_message(BotName, #{chat_id => ChatId, text => ResponseText});
handle_message(_, _) ->
    ok.
