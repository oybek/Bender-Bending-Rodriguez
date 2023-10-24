-module(ben_upd_handler).

-export([run_upd_handler/0]).

run_upd_handler() ->
    receive
        {pe4kin_update, BotName, Update} ->
            {ok, ChatId} = pe4kin_types:chat_id(message, Update),
            HeartEmoji = pe4kin_emoji:name_to_char('heart'),
            ResponseText = unicode:characters_to_binary([<<"Hello ">>, HeartEmoji]),
            {ok, _} = pe4kin:send_message(BotName, #{
                chat_id => ChatId, text => ResponseText
            })
    end,
    run_upd_handler().
