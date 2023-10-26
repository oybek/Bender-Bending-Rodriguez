-module(ben_update_handler).

-export([run_update_handler/1]).

run_update_handler(State) ->
  receive
    {pe4kin_update, _, #{<<"message">> := Message}} ->
      logger:notice("got message: ~ts", [jiffy:encode(Message)]),
      on_message(State, Message),
      NextState = State;

    {pe4kin_update, _, #{<<"callback_query">> := CallbackQuery}} ->
      logger:notice("got callback_query: ~ts", [jiffy:encode(CallbackQuery)]),
      NextState = on_callback_query(State, CallbackQuery)
  end,
  run_update_handler(NextState).


% private functions
on_message({BotName, _} = _, #{<<"chat">> := #{<<"id">> := ChatId}, <<"text">> := _}) ->
  send_menu(BotName, ChatId);

on_message(_, _) -> logger:notice("doing nothing").

send_menu(BotName, ChatId) ->
  pe4kin:send_message(
    BotName,
    #{
      chat_id => ChatId,
      text => <<"You wanna play? Let's play!\nChoose " "you fighter!">>,
      reply_markup
      =>
      #{
        inline_keyboard
        =>
        [
          [
            #{text => <<"x">>, callback_data => <<"x">>},
            #{text => <<"o">>, callback_data => <<"o">>}
          ]
        ]
      }
    }
  ).

on_callback_query(
  {Bot, Boards} = _,
  #{
    <<"message">> := #{<<"chat">> := #{<<"id">> := ChatId}, <<"message_id">> := MessageId},
    <<"data">> := Data
  }
) ->
  Board =
    case Data of
      <<"x">> -> board:empty();
      <<"o">> -> ben_brain:think(board:empty());

      _ ->
        [X, Y] = [binary_to_integer(N) || N <- binary:split(Data, <<",">>)],
        play(maps:get(ChatId, Boards), {X, Y})
    end,
  pe4kin:api_call(
    Bot,
    <<"editMessageText">>,
    {
      json,
      #{
        chat_id => ChatId,
        message_id => MessageId,
        text => <<"You'll never win!">>,
        reply_markup => board:to_inline_keyboard(Board)
      }
    }
  ),
  {Bot, maps:put(ChatId, Board, Boards)}.


play(Board, {X, Y}) ->
  case lists:member({X, Y}, board:free_cells(Board)) of
    true -> ben_brain:think(board:put(Board, {X, Y}));
    false -> Board
  end.
