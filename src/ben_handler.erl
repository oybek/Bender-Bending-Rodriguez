-module(ben_handler).

-export([run_update_handler/0, init/0]).

-define(CMD_START, <<"/start">>).
-define(CMD_EMPTY, <<"empty">>).
-define(CMD_MENU, <<"menu">>).
-define(CMD_SET, <<"set">>).
-define(CMD_X, <<"x">>).
-define(CMD_O, <<"o">>).
-define(MEM_FILE, '01.mem').

init() -> {ok, _} = dets:open_file(?MEM_FILE, []).

%
run_update_handler() ->
  receive
    {pe4kin_update, Bot, #{<<"message">> := Message}} ->
      logger:notice("got message: ~ts", [jiffy:encode(Message)]),
      on_message(Bot, Message);

    {pe4kin_update, Bot, #{<<"callback_query">> := CallbackQuery}} ->
      logger:notice("got callback_query: ~ts", [jiffy:encode(CallbackQuery)]),
      on_callback_query(Bot, CallbackQuery)
  end,
  run_update_handler().


%
on_message(Bot, #{<<"chat">> := #{<<"id">> := ChatId}, <<"text">> := ?CMD_START}) ->
  send_menu(Bot, ChatId);

on_message(_, _) -> logger:notice("doing nothing").

%
on_callback_query(
  Bot,
  #{
    <<"message">> := #{<<"chat">> := #{<<"id">> := ChatId}, <<"message_id">> := MessageId},
    <<"data">> := Command
  }
) ->
  on_command(Bot, {ChatId, MessageId}, Command).

%
-spec on_command(bitstring(), domain:coord(), bitstring()) -> any().
on_command(Bot, {ChatId, _}, ?CMD_MENU) -> send_menu(Bot, ChatId);

on_command(Bot, Coord, ?CMD_X) ->
  Board = board:empty(),
  dets:insert(?MEM_FILE, {Coord, Board}),
  send_board(Bot, Coord, Board);

on_command(Bot, Coord, ?CMD_O) ->
  Board = ben_brain:think(board:empty()),
  dets:insert(?MEM_FILE, {Coord, Board}),
  send_board(Bot, Coord, Board);

on_command(Bot, Coord, <<"set", Arg/bitstring>>) ->
  case dets:lookup(?MEM_FILE, Coord) of
    [{_, Board}] ->
      [X, Y] = [binary_to_integer(N) || N <- binary:split(Arg, <<",">>)],
      NextBoard = play(Board, {X, Y}),
      dets:insert(?MEM_FILE, {Coord, NextBoard}),
      send_board(Bot, Coord, play(Board, {X, Y}));

    {error, _} -> ok
  end;

on_command(_, _, _) -> ok.


-spec play(domain:board(), domain:coord()) -> domain:board().
play(Board, {X, Y}) ->
  case lists:member({X, Y}, board:free_cells(Board)) of
    true -> ben_brain:think(board:put(Board, {X, Y}));
    false -> Board
  end.


% helper functions
send_menu(Bot, ChatId) ->
  Keyboard = [[#{text => V, callback_data => V} || V <- [?CMD_X, ?CMD_O]]],
  pe4kin:send_message(
    Bot,
    #{
      chat_id => ChatId,
      text => <<"You wanna play? Let's play!\nChoose " "your fighter!">>,
      reply_markup => #{inline_keyboard => Keyboard}
    }
  ).


send_board(Bot, {ChatId, MessageId}, Board) ->
  pe4kin:api_call(
    Bot,
    <<"editMessageText">>,
    {
      json,
      #{
        chat_id => ChatId,
        message_id => MessageId,
        text => <<"You'll never win!">>,
        reply_markup => to_inline_keyboard(Board)
      }
    }
  ).

-spec to_string(domain:coord()) -> bitstring().
to_string({X, Y}) ->
  L = [integer_to_list(I) || I <- [X, Y]],
  list_to_binary(string:join(L, ",")).


-spec to_inline_keyboard(domain:board()) -> #{inline_keyboard := any()}.
to_inline_keyboard({Xs, Os} = Board) ->
  Winner = board:winner(Board),
  InlineKeyboard =
    lists2:tabulate(
      domain:board_size(),
      domain:board_size(),
      fun
        (C) ->
          CallbackData =
            case Winner of
              undefined -> <<?CMD_SET/bitstring, (to_string(C))/bitstring>>;
              _ -> ?CMD_EMPTY
            end,
          X = lists:member(C, Xs),
          O = lists:member(C, Os),
          if
            X -> #{text => <<"x">>, callback_data => ?CMD_EMPTY};
            O -> #{text => <<"o">>, callback_data => ?CMD_EMPTY};
            true -> #{text => <<" ">>, callback_data => CallbackData}
          end
      end
    ),
  #{
    inline_keyboard => case Winner of
      undefined -> InlineKeyboard;
      _ -> InlineKeyboard ++ [[#{text => <<"New Game">>, callback_data => ?CMD_MENU}]]
    end
  }.
