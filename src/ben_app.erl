-module(ben_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  application:ensure_all_started(pe4kin),
  {ok, Config} = file:consult("bot.config"),
  {token, Token} = lists:keyfind(token, 1, Config),
  BotName = <<"MrBenderBendingRodriguezBot">>,
  BotToken = unicode:characters_to_binary(Token),
  % Spawn process which listen for telegram updates
  Pid = spawn(ben_update_handler, run_update_handler, [{BotName, maps:new()}]),
  % Initialize bot
  pe4kin:launch_bot(BotName, BotToken, #{receiver => true}),
  % Subscribe process to telegram updates
  pe4kin_receiver:subscribe(BotName, Pid),
  pe4kin_receiver:start_http_poll(BotName, #{limit => 100, timeout => 60}),
  ben_sup:start_link().


stop(_State) -> ok.

%% internal functions
