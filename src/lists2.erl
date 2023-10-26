-module(lists2).

-export([tabulate/3]).

tabulate(XM, YM, F) -> [[F({X, Y}) || Y <- lists:seq(1, YM)] || X <- lists:seq(1, XM)].
