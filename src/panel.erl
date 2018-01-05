-module(panel).

%% API
-export([clear/0, goto/1, printxy/2]).

clear() ->
  io:format("\e[2J",[]).

goto({X, Y}) ->
  io:format("\e[~p;~pH",[Y,X]).

printxy(Msg, {X, Y}) ->
  io:format("\e[~p;~pH~p",[Y,X,Msg]).
