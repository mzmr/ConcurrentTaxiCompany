-module(utils).

-include("app_declarations.hrl").

%% API
-export([distance/2]).

distance(#coords{x=X1, y=Y1}, #coords{x=X2, y=Y2}) ->
  math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).