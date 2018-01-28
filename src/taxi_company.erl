-module(taxi_company).

-behaviour(application).

%% Application callbacks
-export([
  start/2,
  stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _Settings) ->
  main_supervisor:start_link(#{show_panel => true}).

stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
