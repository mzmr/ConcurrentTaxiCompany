-module(client).

-include("app_config.hrl").

%% API
-export([start/1]).

%% private functions
-export([run_client/1]).


%%%===================================================================
%%% API
%%%===================================================================

start(OrderReceiver) ->
  timer:sleep(?MIN_INTERVAL + (?MAX_INTERVAL - ?MIN_INTERVAL)*rand:uniform()),
  spawn(?MODULE, run_client, [OrderReceiver]),
  start(OrderReceiver).


%%%===================================================================
%%% private functions
%%%===================================================================

run_client(OrderReceiver) ->
  X = ?CITY_WIDTH * rand:uniform(),
  Y = ?CITY_LENGTH * rand:uniform(),
  order_receiver:order_taxi(OrderReceiver, #coords{x=X, y=Y}).