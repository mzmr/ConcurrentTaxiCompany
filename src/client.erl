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
  timer:sleep(utils:random_number(?MIN_ORDER_INTERVAL, ?MAX_ORDER_INTERVAL)),
  spawn(?MODULE, run_client, [OrderReceiver]),
  start(OrderReceiver).


%%%===================================================================
%%% private functions
%%%===================================================================

run_client(OrderReceiver) ->
  order_receiver:order_taxi(OrderReceiver, utils:random_coords()).