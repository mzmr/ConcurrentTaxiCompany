-module(client).

-behaviour(gen_server).

-include("app_config.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%% private functions
-export([run_client/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(#order_receiver{pid=R}) when is_pid(R) ->
  utils:log_creating_process(?MODULE),
  gen_server:start_link(?MODULE, R, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(OrderReceiver) ->
  Interval = round(utils:random_number(?MIN_ORDER_INTERVAL, ?MAX_ORDER_INTERVAL)),
  erlang:send_after(Interval, self(), order_taxi),
  {ok, OrderReceiver}.

handle_call(_Msg, _From, OrderReceiver) -> {noreply, OrderReceiver}.

handle_cast(_Msg, OrderReceiver) -> {noreply, OrderReceiver}.

handle_info(order_taxi, OrderReceiver) ->
  spawn(?MODULE, run_client, [OrderReceiver]),
  Interval = round(utils:random_number(?MIN_ORDER_INTERVAL, ?MAX_ORDER_INTERVAL)),
  erlang:send_after(Interval, self(), order_taxi),
  {noreply, OrderReceiver};

handle_info(_Info, OrderReceiver) -> {noreply, OrderReceiver}.

terminate(_Reason, _OrderReceiver) -> ok.

code_change(_OldVsn, OrderReceiver, _Extra) -> {ok, OrderReceiver}.

%%%===================================================================
%%% private functions
%%%===================================================================

run_client(Receiver) ->
  order_receiver:order_taxi(Receiver, utils:random_coords()).