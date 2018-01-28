-module(order_receiver).

-behaviour(gen_server).

-include("app_config.hrl").

%% API
-export([start_link/1,
  stop/1,
  order_taxi/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(TaxiSup=#taxi_sup{pid=P}) when is_pid(P) ->
  utils:log_creating_process(?MODULE),
  gen_server:start_link(?MODULE, TaxiSup, []).

order_taxi(Receiver, Coords) when is_pid(Receiver)
    andalso is_record(Coords, coords) ->
  gen_server:cast(Receiver, {order_taxi, Coords}).

stop(Receiver) when is_pid(Receiver) ->
  gen_server:stop(Receiver).


%%%===================================================================
%%% private functions
%%%===================================================================

init(TaxiSup) ->
  {ok, TaxiSup}.

handle_cast({order_taxi, Coords}, TaxiSup) ->
  spawn(order_handler, handle_order, [Coords, TaxiSup, self()]),
  {noreply, TaxiSup};

handle_cast(_Msg, _TaxiSup) -> {noreply, _TaxiSup}.

handle_call(Msg, _From, TaxiSup) -> {reply, {unsupported_message, Msg}, TaxiSup}.

handle_info(_Info, _TaxiSup) -> {noreply, _TaxiSup}.

terminate(_Reason, _TaxiSup) -> ok.

code_change(_OldVsn, TaxiSup, _Extra) -> {ok, TaxiSup}.
