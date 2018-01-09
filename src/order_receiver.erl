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

start_link(TaxiDBAccess) when is_pid(TaxiDBAccess) ->
  gen_server:start_link(?MODULE, TaxiDBAccess, []).

order_taxi(Receiver, Coords) when is_pid(Receiver)
    andalso is_record(Coords, coords) ->
  gen_server:call(Receiver, {order_taxi, Coords}).

stop(Receiver) when is_pid(Receiver) ->
  gen_server:stop(Receiver).


%%%===================================================================
%%% private functions
%%%===================================================================

init(TaxiDBAccess) ->
  {ok, TaxiDBAccess}.

handle_call({order_taxi, Coords}, _From, TaxiDBAccess) ->
  spawn(order_handler, handle_order, [Coords, TaxiDBAccess]),
  {reply, {order_accepted, Coords}, TaxiDBAccess};

handle_call(_Msg, _From, TaxiDBAccess) -> {noreply, TaxiDBAccess}.


handle_cast(_Msg, TaxiDBAccess) -> {noreply, TaxiDBAccess}.

handle_info(_Info, TaxiDBAccess) -> {noreply, TaxiDBAccess}.

terminate(_Reason, _TaxiDBAccess) -> ok.

code_change(_OldVsn, TaxiDBAccess, _Extra) -> {ok, TaxiDBAccess}.
