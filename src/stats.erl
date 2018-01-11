-module(stats).

-behaviour(gen_server).

-record(data, {
  taxi_number = 0,
  taxi_inactive_number = 0,
  taxi_waiting_number = 0,
  taxi_driving_to_client_number = 0,
  taxi_driving_with_client_number = 0,
  taxi_driving_from_client_number = 0,
  taxi_db_number = 0,
  taxi_db_access_number = 0,
  order_receiver_number = 0,
  orders_accepted = 0,
  orders_in_progress = 0,
  orders_finished = 0
  }).

%% API
-export([start_link/0,
  stop/0,
  created_taxi/0,
  terminated_taxi/0,
  changed_taxi_state/2,
  finished_order/0,
  started_order/0,
  accepted_order/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

created_taxi() ->
  gen_server:cast(?SERVER, {changed_taxi_number, 1}).

terminated_taxi() ->
  gen_server:cast(?SERVER, {changed_taxi_number, -1}).

changed_taxi_state(NewState, OldState) ->
  gen_server:cast(?SERVER, {changed_taxi_state, NewState, OldState}).

accepted_order() ->
  gen_server:cast(?SERVER, accepted_order).

started_order() ->
  gen_server:cast(?SERVER, started_order).

finished_order() ->
  gen_server:cast(?SERVER, finished_order).

stop() ->
  gen_server:stop(?SERVER).


%%%===================================================================
%%% private functions
%%%===================================================================

init([]) ->
  {ok, #data{}}.


handle_cast({changed_taxi_state, State, State}, Data) ->
  {noreply, change_state(State, 1, Data)};

handle_cast({changed_taxi_state, NewState, OldState}, Data) ->
  {noreply, change_state(OldState, -1, change_state(NewState, 1, Data))};

handle_cast({changed_taxi_number, Diff}, Data) ->
  {noreply, Data#data{taxi_number = Data#data.taxi_number + Diff}};

handle_cast(finished_order, Data) ->
  NewData = Data#data{
    orders_in_progress = Data#data.orders_in_progress - 1,
    orders_finished = Data#data.orders_finished + 1},
  {noreply, NewData};

handle_cast(accepted_order, Data) ->
  {noreply, Data#data{orders_accepted = Data#data.orders_accepted + 1}};

handle_cast(started_order, Data) ->
  {noreply, Data#data{orders_in_progress = Data#data.orders_in_progress + 1}};

handle_cast(_Msg, Data) ->
  {noreply, Data}.


handle_call(Request, _From, Data) ->
  {reply, Request, Data}.

handle_info(_Info, Data) -> {noreply, Data}.

terminate(_Reason, _Data) -> ok.

code_change(_OldVsn, Data, _Extra) -> {ok, Data}.


change_state(inactive, Diff, Data) ->
  Data#data{taxi_inactive_number =
      Data#data.taxi_inactive_number + Diff};

change_state(waiting, Diff, Data) ->
  Data#data{taxi_waiting_number =
      Data#data.taxi_waiting_number + Diff};

change_state(driving_to_client, Diff, Data) ->
  Data#data{taxi_driving_to_client_number =
      Data#data.taxi_driving_to_client_number + Diff};

change_state(driving_with_client, Diff, Data) ->
  Data#data{taxi_driving_with_client_number =
      Data#data.taxi_driving_with_client_number + Diff};

change_state(driving_from_client, Diff, Data) ->
  Data#data{taxi_driving_from_client_number =
      Data#data.taxi_driving_from_client_number + Diff}.
