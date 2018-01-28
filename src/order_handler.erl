-module(order_handler).

-include("app_config.hrl").

%% API
-export([handle_order/3]).


%%%===================================================================
%%% API
%%%===================================================================

handle_order(ClientCoords, #taxi_sup{pid=P}, Receiver) when is_pid(Receiver)
    andalso is_record(ClientCoords, coords) andalso is_pid(P) ->
  utils:log_creating_process(?MODULE),
  Taxis = taxi_supervisor:get_taxis(P),
  DistanceFun = fun(T) -> distance_to_client(T, ClientCoords) end,
  Distances = utils:concurrent_map(DistanceFun, Taxis),
  Available = lists:filter(fun({_,D}) -> D /= busy end, Distances),
  Sorted = lists:sort(fun({_,A}, {_,B}) -> A < B end, Available),
  Receiver ! offer_next(Sorted, ClientCoords).


%%%===================================================================
%%% private functions
%%%===================================================================

distance_to_client(Taxi, ClientCoords) ->
  case taxi:get_position(Taxi) of
    busy -> {Taxi, busy};
    TaxiCoords -> {Taxi, utils:distance(TaxiCoords, ClientCoords)}
  end.

offer_next([], _ClientCoords) ->
  rejected;

offer_next([{_TaxiPid, Distance} | _T], _ClientCoords)
    when Distance > ?MAX_DISTANCE_TO_CLIENT ->
  rejected;

offer_next([{TaxiPid, _Distance} | T], ClientCoords) ->
  case taxi:offer_job(TaxiPid, ClientCoords) of
    job_accepted -> accepted;
    job_rejected -> offer_next(T, ClientCoords)
  end.
