-module(order_handler).

-include("app_declarations.hrl").

%% API
-export([handle_order/2]).


%%%===================================================================
%%% API
%%%===================================================================

handle_order(ClientCoords, TaxiDBAccess) when is_record(ClientCoords, coords)
    andalso is_pid(TaxiDBAccess) ->
  TaxiDB = taxi_database_access:get_taxi_db(TaxiDBAccess),
  Taxis = taxi_database:get_taxis(TaxiDB),
  DistanceFun = fun(Taxi) -> taxi_to_client_distance(Taxi, ClientCoords) end,
  Distances = utils:concurrent_map(DistanceFun, Taxis),
  Available = lists:filter(fun({_,D}) -> D /= busy end, Distances),
  Sorted = lists:sort(fun({_,A}, {_,B}) -> A < B end, Available),
  offer_job(Sorted, ClientCoords).


%%%===================================================================
%%% private functions
%%%===================================================================

taxi_to_client_distance(Taxi, ClientCoords) ->
  TaxiCoords = taxi:get_position(Taxi),
  Distance = utils:distance(TaxiCoords, ClientCoords),
  {Taxi, Distance}.

offer_job(Distances, ClientCoords) ->
  case offer_next(Distances, ClientCoords) of
    accepted -> ok;
    rejected -> offer_job(Distances, ClientCoords)
  end.

offer_next([], _ClientCoords) ->
  rejected;

offer_next([{TaxiPid, _Distance} | T], ClientCoords) ->
  case taxi:offer_job(TaxiPid, ClientCoords) of
    job_accepted -> accepted;
    job_rejected -> offer_next(T, ClientCoords)
  end.
