-module(order_handler).

-include("app_config.hrl").

%% API
-export([handle_order/2]).


%%%===================================================================
%%% API
%%%===================================================================

handle_order(ClientCoords, TaxiDBAccess) when is_record(ClientCoords, coords)
    andalso is_pid(TaxiDBAccess) ->
  TaxiDB = taxi_database_access:get_taxi_db(TaxiDBAccess),
  Taxis = taxi_database:get_taxis(TaxiDB),
  DistanceFun = fun(T) -> distance_to_client(T, ClientCoords) end,
  Distances = utils:concurrent_map(DistanceFun, Taxis),
  Available = lists:filter(fun({_,D}) -> D /= busy end, Distances),
  Sorted = lists:sort(fun({_,A}, {_,B}) -> A < B end, Available),
  offer_next(Sorted, ClientCoords).


%%%===================================================================
%%% private functions
%%%===================================================================

distance_to_client(Taxi, ClientCoords) ->
  TaxiCoords = taxi:get_position(Taxi),
  Distance = utils:distance(TaxiCoords, ClientCoords),
  {Taxi, Distance}.

offer_next([], _ClientCoords) ->
  rejected;

offer_next([{TaxiPid, _Distance} | T], ClientCoords) ->
  case taxi:offer_job(TaxiPid, ClientCoords) of
    job_accepted -> accepted;
    job_rejected -> offer_next(T, ClientCoords)
  end.
