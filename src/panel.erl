-module(panel).

-behaviour(gen_server).

-include("app_config.hrl").

%% API
-export([start_link/0]).

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
  utils:log_creating_process(?MODULE),
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  case ?DEBUG of
    on -> ok;
    _ ->
      print_description(),
      erlang:send_after(?REFRESH_INTERVAL, self(), refresh_panel)
  end,
  {ok, []}.

handle_call(_Msg, _From, Data) -> {noreply, Data}.

handle_cast(_Msg, Data) -> {noreply, Data}.

handle_info(refresh_panel, Data) ->
  erlang:send_after(?REFRESH_INTERVAL, self(), refresh_panel),
  refresh(),
  {noreply, Data};

handle_info(_Info, Data) -> {noreply, Data}.

terminate(_Reason, _Data) -> ok.

code_change(_OldVsn, Data, _Extra) -> {ok, Data}.

%%%===================================================================
%%% private functions
%%%===================================================================

print_description() ->
  L = 1,
  Description = [
    clear(),
    printxy({L,1}, "TAXIS"),
    printxy({L,2}, " - Total:"),
    printxy({L,3}, " - Waiting:"),
    printxy({L,4}, " - Inactive:"),
    printxy({L,5}, " - Driving to clients:"),
    printxy({L,6}, " - Driving with clients:"),
    printxy({L,7}, " - Driving from clients:"),
    printxy({L,9}, "ORDERS"),
    printxy({L,10}, " - Total/accepted:"),
    printxy({L,11}, " - In progress:"),
    printxy({L,12}, " - Finished:"),
    printxy({L,14}, "DRIVEN DISTANCE [KM]"),
    printxy({L,15}, " - Total:"),
    printxy({L,16}, " - With clients:"),
    printxy({L,18}, "FINANCE [PLN]"),
    printxy({L,19}, " - Income:"),
    printxy({L,20}, " - Expenditure:"),
    printxy({L,21}, " - Balance:"),
    goto({L,23})
  ],
  io:format(lists:flatten(Description)).

refresh() ->
  R = 30,
  TotalDriven = stats:get_total_distance_driven() / 1000,
  DrivenWithClients = stats:get_distance_driven_with_client() / 1000,
  FinishedOrders = stats:get_finished_orders_number(),
  Income = FinishedOrders * ?ORDER_CONST_COST + DrivenWithClients * ?ONE_KM_COST,
  Expenditure = TotalDriven * ?FUEL_LITER_COST * ?LITERS_PER_KM / 100,
  Balance = Income - Expenditure,
  Summary = [
    printxy({R,2}, stats:get_taxi_number()),
    printxy({R,3}, stats:get_waiting_taxi_number()),
    printxy({R,4}, stats:get_inactive_taxi_number()),
    printxy({R,5}, stats:get_driving_to_client_taxi_number()),
    printxy({R,6}, stats:get_driving_with_client_taxi_number()),
    printxy({R,7}, stats:get_driving_from_client_taxi_number()),
    printxy({R,10}, stats:get_accepted_orders_number()),
    printxy({R,11}, stats:get_in_progress_orders_number()),
    printxy({R,12}, FinishedOrders),
    printxy({R,15}, round_2_places(TotalDriven)),
    printxy({R,16}, round_2_places(DrivenWithClients)),
    printxy({R,19}, round_2_places(Income)),
    printxy({R,20}, round_2_places(Expenditure)),
    printxy({R,21}, round_2_places(Balance)),
    goto({1,23})
  ],
  io:format(lists:flatten(Summary)).

round_2_places(Number) when is_float(Number) ->
  float_to_list(Number,[{decimals,2}]).

clear() ->
  io_lib:format("\e[2J",[]).

goto({X, Y}) ->
  io_lib:format("\e[~p;~pH",[Y,X]).

printxy({X, Y}, Msg) when is_list(Msg)->
  io_lib:format("\e[~p;~pH" ++ Msg, [Y,X]);

printxy({X, Y}, Msg) ->
  io_lib:format("\e[~p;~pH~p                    ",[Y,X,Msg]).
