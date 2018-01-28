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
-define(PARAMS_WIDTH, 15).

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
      send_refresh_request(?REFRESH_INTERVAL)
  end,
  {ok, []}.

handle_call(_Msg, _From, Data) -> {noreply, Data}.

handle_cast(_Msg, Data) -> {noreply, Data}.

handle_info(refresh_panel, Data) ->
  send_refresh_request(?REFRESH_INTERVAL),
  refresh(),
  {noreply, Data};

handle_info(_Info, Data) -> {noreply, Data}.

terminate(_Reason, _Data) -> ok.

code_change(_OldVsn, Data, _Extra) -> {ok, Data}.

%%%===================================================================
%%% private functions
%%%===================================================================

send_refresh_request(Interval) when is_integer(Interval) ->
  erlang:send_after(Interval, self(), refresh_panel).

print_description() ->
  L = 1,
  Description = [
    clear(),
    set_xy({L,1}, "TAXIS"),
    set_xy({L,2}, " - Total:"),
    set_xy({L,3}, " - Waiting:"),
    set_xy({L,4}, " - Inactive:"),
    set_xy({L,5}, " - Driving to clients:"),
    set_xy({L,6}, " - Driving with clients:"),
    set_xy({L,7}, " - Driving from clients:"),
    set_xy({L,9}, "ORDERS"),
    set_xy({L,10}, " - Total:"),
    set_xy({L,11}, " - Accepted:"),
    set_xy({L,12}, " - Rejected:"),
    set_xy({L,13}, " - In progress:"),
    set_xy({L,14}, " - Finished:"),
    set_xy({L,16}, "DRIVEN DISTANCE [KM]"),
    set_xy({L,17}, " - Total:"),
    set_xy({L,18}, " - With clients:"),
    set_xy({L,20}, "FINANCE [PLN]"),
    set_xy({L,21}, " - Income:"),
    set_xy({L,22}, " - Expenditure:"),
    set_xy({L,23}, " - Balance:"),
    goto({L,25})
  ],
  io:format(lists:flatten(Description)).

refresh() ->
  R = 25,
  OrdersAccepted = stats:get_accepted_orders_number(),
  OrdersRejected = stats:get_rejected_orders_number(),
  TotalOrders = OrdersAccepted + OrdersRejected,
  TotalDriven = stats:get_total_distance_driven() / 1000,
  DrivenWithClients = stats:get_distance_driven_with_client() / 1000,
  FinishedOrders = stats:get_finished_orders_number(),
  Income = FinishedOrders * ?ORDER_CONST_COST + DrivenWithClients * ?ONE_KM_COST,
  Wages = stats:get_day_wages_number(),
  Expenditure = TotalDriven * ?FUEL_LITER_COST * ?LITERS_PER_KM / 100
    + Wages * ?DAY_WAGE,
  Balance = Income - Expenditure,
  Summary = [
    set_xy_and_format({R,2}, stats:get_taxi_number()),
    set_xy_and_format({R,3}, stats:get_waiting_taxi_number()),
    set_xy_and_format({R,4}, stats:get_inactive_taxi_number()),
    set_xy_and_format({R,5}, stats:get_driving_to_client_taxi_number()),
    set_xy_and_format({R,6}, stats:get_driving_with_client_taxi_number()),
    set_xy_and_format({R,7}, stats:get_driving_from_client_taxi_number()),
    set_xy_and_format({R,10}, TotalOrders),
    set_xy_and_format({R,11}, OrdersAccepted),
    set_xy_and_format({R,12}, OrdersRejected),
    set_xy_and_format({R,13}, stats:get_in_progress_orders_number()),
    set_xy_and_format({R,14}, FinishedOrders),
    set_xy_and_format({R,17}, TotalDriven),
    set_xy_and_format({R,18}, DrivenWithClients),
    set_xy_and_format({R,21}, Income),
    set_xy_and_format({R,22}, Expenditure),
    set_xy_and_format({R,23}, Balance),
    goto({1,25})
  ],
  io:format(lists:flatten(Summary)).

round_2_places(Number) when is_float(Number) ->
  float_to_list(Number,[{decimals,2}]).

clear() ->
  io_lib:format("\e[2J",[]).

goto({X, Y}) ->
  io_lib:format("\e[~p;~pH",[Y,X]).

set_xy({X, Y}, Msg) when is_list(Msg) ->
  io_lib:format("\e[~p;~pH" ++ Msg, [Y,X]);

set_xy(_Coords, Msg) ->
  io_lib:format("[panel] Error: ~p should be a number or a list.", [Msg]).

set_xy_and_format(Coords, Msg) when is_integer(Msg) ->
  NumString = integer_to_list(Msg),
  Txt = fill_txt_margins(?PARAMS_WIDTH - 3 - length(NumString), 3, NumString),
  set_xy(Coords, Txt);

set_xy_and_format(Coords, Msg) when is_float(Msg) ->
  NumString = round_2_places(Msg),
  Txt = fill_txt_margins(?PARAMS_WIDTH - length(NumString), 0, NumString),
  set_xy(Coords, Txt);

set_xy_and_format(Coords, Msg) when is_list(Msg) ->
  Txt = fill_txt_margins(?PARAMS_WIDTH - length(Msg), 0, Msg),
  set_xy(Coords, Txt).

fill_txt_margins(Left, Right, Text) ->
  lists:concat([space_list(Left), Text, space_list(Right)]).

space_list(Length) when Length >= 0 ->
  [32 || _ <- lists:seq(0, Length)]. % 32 is the ASCII code for space character