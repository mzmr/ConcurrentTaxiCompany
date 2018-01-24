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

%% private functions
-export([refresh/0]).

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
  erlang:send_after(?REFRESH_INTERVAL, self(), refresh_panel),
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

refresh() ->
  case ?DEBUG of
    on -> ok;
    _ ->
      Col2 = 55,
      Summary = [
        clear(),
        printxy({1,1}, "Total number of taxis:"),
        printxy({Col2,1}, stats:get_taxi_number()),
        printxy({1,3}, "Number of taxis waiting for a job:"),
        printxy({Col2,3}, stats:get_waiting_taxi_number()),
        printxy({1,5}, "Number of inactive taxis:"),
        printxy({Col2,5}, stats:get_inactive_taxi_number()),
        printxy({1,7}, "Number of taxis driving with clients:"),
        printxy({Col2,7}, stats:get_driving_with_client_taxi_number()),
        printxy({1,9}, "Number of taxis driving to clients:"),
        printxy({Col2,9}, stats:get_driving_to_client_taxi_number()),
        printxy({1,11}, "Number of taxis driving from clients:"),
        printxy({Col2,11}, stats:get_driving_from_client_taxi_number()),
        printxy({1,13}, "Number of accepted orders:"),
        printxy({Col2,13}, stats:get_accepted_orders_number()),
        printxy({1,15}, "Number of orders in progress:"),
        printxy({Col2,15}, stats:get_in_progress_orders_number()),
        printxy({1,17}, "Number finished orders:"),
        printxy({Col2,17}, stats:get_finished_orders_number()),
        printxy({1,19}, "Total distance driven by all taxis:"),
        printxy({Col2,19}, stats:get_total_distance_driven()),
        printxy({1,21}, "Total distance driven by all taxis with clients:"),
        printxy({Col2,21}, stats:get_distance_driven_with_client()),
        goto({1,22})
      ],
      io:format(lists:flatten(Summary))
  end.

clear() ->
  io_lib:format("\e[2J",[]).

goto({X, Y}) ->
  io_lib:format("\e[~p;~pH",[Y,X]).

printxy(Coords, Number) when is_integer(Number) ->
  printxy(Coords, integer_to_list(Number));

printxy({X, Y}, Msg) ->
  list_to_bitstring(io_lib:format("\e[~p;~pH~p",[Y,X,Msg])).
