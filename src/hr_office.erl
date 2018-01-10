-module(hr_office).

-behaviour(gen_server).

-include("app_config.hrl").

%% API
-export([start_link/1,
  stop/1,
  apply_for_job/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%% private functions
-export([add_new_taxi/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(TaxiDBAccess) when is_pid(TaxiDBAccess) ->
  gen_server:start_link(?MODULE, TaxiDBAccess, []).

apply_for_job(Office) when is_pid(Office) ->
  gen_server:call(Office, apply_for_job).

stop(Office) when is_pid(Office) ->
  gen_server:stop(Office).


%%%===================================================================
%%% private functions
%%%===================================================================

init(TaxiDBAccess) ->
  {ok, TaxiDBAccess}.

handle_call(apply_for_job, _From, TaxiDBAccess) ->
  Rand = rand:uniform() * 100,
  case Rand < ?CHANCE_FOR_ACCEPTANCE of
    true ->
      spawn(?MODULE, add_new_taxi, [TaxiDBAccess]),
      {reply, application_accepted, TaxiDBAccess};
    false ->
      {reply, application_rejected, TaxiDBAccess}
  end;

handle_call(_Msg, _From, TaxiDBAccess) -> {noreply, TaxiDBAccess}.


add_new_taxi(TaxiDBAccess) ->
  NewTaxi = taxi:start_link(utils:random_coords()),
  AddNewTaxiFun = fun(DB) -> taxi_database:add_taxi(DB, NewTaxi) end,
  TaxiDBs = taxi_database_access:get_all_taxi_db(TaxiDBAccess),
  utils:concurrent_map(AddNewTaxiFun, TaxiDBs),
  ok.

handle_cast(_Msg, TaxiDBAccess) -> {noreply, TaxiDBAccess}.

handle_info(_Info, TaxiDBAccess) -> {noreply, TaxiDBAccess}.

terminate(_Reason, _TaxiDBAccess) -> ok.

code_change(_OldVsn, TaxiDBAccess, _Extra) -> {ok, TaxiDBAccess}.
