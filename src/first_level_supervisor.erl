-module(first_level_supervisor).

-behaviour(supervisor).

-include("app_config.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  utils:log_creating_process(?MODULE),
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 10},
  TaxiSups = create_taxi_sups(?CITIES_NUMBER),
  {ok, {SupFlags, TaxiSups}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_taxi_sups(NumberOfCities) when is_integer(NumberOfCities)
    andalso NumberOfCities > 0 ->
  [utils:create_child_spec(taxi_supervisor, supervisor)
    || _ <- lists:seq(1, NumberOfCities)].
