-module(taxi_database_access_supervisor).

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
  Result = supervisor:start_link({local, ?SERVER}, ?MODULE, ?CITIES_NUMBER),
  case Result of
    {ok, _Pid} -> spawn_link(fun start_taxi_database_supervisor/0)
  end,
  Result.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(NumberOfCities) when is_integer(NumberOfCities)
    andalso NumberOfCities >= 0 ->
  TaxiDBAccess = #{start => {taxi_database_access, start_link, []},
    restart => permanent,
    shutdown => brutal_kill,
    type => worker,
    modules => [taxi_database_access]},
  AllDBAccesses = [TaxiDBAccess#{id => erlang:unique_integer()} ||
    _X <- lists:seq(1,NumberOfCities)],
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 4},
  {ok, {SupFlags, AllDBAccesses}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_taxi_database_supervisor() ->
  TaxiDBSupervisor = #{id => erlang:unique_integer(),
    start => {taxi_database_supervisor, start_link, []},
    restart => permanent,
    shutdown => brutal_kill,
    type => supervisor,
    modules => [taxi_database_supervisor]},
  supervisor:start_child(main_supervisor, TaxiDBSupervisor).