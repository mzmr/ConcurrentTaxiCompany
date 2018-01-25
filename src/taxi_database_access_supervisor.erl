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
  supervisor:start_link({local, ?SERVER}, ?MODULE, ?CITIES_NUMBER).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(NumberOfCities) when is_integer(NumberOfCities)
    andalso NumberOfCities >= 0 ->
  AllDBAccesses = [utils:create_child_spec(taxi_database_access, worker) ||
    _X <- lists:seq(1, NumberOfCities)],
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 4},
  {ok, {SupFlags, AllDBAccesses}}.
