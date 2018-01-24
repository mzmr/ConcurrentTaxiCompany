-module(main_supervisor).

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
  SupFlags = #{strategy => rest_for_one, intensity => 1, period => 10},

  StatsGS = #{id => erlang:unique_integer(),
    start => {stats, start_link, []},
    restart => permanent,
    shutdown => brutal_kill,
    type => worker,
    modules => [stats]},

  PanelGS = #{id => erlang:unique_integer(),
    start => {panel, start_link, []},
    restart => permanent,
    shutdown => brutal_kill,
    type => worker,
    modules => [panel]},

  TaxiDBAccessSup = #{id => erlang:unique_integer(),
    start => {taxi_database_access_supervisor, start_link, []},
    restart => permanent,
    shutdown => brutal_kill,
    type => supervisor,
    modules => [taxi_database_access_supervisor]},

  {ok, {SupFlags, [StatsGS, PanelGS, TaxiDBAccessSup]}}.
