-module(main_supervisor).

-behaviour(supervisor).

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

  StatsGS = utils:create_child_spec(stats, worker),
  PanelGS = utils:create_child_spec(panel, worker),
  FirstLvlSup = utils:create_child_spec(first_level_supervisor, supervisor),
  SecondLvlSup = utils:create_child_spec(second_level_supervisor, supervisor),

  {ok, {SupFlags, [StatsGS, PanelGS, FirstLvlSup, SecondLvlSup]}}.
