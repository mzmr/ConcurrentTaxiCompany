-module(main_supervisor).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(Settings) ->
  utils:log_creating_process(?MODULE),
  supervisor:start_link({local, ?SERVER}, ?MODULE, Settings).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(Settings) when is_map(Settings)->
  SupFlags = #{strategy => rest_for_one, intensity => 1, period => 10},

  StatsGS = utils:create_child_spec(stats, worker),
  FirstLvlSup = utils:create_child_spec(first_level_supervisor, supervisor),
  SecondLvlSup = utils:create_child_spec(second_level_supervisor, supervisor),

  ChildSpecs = case maps:get(show_panel, Settings, true) of
                 true ->
                   PanelGS = utils:create_child_spec(panel, worker),
                   [StatsGS, PanelGS, FirstLvlSup, SecondLvlSup];
                 _ ->
                   [StatsGS, FirstLvlSup, SecondLvlSup]
               end,

  {ok, {SupFlags, ChildSpecs}}.
