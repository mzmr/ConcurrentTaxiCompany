-module(second_level_supervisor).

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
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 10},

  OrderingSup = utils:create_child_spec(ordering_supervisor, supervisor),
  HiringSup = utils:create_child_spec(hiring_supervisor, supervisor),

  {ok, {SupFlags, [OrderingSup, HiringSup]}}.
