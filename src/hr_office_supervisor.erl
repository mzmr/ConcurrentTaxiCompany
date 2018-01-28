-module(hr_office_supervisor).

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
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  TaxiSupervisors = utils:get_supervisor_children_pids(first_level_supervisor),
  {ok, {SupFlags, create_offices(TaxiSupervisors)}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_offices(TaxiSupervisors) ->
  [utils:create_child_spec(hr_office, worker, [#taxi_sup{pid=P}])
    || P <- TaxiSupervisors].
