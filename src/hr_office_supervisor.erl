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
  DBAccessesPids = utils:get_supervisor_children_pids(taxi_database_access_supervisor),
  {ok, {SupFlags, create_offices(DBAccessesPids)}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_offices(DBAccesses) ->
  Fun = fun(P) ->
          utils:create_child_spec(hr_office, worker, [#taxi_db_access{pid=P}])
        end,
  lists:map(Fun, DBAccesses).
