-module(taxi_database_supervisor).

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
  DBAccesses = utils:get_supervisor_children_pids(taxi_database_access_supervisor),
  Result = supervisor:start_link({local, ?SERVER}, ?MODULE, DBAccesses),
  create_taxis(?INITIAL_TAXI_PER_CITY_NUMBER, DBAccesses),
  Result.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(DBAccesses) ->
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  Databases = create_databases(DBAccesses),
  {ok, {SupFlags, Databases}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_databases(DBAccesses) ->
  lists:flatmap(fun(Pid) -> create_databases_for_city(Pid, ?TAXI_DB_PER_CITY) end,
    DBAccesses).

create_databases_for_city(DBAccess, DBPerCity) when is_integer(DBPerCity)
    andalso DBPerCity > 0 ->
  Database = utils:create_child_spec(taxi_database, worker,
    [#taxi_db_access{pid=DBAccess}]),
  [Database#{id => erlang:unique_integer()} || _X <- lists:seq(1,DBPerCity)].

create_taxis(NumberOfTaxis, DBAccesses) when is_integer(NumberOfTaxis)
    andalso NumberOfTaxis > 0 andalso is_list(DBAccesses) ->
  lists:foreach(
    fun(DBA) -> create_taxis_for_city(DBA, NumberOfTaxis) end,
    DBAccesses
  ).

create_taxis_for_city(DBAccess, NumberOfTaxis) ->
  lists:foreach(
    fun(_X) -> taxi:start_link(utils:random_coords(), #taxi_db_access{pid=DBAccess}) end,
    lists:seq(1, NumberOfTaxis)
  ).