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
  Result = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
  case Result of
    {ok, _Pid} ->
      spawn(fun start_hr_office_supervisor/0),
      spawn(fun start_order_receiver_supervisor/0)
  end,
  Result.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  Databases = create_databases(),
  {ok, {SupFlags, Databases}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_databases() ->
  lists:flatmap(fun(Pid) -> create_databases_for_city(Pid, ?TAXI_DB_PER_CITY) end,
    utils:get_supervisor_children_pids(taxi_database_access_supervisor)).

create_databases_for_city(DBAccess, DBPerCity) when is_integer(DBPerCity)
    andalso DBPerCity > 0 ->
  Database = #{
    start => {taxi_database, start_link, [#taxi_db_access{pid=DBAccess}]},
    restart => permanent,
    shutdown => brutal_kill,
    type => worker,
    modules => [taxi_database] },
  [Database#{id => erlang:unique_integer()} || _X <- lists:seq(1,DBPerCity)].

start_hr_office_supervisor() ->
  HrOfficeSup = #{id => erlang:unique_integer(),
    start => {hr_office_supervisor, start_link, []},
    restart => permanent,
    shutdown => brutal_kill,
    type => supervisor,
    modules => [hr_office_supervisor]},
  supervisor:start_child(main_supervisor, HrOfficeSup).


start_order_receiver_supervisor() ->
  OrderReceiverSup = #{id => erlang:unique_integer(),
    start => {order_receiver_supervisor, start_link, []},
    restart => permanent,
    shutdown => brutal_kill,
    type => supervisor,
    modules => [order_receiver_supervisor]},
  supervisor:start_child(main_supervisor, OrderReceiverSup).
