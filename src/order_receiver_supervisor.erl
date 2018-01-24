-module(order_receiver_supervisor).

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
    {ok, _Pid} -> spawn(fun start_client_supervisor/0)
  end,
  Result.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  Receivers = create_receivers(),

  {ok, {SupFlags, Receivers}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_receivers() ->
  lists:flatmap(fun(Pid) -> create_receivers_for_city(Pid, ?RECEIVERS_PER_CITY) end,
    utils:get_supervisor_children_pids(taxi_database_access_supervisor)).

create_receivers_for_city(DBAccess, RecPerCity) when is_integer(RecPerCity)
    andalso RecPerCity > 0 ->
  Receiver = #{
    start => {order_receiver, start_link, [#taxi_db_access{pid=DBAccess}]},
    restart => permanent,
    shutdown => brutal_kill,
    type => worker,
    modules => [order_receiver] },
  [Receiver#{id => erlang:unique_integer()} || _X <- lists:seq(1,RecPerCity)].

start_client_supervisor() ->
  OrderReceiverSup = #{id => erlang:unique_integer(),
    start => {order_receiver_supervisor, start_link, []},
    restart => permanent,
    shutdown => brutal_kill,
    type => supervisor,
    modules => [order_receiver_supervisor]},
  supervisor:start_child(main_supervisor, OrderReceiverSup).
