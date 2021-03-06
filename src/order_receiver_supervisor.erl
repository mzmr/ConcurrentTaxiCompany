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
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

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
    utils:get_supervisor_children_pids(first_level_supervisor)).

create_receivers_for_city(TaxiSup, RecPerCity) when is_integer(RecPerCity)
    andalso RecPerCity > 0 ->
  [utils:create_child_spec(order_receiver, worker, [#taxi_sup{pid=TaxiSup}])
    || _ <- lists:seq(1, RecPerCity)].
