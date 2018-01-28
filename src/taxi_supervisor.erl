-module(taxi_supervisor).

-behaviour(supervisor).

-include("app_config.hrl").

%% API
-export([start_link/0,
  add_taxi/1,
  get_taxis/1]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
  utils:log_creating_process(?MODULE),
  supervisor:start_link(?MODULE, []).

add_taxi(Server) when is_pid(Server) ->
  supervisor:start_child(Server, create_taxi_spec()).

get_taxis(Server) when is_pid(Server) ->
  utils:get_supervisor_children_pids(Server).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  SupFlags = #{strategy => one_for_one, intensity => 5, period => 5},
  ChildSpecs = create_taxis(?INITIAL_TAXI_PER_CITY_NUMBER),
  {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_taxis(NumberOfTaxis) when is_integer(NumberOfTaxis)
    andalso NumberOfTaxis > 0 ->
  [create_taxi_spec() || _ <- lists:seq(1, NumberOfTaxis)].

create_taxi_spec() ->
  utils:create_child_spec(taxi, worker, [utils:random_coords()]).