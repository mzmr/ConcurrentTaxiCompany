-module(client_supervisor).

-behaviour(supervisor).

-include("app_config.hrl").

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link(OrderReceivers) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, OrderReceivers).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(OrderReceivers) ->
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  Receivers = assign_clients(OrderReceivers),
  ChildSpecs = lists:flatmap(fun({R,N}) -> create_clients(N, R) end, Receivers),
  {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

assign_clients(OrderReceivers) ->
  WithWages = lists:map(fun(R) -> {R, rand:uniform()} end, OrderReceivers),
  WagesSum = lists:foldl(fun({_,W}, A) -> A + W end, 0, WithWages),
  Factor = ?CLIENTS_NUMBER / WagesSum,
  lists:map(fun({R,W}) -> {R, round(W*Factor)} end, WithWages).

create_clients(0, _OrderReceiver) ->
  [];

create_clients(Number, OrderReceiver) when is_integer(Number) ->
  Client = #{id => erlang:system_time(),
    start => {client, start, [OrderReceiver]},
    restart => permanent,
    shutdown => brutal_kill,
    type => worker,
    modules => [client]},
  [Client | create_clients(Number - 1, OrderReceiver)].