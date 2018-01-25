-module(client_supervisor).

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
  SupFlags = #{strategy => one_for_one, intensity => 2, period => 5},
  ReceiversPids = utils:get_supervisor_children_pids(order_receiver_supervisor),
  Receivers = assign_clients_to_cities(ReceiversPids),
  ChildSpecs = lists:flatmap(fun({R,N}) -> create_clients(N, R) end, Receivers),
  {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

assign_clients_to_cities(OrderReceivers) ->
  WithWages = lists:map(fun(R) -> {R, rand:uniform()} end, OrderReceivers),
  WagesSum = lists:foldl(fun({_,W}, A) -> A + W end, 0, WithWages),
  Factor = ?CLIENTS_NUMBER / WagesSum,
  lists:map(fun({R,W}) -> {R, round(W*Factor)} end, WithWages).

create_clients(ClientsNumber, OrderReceiver) when is_integer(ClientsNumber)
    andalso ClientsNumber > 0 ->
  [utils:create_child_spec(client, worker, [#order_receiver{pid=OrderReceiver}])
    || _X <- lists:seq(1, ClientsNumber)];

create_clients(_ClientsNumber, _OrderReceiver) ->
  [].