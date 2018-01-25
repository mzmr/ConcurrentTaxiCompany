-module(ordering_supervisor).

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
  SupFlags = #{strategy => rest_for_one, intensity => 1, period => 10},

  OrderReceiverSup = utils:create_child_spec(order_receiver_supervisor, supervisor),
  ClientSup = utils:create_child_spec(client_supervisor, supervisor),

  {ok, {SupFlags, [OrderReceiverSup, ClientSup]}}.
