-module(taxi_database).

-behaviour(gen_server).

%% API
-export([start_link/0,
  stop/1,
  addTaxi/2,
  removeTaxi/2,
  getTaxis/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
  gen_server:start_link(?MODULE, [], []).

addTaxi(Server, TaxiPid) ->
  gen_server:call(Server, {add, TaxiPid}).

getTaxis(Server) ->
  gen_server:call(Server, get).

removeTaxi(Server, TaxiPid) ->
  gen_server:call(Server, {remove, TaxiPid}).

stop(Server) ->
  gen_server:stop(Server).


%%%===================================================================
%%% private functions
%%%===================================================================

init([]) ->
  {ok, []}.

handle_call({add, Pid}, _From, Taxis) ->
  {reply, {added, Pid}, [Pid | Taxis]};
handle_call(get, _From, Taxis) ->
  {reply, Taxis, Taxis};
handle_call({remove, Pid}, _From, Taxis) ->
  case lists:member(Pid, Taxis) of
    true -> {reply, {removed, Pid}, lists:delete(Pid, Taxis)};
    _ -> {reply, not_exists, Taxis}
  end.

handle_cast(_Msg, Taxis) ->
  {noreply, Taxis}.

handle_info(_Info, Taxis) ->
  {noreply, Taxis}.

terminate(_Reason, _Taxis) ->
  ok.

code_change(_OldVsn, Taxis, _Extra) ->
  {ok, Taxis}.
