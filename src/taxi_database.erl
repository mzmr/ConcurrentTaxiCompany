-module(taxi_database).

-behaviour(gen_server).

%% API
-export([start_link/0,
  stop/1,
  add_taxi/2,
  remove_taxi/2,
  get_taxis/1]).

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

add_taxi(Server, TaxiPid) when is_pid(Server) andalso is_pid(TaxiPid) ->
  gen_server:call(Server, {add, TaxiPid}).

get_taxis(Server) when is_pid(Server) ->
  gen_server:call(Server, get).

remove_taxi(Server, TaxiPid) when is_pid(Server) andalso is_pid(TaxiPid) ->
  gen_server:call(Server, {remove, TaxiPid}).

stop(Server) when is_pid(Server) ->
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
  end;

handle_call(_Msg, _From, Taxis) ->
  {noreply, Taxis}.


handle_cast(_Msg, Taxis) -> {noreply, Taxis}.

handle_info(_Info, Taxis) -> {noreply, Taxis}.

terminate(_Reason, _Taxis) -> ok.

code_change(_OldVsn, Taxis, _Extra) -> {ok, Taxis}.
