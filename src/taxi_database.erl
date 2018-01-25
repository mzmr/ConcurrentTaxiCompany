-module(taxi_database).

-behaviour(gen_server).

-include("app_config.hrl").

-record(data, {dbaccess, taxis=[]}).

%% API
-export([start_link/1,
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

start_link(#taxi_db_access{pid=P}) when is_pid(P) ->
  utils:log_creating_process(?MODULE),
  gen_server:start_link(?MODULE, P, []).

add_taxi(Server, TaxiPid) when is_pid(Server) andalso is_pid(TaxiPid) ->
  gen_server:cast(Server, {add, TaxiPid}).

get_taxis(Server) when is_pid(Server) ->
  gen_server:call(Server, get).

remove_taxi(Server, TaxiPid) when is_pid(Server) andalso is_pid(TaxiPid) ->
  gen_server:cast(Server, {remove, TaxiPid}).

stop(Server) when is_pid(Server) ->
  gen_server:stop(Server).


%%%===================================================================
%%% private functions
%%%===================================================================

init(TaxiDBAccess) ->
  taxi_database_access:add_taxi_db(TaxiDBAccess, self()),
  {ok, #data{dbaccess = TaxiDBAccess}}.

handle_call(get, _From, Data) ->
  {reply, Data#data.taxis, Data};

handle_call(_Msg, _From, Data) ->
  {noreply, Data}.


handle_cast({add, Pid}, D=#data{taxis=T}) ->
  {noreply, D#data{taxis=[Pid | T]}};

handle_cast({remove, Pid}, D=#data{taxis=T}) ->
  {noreply, D#data{taxis=lists:delete(Pid, T)}};

handle_cast(_Msg, Data) -> {noreply, Data}.

handle_info(_Info, Data) -> {noreply, Data}.

terminate(_Reason, #data{dbaccess=DBA}) ->
  taxi_database_access:remove_taxi_db(DBA, self()),
  ok.

code_change(_OldVsn, Data, _Extra) -> {ok, Data}.
