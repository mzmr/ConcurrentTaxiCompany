-module(taxi_database_access).

-behaviour(gen_server).

-record(data, {taxi_databases, counter}).

%% API
-export([start_link/1,
  stop/1,
  get_taxi_db/1,
  get_all_taxi_db/1]).

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

start_link(TaxiDatabases) when is_list(TaxiDatabases) ->
  gen_server:start_link(?MODULE, TaxiDatabases, []).

get_taxi_db(Server) when is_pid(Server) ->
  gen_server:call(Server, get_taxi_db).

get_all_taxi_db(Server) when is_pid(Server) ->
  gen_server:call(Server, get_all_taxi_db).

stop(Server) when is_pid(Server) ->
  gen_server:stop(Server).


%%%===================================================================
%%% private functions
%%%===================================================================

init(TaxiDatabases) ->
  {ok, #data{taxi_databases=TaxiDatabases, counter=1}}.

handle_call(get_taxi_db, _From, Data = #data{taxi_databases=T, counter=C}) ->
  {reply, lists:nth(C, T), Data#data{counter = new_counter(length(T), C)}};

handle_call(get_all_taxi_db, _From, Data) ->
  {reply, Data#data.taxi_databases, Data}.

new_counter(ListSize, OldCounter) when ListSize == OldCounter -> 1;
new_counter(_ListSize, OldCounter) -> OldCounter + 1.

handle_cast(_Msg, Data) -> {noreply, Data}.

handle_info(_Info, Data) -> {noreply, Data}.

terminate(_Reason, _Data) -> ok.

code_change(_OldVsn, Data, _Extra) -> {ok, Data}.
