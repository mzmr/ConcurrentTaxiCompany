-module(taxi_database_access).

-behaviour(gen_server).

-include("app_config.hrl").

-record(data, {taxi_databases, counter}).

%% API
-export([start_link/0,
  stop/1,
  get_taxi_db/1,
  get_all_taxi_db/1,
  add_taxi_db/2,
  remove_taxi_db/2]).

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
  utils:log_creating_process(?MODULE),
  gen_server:start_link(?MODULE, [], []).

get_taxi_db(Server) when is_pid(Server) ->
  gen_server:call(Server, get_taxi_db).

get_all_taxi_db(Server) when is_pid(Server) ->
  gen_server:call(Server, get_all_taxi_db).

add_taxi_db(Server, TaxiDBPid) when is_pid(Server) andalso is_pid(TaxiDBPid) ->
  gen_server:cast(Server, {add_taxi_db, TaxiDBPid}).

remove_taxi_db(Server, TaxiDBPid) when is_pid(Server) andalso is_pid(TaxiDBPid) ->
  gen_server:cast(Server, {remove_taxi_db, TaxiDBPid}).

stop(Server) when is_pid(Server) ->
  gen_server:stop(Server).


%%%===================================================================
%%% private functions
%%%===================================================================

init([]) ->
  {ok, #data{taxi_databases=[], counter=1}}.


handle_call(get_taxi_db, _From, D=#data{taxi_databases=T, counter=C}) ->
  {reply, #taxi_db{pid = lists:nth(C, T)},
    D#data{counter = new_counter(length(T), C)}};

handle_call(get_all_taxi_db, _From, Data) ->
  {reply, Data#data.taxi_databases, Data}.


new_counter(ListSize, OldCounter) when ListSize == OldCounter -> 1;
new_counter(_ListSize, OldCounter) -> OldCounter + 1.


handle_cast({add_taxi_db, TaxiDBPid}, D=#data{taxi_databases=T}) ->
  {noreply, D#data{taxi_databases = [TaxiDBPid | T]}};

handle_cast({remove_taxi_db, TaxiDBPid}, D=#data{taxi_databases=T}) ->
  {noreply, D#data{taxi_databases = lists:delete(TaxiDBPid, T)}};

handle_cast(_Msg, Data) -> {noreply, Data}.

handle_info(_Info, Data) -> {noreply, Data}.

terminate(_Reason, _Data) -> ok.

code_change(_OldVsn, Data, _Extra) -> {ok, Data}.
