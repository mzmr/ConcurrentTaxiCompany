-module(hr_office).

-behaviour(gen_server).

-include("app_config.hrl").

%% API
-export([start_link/1,
  stop/1,
  apply_for_job/1]).

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

start_link(#taxi_sup{pid=P}) when is_pid(P) ->
  utils:log_creating_process(?MODULE),
  gen_server:start_link(?MODULE, P, []).

apply_for_job(Office) when is_pid(Office) ->
  gen_server:call(Office, apply_for_job).

stop(Office) when is_pid(Office) ->
  gen_server:stop(Office).


%%%===================================================================
%%% private functions
%%%===================================================================

init(TaxiSup) ->
  {ok, TaxiSup}.

handle_call(apply_for_job, _From, TaxiSup) ->
  Rand = rand:uniform() * 100,
  case Rand < ?CHANCE_OF_ACCEPTANCE of
    true ->
      taxi_supervisor:add_taxi(TaxiSup),
      {reply, application_accepted, TaxiSup};
    false ->
      {reply, application_rejected, TaxiSup}
  end;

handle_call(Msg, _From, TaxiSup) -> {reply, {unsupported_message, Msg}, TaxiSup}.

handle_cast(_Msg, TaxiSup) -> {noreply, TaxiSup}.

handle_info(_Info, TaxiSup) -> {noreply, TaxiSup}.

terminate(_Reason, _TaxiSup) -> ok.

code_change(_OldVsn, TaxiSup, _Extra) -> {ok, TaxiSup}.
