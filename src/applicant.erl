-module(applicant).

-behaviour(gen_server).

-include("app_config.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

%% private functions
-export([run_applicant/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(#hr_office{pid=O}) when is_pid(O) ->
  utils:log_creating_process(?MODULE),
  gen_server:start_link(?MODULE, O, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init(HrOffice) ->
  Interval = utils:random_number(?MIN_APPLICATION_INTERVAL, ?MAX_APPLICATION_INTERVAL),
  erlang:send_after(Interval, self(), apply_for_job),
  {ok, HrOffice}.

handle_call(_Msg, _From, HrOffice) ->
  {noreply, HrOffice}.

handle_cast(_Msg, HrOffice) -> {noreply, HrOffice}.

handle_info(apply_for_job, HrOffice) ->
  spawn(?MODULE, run_applicant, [HrOffice]),
  Interval = utils:random_number(?MIN_APPLICATION_INTERVAL, ?MAX_APPLICATION_INTERVAL),
  erlang:send_after(Interval, self(), apply_for_job),
  {noreply, HrOffice};

handle_info(_Info, HrOffice) -> {noreply, HrOffice}.

terminate(_Reason, _HrOffice) -> ok.

code_change(_OldVsn, HrOffice, _Extra) -> {ok, HrOffice}.

%%%===================================================================
%%% private functions
%%%===================================================================

run_applicant(HrOffice) ->
  hr_office:apply_for_job(HrOffice).