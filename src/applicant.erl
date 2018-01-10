-module(applicant).

-include("app_config.hrl").

%% API
-export([start/1]).

%% private functions
-export([run_applicant/1]).


%%%===================================================================
%%% API
%%%===================================================================

start(HrOffice) ->
  timer:sleep(utils:random_number(?MIN_APPLICATION_INTERVAL, ?MAX_APPLICATION_INTERVAL)),
  spawn(?MODULE, run_applicant, [HrOffice]),
  start(HrOffice).


%%%===================================================================
%%% private functions
%%%===================================================================

run_applicant(HrOffice) ->
  hr_office:apply_for_job(HrOffice).