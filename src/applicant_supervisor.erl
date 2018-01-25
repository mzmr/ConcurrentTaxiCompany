-module(applicant_supervisor).

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
  HrOffices = utils:get_supervisor_children_pids(hr_office_supervisor),
  Offices = assign_applicants_to_cities(HrOffices),
  ChildSpecs = lists:flatmap(fun({O,N}) -> create_applicants(N, O) end, Offices),
  {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

assign_applicants_to_cities(HrOffices) ->
  WithWages = lists:map(fun(O) -> {O, rand:uniform()} end, HrOffices),
  WagesSum = lists:foldl(fun({_,W}, A) -> A + W end, 0, WithWages),
  Factor = ?APPLICANTS_NUMBER / WagesSum,
  lists:map(fun({O,W}) -> {O, round(W*Factor)} end, WithWages).

create_applicants(ApplicantsNumber, HrOffice) when is_integer(ApplicantsNumber)
    andalso ApplicantsNumber > 0 ->
  Applicant = #{
    start => {applicant, start_link, [#hr_office{pid=HrOffice}]},
    restart => permanent,
    shutdown => brutal_kill,
    type => worker,
    modules => [applicant]},
  [Applicant#{id => erlang:unique_integer()} || _X <- lists:seq(1, ApplicantsNumber)];

create_applicants(_ApplicantsNumber, _HrOffice) ->
  [].