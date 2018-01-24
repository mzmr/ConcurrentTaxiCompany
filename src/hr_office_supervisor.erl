-module(hr_office_supervisor).

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
  Result = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
  case Result of
    {ok, _Pid} -> spawn(fun start_applicant_supervisor/0)
  end,
  Result.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  DBAccessesPids = utils:get_supervisor_children_pids(taxi_database_access_supervisor),
  {ok, {SupFlags, create_offices(DBAccessesPids)}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_offices(DBAccesses) ->
  Office = #{
    restart => permanent,
    shutdown => brutal_kill,
    type => worker,
    modules => [hr_office] },
  Fun = fun(P) -> Office#{
          id => erlang:unique_integer(),
          start => {hr_office, start_link, [#taxi_db_access{pid=P}]} }
        end,
  lists:map(Fun, DBAccesses).

start_applicant_supervisor() ->
  ApplicantSup = #{id => erlang:unique_integer(),
    start => {applicant_supervisor, start_link, []},
    restart => permanent,
    shutdown => brutal_kill,
    type => supervisor,
    modules => [applicant_supervisor]},
  supervisor:start_child(main_supervisor, ApplicantSup).