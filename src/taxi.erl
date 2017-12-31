-module(taxi).

-behaviour(gen_statem).

-include("app_interface.hrl").

-record(stats, {driven_total=0, driven_with_clients=0, jobs_done=0}).
-record(client, {my_start, from, to}).
-record(data, {home, stats=#stats{}, client=#client{}}).

-define(WORK_TIME, 7000).
-define(BREAK_TIME, 7000).
-define(SPEED, 11). % meters per second
-define(MAX_JOB_LENGTH, 5000). % meters

%% API
-export([start_link/0,
  start_link/1,
  stop/0,
  get_position/0,
  offer_job/1,
  offer_job/0]).

%% gen_statem callbacks
-export([init/1,
  callback_mode/0,
  terminate/3,
  code_change/4]).

%% private functions
-export([waiting/3,
  inactive/3,
  driving_to_client/3,
  driving_with_client/3,
  driving_from_client/3]).

-define(NAME, taxi).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Home) when is_record(Home, coords) ->
  gen_statem:start_link({local,?NAME}, ?MODULE, Home, []).

stop() ->
  gen_statem:stop(?NAME).

get_position() ->
  gen_statem:call(?NAME, get_position).

offer_job(ClientCoords) when is_record(ClientCoords, coords) ->
  gen_statem:call(?NAME, {take_job, ClientCoords}).

%% for testing only
start_link() ->
  gen_statem:start_link({local,?NAME}, ?MODULE, #coords{x = 5, y = 6}, []).

offer_job() ->
  gen_statem:call(?NAME, {take_job, #coords{x=80,y=60}}).


%%%===================================================================
%%% private functions
%%%===================================================================

init(Home) ->
  process_flag(trap_exit, true),
  {ok, waiting, #data{home=Home}}.

callback_mode() ->
  [state_functions, state_enter].


waiting(enter, OldState, Data)
  when OldState == inactive
  orelse OldState == waiting ->
  io:format("[taxi] Entering waiting state and clearing data~n"),
  NewData = Data#data{stats=#stats{}},
  Timeout = {{timeout,end_of_work_tm}, ?WORK_TIME, finish_work},
  {keep_state, NewData, [Timeout]};

waiting(enter, _OldState, _Data) ->
  io:format("[taxi] Entering waiting state~n"),
  keep_state_and_data;

waiting({timeout,end_of_work_tm}, finish_work, Data) ->
  Timeout = {{timeout,end_of_break_tm}, ?BREAK_TIME, start_work},
  {next_state, inactive, Data, [Timeout]};

waiting({call, From}, {take_job, ClientCoords}, Data=#data{home=Home}) ->
  Distance = utils:distance(Home, ClientCoords),
  DriveTime = time_distance(Distance),
  NewData = Data#data.client#client{my_start=Home, from=ClientCoords},
  Timeout = {{timeout,driven_to_client_tm}, DriveTime, {take_client,Distance}},
  Reply = {reply, From, job_accepted},
  {next_state, driving_to_client, NewData, [Timeout, Reply]};

waiting({call, From}, get_position, #data{home=Home}) ->
  {keep_state_and_data, [{reply, From, Home}]};

waiting(_EventType, _EventContent, _Data) ->
  keep_state_and_data.


inactive(enter, _OldState, _Data) ->
  io:format("[taxi] Entering inactive state~n"),
  send_raport(),
  keep_state_and_data;

inactive({timeout,end_of_break_tm}, start_work, Data) ->
  {next_state, waiting, Data};

inactive({call, From}, get_position, _Data) ->
  reply_busy(From);

inactive({call, From}, {take_job, _ClientCoords}, _Data) ->
  reply_reject_job(From);

inactive(_EventType, _EventContent, _Data) ->
  keep_state_and_data.


driving_to_client(enter, _OldState, _Data) ->
  io:format("[taxi] Entering driving_to_client state~n"),
  keep_state_and_data;

driving_to_client({call, From}, get_position, _Data) ->
  reply_busy(From);

driving_to_client({call, From}, {take_job, _ClientCoords}, _Data) ->
  reply_reject_job(From);

driving_to_client({timeout,driven_to_client_tm}, {take_client, Driven},
    Data=#data{stats=S, client=C#client{from=From}}) ->
  Destination = get_destination(From),
  NewData = Data#data{
    stats = S#stats{driven_total=S#stats.driven_total + Driven},
    client = C#client{to=Destination}},
  Distance = utils:distance(From, Destination),
  DriveTime = time_distance(Distance),
  Timeout = {{timeout,driven_with_client_tm}, DriveTime, {go_home,Distance}},
  {next_state, driving_with_client, NewData, [Timeout]};

driving_to_client(_EventType, _EventContent, _Data) ->
  keep_state_and_data.


driving_with_client(enter, _OldState, _Data) ->
  io:format("[taxi] Entering driving_with_client state~n"),
  keep_state_and_data;

driving_with_client({call, From}, get_position, _Data) ->
  reply_busy(From);

driving_with_client({call, From}, {take_job, _ClientCoords}, _Data) ->
  reply_reject_job(From);

driving_with_client({timeout,driven_with_client_tm}, {go_home,Driven},
    Data=#data{stats=S}) ->
  NewData = Data#data{stats = S#stats{
    driven_total = S#stats.driven_total + Driven,
    driven_with_clients = S#stats.driven_with_clients + Driven,
    jobs_done = S#stats.jobs_done + 1}},
  Distance = utils:distance(Data#data.client#client.to, Data#data.home),
  DriveTime = time_distance(Distance),
  Timeout = {{timeout,driven_from_client_tm}, DriveTime, {start_waiting,Distance}},
  {next_state, driving_from_client, NewData, [Timeout]};

driving_with_client(_EventType, _EventContent, _Data) ->
  keep_state_and_data.


driving_from_client(enter, _OldState, _Data) ->
  io:format("[taxi] Entering driving_from_client state~n"),
  keep_state_and_data;

driving_from_client({call, From}, get_position, _Data) ->
  reply_busy(From);

driving_from_client({call, From}, {take_job, _ClientCoords}, _Data) ->
  reply_reject_job(From);

driving_from_client({timeout,driven_from_client_tm}, {start_waiting,Driven},
    Data=#data{stats=S}) ->
  NewData = Data#data{stats = S#stats{
    driven_total = S#stats.driven_total + Driven}},
  {next_state, waiting, NewData};

driving_from_client(_EventType, _EventContent, _Data) ->
  keep_state_and_data.


reply_busy(From) ->
  {keep_state_and_data, [{reply, From, busy}]}.

reply_reject_job(From) ->
  {keep_state_and_data, [{reply, From, job_rejected}]}.

send_raport() ->
  io:format("[taxi] Sending the raport~n").

terminate(_Reason, State, _Data) ->
  % dodać obsługę zakończenia życia procesu w czasie jazdy z klientem
  State =/= inactive andalso send_raport(),
  ok.

code_change(_Vsn, State, Data, _Extra) ->
  {ok,State,Data}.

time_distance(Start, End) ->
  round(utils:distance(Start, End) / ?SPEED).

time_distance(Distance) ->
  round(Distance / ?SPEED).

get_destination(#coords{x=X, y=Y}) ->
  TargetX = abs(X + ?MAX_JOB_LENGTH * (2 - rand:uniform(3))),
  TargetY = abs(Y + ?MAX_JOB_LENGTH * (2 - rand:uniform(3))),
  #coords{x=TargetX, y=TargetY}.


% opoznic timeout zwiazany z koncem pracy do czasu wejscia w stan waiting