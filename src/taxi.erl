-module(taxi).

-behaviour(gen_statem).

-include("app_interface.hrl").

-record(stats, {driven_total=0, driven_with_clients=0, jobs_done=0}).
-record(client, {my_start, from, to}).
-record(data, {home, stats=#stats{}, client=#client{}, return_timer}).

-define(WORK_TIME, 70000).
-define(BREAK_TIME, 7000).
-define(SPEED, 11). % meters per second
-define(MAX_JOB_LENGTH, 150). % meters

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
  finish_work(Data);

waiting({call, From}, {take_job, ClientCoords}, Data=#data{home=H, client=C}) ->
  Distance = utils:distance(H, ClientCoords),
  DriveTime = time_distance(Distance),
  NewData = Data#data{client = C#client{my_start=H, from=ClientCoords}},
  Timeout = {{timeout,driven_to_client_tm}, DriveTime, {take_client,Distance}},
  Reply = {reply, From, job_accepted},
  {next_state, driving_to_client, NewData, [Reply, Timeout]};

waiting({call, From}, get_position, #data{home=Home}) ->
  {keep_state_and_data, [{reply, From, Home}]};

waiting(EventType, EventContent, _Data) ->
  unsupported_event(EventType, EventContent).


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

inactive(EventType, EventContent, _Data) ->
  unsupported_event(EventType, EventContent).


driving_to_client(enter, _OldState, _Data) ->
  io:format("[taxi] Entering driving_to_client state~n"),
  keep_state_and_data;

driving_to_client({call, From}, get_position, _Data) ->
  reply_busy(From);

driving_to_client({call, From}, {take_job, _ClientCoords}, _Data) ->
  reply_reject_job(From);

driving_to_client({timeout,driven_to_client_tm}, {take_client, Driven},
    Data = #data{stats=S, client=C}) ->
  Destination = get_destination(C#client.from),
  NewData = Data#data{
    stats = S#stats{driven_total = S#stats.driven_total + Driven},
    client = C#client{to = Destination}},
  Distance = utils:distance(C#client.from, Destination),
  DriveTime = time_distance(Distance),
  Timeout = {{timeout,driven_with_client_tm}, DriveTime, {go_home,Distance}},
  {next_state, driving_with_client, NewData, [Timeout]};

driving_to_client({timeout,end_of_work_tm}, finish_work, Data) ->
  {keep_state, Data, [postpone]};

driving_to_client(EventType, EventContent, _Data) ->
  unsupported_event(EventType, EventContent).


driving_with_client(enter, _OldState, _Data) ->
  io:format("[taxi] Entering driving_with_client state~n"),
  keep_state_and_data;

driving_with_client({call, From}, get_position, _Data) ->
  reply_busy(From);

driving_with_client({call, From}, {take_job, _ClientCoords}, _Data) ->
  reply_reject_job(From);

driving_with_client({timeout,driven_with_client_tm}, {go_home,Driven},
    Data=#data{stats=S, client=C}) ->
  Distance = utils:distance(C#client.to, Data#data.home),
  DriveTime = time_distance(Distance),
  NewData = Data#data{
    stats = S#stats{
      driven_total = S#stats.driven_total + Driven,
      driven_with_clients = S#stats.driven_with_clients + Driven,
      jobs_done = S#stats.jobs_done + 1},
    return_timer = erlang:start_timer(DriveTime, self(), {start_waiting,Distance})},
  {next_state, driving_from_client, NewData};

driving_with_client({timeout,end_of_work_tm}, finish_work, Data) ->
  {keep_state, Data, [postpone]};

driving_with_client(EventType, EventContent, _Data) ->
  unsupported_event(EventType, EventContent).


driving_from_client(enter, _OldState, _Data) ->
  io:format("[taxi] Entering driving_from_client state~n"),
  keep_state_and_data;

driving_from_client({call, From}, get_position, Data) ->
  {keep_state_and_data, [{reply, From, current_position(Data)}]};

driving_from_client({call, From}, {take_job, _ClientCoords}, _Data) ->
  reply_reject_job(From);

driving_from_client(info, {timeout,T,{start_waiting, Driven}},
    Data=#data{stats=S, return_timer=T}) ->
  NewData = Data#data{
    stats = S#stats{driven_total = S#stats.driven_total + Driven},
    return_timer = undefined},
  {next_state, waiting, NewData};

driving_from_client({timeout,end_of_work_tm}, finish_work,
    Data=#data{home=H, stats=S, client=C, return_timer=T}) ->
  P = percentage_of_return(C#client.to, H, T),
  Driven = P * utils:distance(C#client.to, H),
  erlang:cancel_timer(T, [{async, true}]),
  NewData = Data#data{
    stats = S#stats{driven_total = S#stats.driven_total + Driven},
    return_timer = undefined},
  finish_work(NewData);

driving_from_client(EventType, EventContent, _Data) ->
  unsupported_event(EventType, EventContent).


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
  {ok, State, Data}.

time_distance(Distance) ->
  round(Distance / ?SPEED * 1000).

get_destination(#coords{x=X, y=Y}) ->
  TargetX = abs(X + ?MAX_JOB_LENGTH * (2 - rand:uniform(3))),
  TargetY = abs(Y + ?MAX_JOB_LENGTH * (2 - rand:uniform(3))),
  #coords{x=TargetX, y=TargetY}.

unsupported_event(EventType, EventContent) ->
  io:format("Unsupported event:~nType: ~p~nContent: ~p~n", [EventType, EventContent]),
  keep_state_and_data.

finish_work(Data) ->
  Timeout = {{timeout,end_of_break_tm}, ?BREAK_TIME, start_work},
  {next_state, inactive, Data, [Timeout]}.

current_position(#data{home=H, client=#client{to=To}, return_timer=Tmr}) ->
  P = percentage_of_return(To, H, Tmr),
  CurrentX = (1 - P)*To#coords.x + P*H#coords.x,
  CurrentY = (1 - P)*To#coords.y + P*H#coords.y,
  #coords{x=CurrentX, y=CurrentY}.

percentage_of_return(From, To, Timer) ->
  FullTime = time_distance(utils:distance(From, To)),
  1 - erlang:read_timer(Timer) / FullTime.