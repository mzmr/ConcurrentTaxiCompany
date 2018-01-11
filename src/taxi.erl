-module(taxi).

-behaviour(gen_statem).

-include("app_config.hrl").

-record(stats, {driven_total=0, driven_with_clients=0, jobs_done=0}).
-record(client, {from, to}).
-record(data, {home, stats=#stats{}, client=#client{}, return_timer}).

%% API
-export([start_link/0,
  start_link/1,
  stop/1,
  get_position/1,
  offer_job/2,
  offer_job/1]).

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

%%%===================================================================
%%% API
%%%===================================================================

start_link(Home) when is_record(Home, coords) ->
  gen_statem:start_link(?MODULE, Home, []).

stop(Taxi) when is_pid(Taxi) ->
  gen_statem:stop(Taxi).

get_position(Taxi) when is_pid(Taxi) ->
  gen_statem:call(Taxi, get_position).

offer_job(Taxi, ClientCoords) when is_record(ClientCoords, coords)
    andalso is_pid(Taxi) ->
  gen_statem:call(Taxi, {take_job, ClientCoords}).

%% for testing only
start_link() ->
  gen_statem:start_link(?MODULE, #coords{x = 5, y = 6}, []).

offer_job(Taxi) when is_pid(Taxi) ->
  gen_statem:call(Taxi, {take_job, #coords{x=80,y=60}}).

%%%===================================================================
%%% private functions
%%%===================================================================

init(Home) ->
  stats:created_taxi(),
  process_flag(trap_exit, true),
  {ok, waiting, #data{home=Home}}.

callback_mode() ->
  [state_functions, state_enter].


waiting(enter, OldState, Data) when OldState == inactive
    orelse OldState == waiting ->
  entered_state(waiting, OldState),
  NewData = Data#data{stats=#stats{}, client=#client{}, return_timer=undefined},
  Timeout = {{timeout,end_of_work_tm}, ?WORK_TIME, finish_work},
  {keep_state, NewData, [Timeout]};

waiting(enter, OldState, _Data) ->
  entered_state(waiting, OldState),
  keep_state_and_data;

waiting({timeout,end_of_work_tm}, finish_work, Data) ->
  finish_work(Data);

waiting({call, From}, {take_job, ClientCoords}, Data=#data{home=H}) ->
  accept_job(H, ClientCoords, From, Data);

waiting({call, From}, get_position, #data{home=H}) ->
  {keep_state_and_data, [{reply, From, H}]};

waiting(EventType, EventContent, _Data) ->
  unsupported_event(EventType, EventContent).


inactive(enter, OldState, _Data) ->
  entered_state(inactive, OldState),
  send_raport(),
  keep_state_and_data;

inactive({timeout,end_of_break_tm}, start_work, Data) ->
  {next_state, waiting, Data};

inactive({call, From}, get_position, _Data) ->
  reply_busy(From);

inactive({call, From}, {take_job, _ClientCoords}, _Data) ->
  reject_job(From);

inactive(EventType, EventContent, _Data) ->
  unsupported_event(EventType, EventContent).


driving_to_client(enter, OldState, _Data) ->
  entered_state(driving_to_client, OldState),
  keep_state_and_data;

driving_to_client({call, From}, get_position, _Data) ->
  reply_busy(From);

driving_to_client({call, From}, {take_job, _ClientCoords}, _Data) ->
  reject_job(From);

driving_to_client({timeout,driven_to_client_tm}, {take_client, Driven},
    Data = #data{stats=S, client=C}) ->
  Target = utils:random_coords(C#client.from, ?MAX_JOB_LENGTH),
  NewData = Data#data{
    stats = S#stats{driven_total = S#stats.driven_total + Driven},
    client = C#client{to = Target}},
  Distance = utils:distance(C#client.from, Target),
  DriveTime = time_distance(Distance),
  Timeout = {{timeout,driven_with_client_tm}, DriveTime, {go_home,Distance}},
  {next_state, driving_with_client, NewData, [Timeout]};

driving_to_client({timeout,end_of_work_tm}, finish_work, Data) ->
  {keep_state, Data, [postpone]};

driving_to_client(EventType, EventContent, _Data) ->
  unsupported_event(EventType, EventContent).


driving_with_client(enter, OldState, _Data) ->
  entered_state(driving_with_client, OldState),
  keep_state_and_data;

driving_with_client({call, From}, get_position, _Data) ->
  reply_busy(From);

driving_with_client({call, From}, {take_job, _ClientCoords}, _Data) ->
  reject_job(From);

driving_with_client({timeout,driven_with_client_tm}, {go_home,Driven},
    Data=#data{stats=S, client=C}) ->
  stats:finished_order(),
  Distance = utils:distance(C#client.to, Data#data.home),
  DriveTime = time_distance(Distance),
  NewData = Data#data{
    stats = S#stats{
      driven_total = S#stats.driven_total + Driven,
      driven_with_clients = S#stats.driven_with_clients + Driven,
      jobs_done = S#stats.jobs_done + 1},
    client = C#client{from = undefined},
    return_timer = erlang:start_timer(DriveTime, self(), {start_waiting,Distance})},
  {next_state, driving_from_client, NewData};

driving_with_client({timeout,end_of_work_tm}, finish_work, Data) ->
  {keep_state, Data, [postpone]};

driving_with_client(EventType, EventContent, _Data) ->
  unsupported_event(EventType, EventContent).


driving_from_client(enter, OldState, _Data) ->
  entered_state(driving_from_client, OldState),
  keep_state_and_data;

driving_from_client({call, From}, get_position, Data) ->
  {keep_state_and_data, [{reply, From, current_position(Data)}]};

driving_from_client({call, From}, {take_job, ClientCoords}, Data) ->
  accept_job(current_position(Data), ClientCoords, From, Data);

driving_from_client(info, {timeout,T,{start_waiting, Driven}},
    Data=#data{stats=S, return_timer=T}) ->
  NewData = Data#data{
    stats = S#stats{driven_total = S#stats.driven_total + Driven},
    return_timer = undefined},
  {next_state, waiting, NewData};

driving_from_client({timeout,end_of_work_tm}, finish_work,
    Data=#data{home=H, stats=S, client=C, return_timer=T}) ->
  Distance = utils:distance(C#client.to, H),
  Driven = Distance * percentage_of_return(Distance, T),
  erlang:cancel_timer(T, [{async, true}, {info, false}]),
  NewData = Data#data{
    stats = S#stats{driven_total = S#stats.driven_total + Driven},
    client = #client{},
    return_timer = undefined},
  finish_work(NewData);

driving_from_client(EventType, EventContent, _Data) ->
  unsupported_event(EventType, EventContent).


reply_busy(From) ->
  {keep_state_and_data, [{reply, From, busy}]}.

reject_job(From) ->
  {keep_state_and_data, [{reply, From, job_rejected}]}.

send_raport() ->
  io:format("[taxi] Sending the raport~n").

terminate(_Reason, State, _Data) ->
  % dodać obsługę zakończenia życia procesu w czasie jazdy z klientem
  State =/= inactive andalso send_raport(),
  stats:terminated_taxi(),
  ok.

code_change(_Vsn, State, Data, _Extra) ->
  {ok, State, Data}.

time_distance(Distance) ->
  round(Distance / ?SPEED * 1000).

unsupported_event(EventType, EventContent) ->
  io:format("Unsupported event:~nType: ~p~nContent: ~p~n", [EventType, EventContent]),
  keep_state_and_data.

finish_work(Data) ->
  Timeout = {{timeout,end_of_break_tm}, ?BREAK_TIME, start_work},
  {next_state, inactive, Data, [Timeout]}.

current_position(#data{home=H, client=#client{to=To}, return_timer=Tmr}) ->
  P = percentage_of_return(utils:distance(To, H), Tmr),
  CurrentX = (1 - P)*To#coords.x + P*H#coords.x,
  CurrentY = (1 - P)*To#coords.y + P*H#coords.y,
  #coords{x=CurrentX, y=CurrentY}.

percentage_of_return(Distance, Timer) ->
  FullTime = time_distance(Distance),
  1 - erlang:read_timer(Timer) / FullTime.

accept_job(MyPos, ClientPos, From, Data=#data{client=C}) ->
  stats:started_order(),
  Distance = utils:distance(MyPos, ClientPos),
  DriveTime = time_distance(Distance),
  NewData = Data#data{client=C#client{from=ClientPos}},
  Timeout = {{timeout,driven_to_client_tm}, DriveTime, {take_client,Distance}},
  Reply = {reply, From, job_accepted},
  {next_state, driving_to_client, NewData, [Reply, Timeout]}.

entered_state(NewState, OldState) ->
  io:format("[taxi] Entering ~p state~n", [NewState]),
  stats:changed_taxi_state(NewState, OldState).