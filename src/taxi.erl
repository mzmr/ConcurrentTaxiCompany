-module(taxi).

-behaviour(gen_statem).

-record(coords, {x, y}).
-record(data, {home, driven_total, driven_with_clients, jobs_done, client_coords, destination_coords}).

-define(WORK_TIME, 5000).
-define(BREAK_TIME, 5000).
-define(SPEED, 11). % defined in meters per second

%% API
-export([start_link/0, start_link/1, stop/0, get_position/0, offer_job/1, offer_job/0]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).

%% private functions
-export([waiting/3, inactive/3, on_the_way_to_client/3]).

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
  gen_statem:call(?NAME, {pick_client_up, ClientCoords}).

%% for testing only
start_link() ->
  gen_statem:start_link({local,?NAME}, ?MODULE, #coords{x = 5, y = 6}, []).

offer_job() ->
  gen_statem:call(?NAME, {pick_client_up, #coords{x=100,y=80}}).


%%%===================================================================
%%% private functions
%%%===================================================================

init(Home) ->
  process_flag(trap_exit, true),
  Data = #data{ home=Home },
  {ok, waiting, Data}.

callback_mode() ->
  [state_functions, state_enter].

waiting(enter, OldState, Data) when OldState == inactive orelse OldState == waiting ->
  io:format("[taxi] Entering waiting state and clearing data~n"),
  NewData = Data#data{ driven_total=0, driven_with_clients=0, jobs_done=0, client_coords=0, destination_coords=0 },
  {keep_state, NewData, [{{timeout,end_of_work_tm},?WORK_TIME,finish_work}]};
waiting(enter, _OldState, _Data) ->
  io:format("[taxi] Entering waiting state~n"),
  keep_state_and_data;
waiting({timeout,end_of_work_tm}, finish_work, Data) ->
  {next_state, inactive, Data, [{{timeout,end_of_break_tm},?BREAK_TIME,start_work}]};
waiting({call, From}, {pick_client_up, ClientCoords}, Data) ->
  DriveTime = round(distance(Data#data.home, ClientCoords) / ?SPEED),
  NewData = Data#data{ client_coords=ClientCoords },
  {next_state, on_the_way_to_client, NewData, [{{timeout,got_to_client_tm},DriveTime,take_client}, {reply,From,job_accepted}]};
waiting({call, From}, get_position, #data{ home = Home }) ->
  {keep_state_and_data, [{reply,From,Home}]};
waiting(_EventType, _EventContent, _Data) ->
  keep_state_and_data.

inactive(enter, _OldState, _Data) ->
  io:format("[taxi] Entering inactive state~n"),
  send_raport(),
  keep_state_and_data;
inactive({timeout,end_of_break_tm}, start_work, Data) ->
  {next_state, waiting, Data};
inactive({call, From}, _EventContent, _Data) ->
  reply_busy(From);
inactive(_EventType, _EventContent, _Data) ->
  keep_state_and_data.

on_the_way_to_client(enter, _OldState, _Data) ->
  io:format("[taxi] Entering on_the_way_to_client state~n"),
  keep_state_and_data;
on_the_way_to_client({call, From}, _EventContent, _Data) ->
  reply_busy(From);
on_the_way_to_client(_EventType, _EventContent, _Data) ->
  keep_state_and_data.

reply_busy(From) ->
  {keep_state_and_data, [{reply,From,busy}]}.

send_raport() ->
  io:format("[taxi] Sending the raport~n").

terminate(_Reason, State, _Data) ->
  % dodać obsługę zakończenia w czasie jazdy z klientem
  State =/= inactive andalso send_raport(),
  ok.

code_change(_Vsn, State, Data, _Extra) ->
  {ok,State,Data}.

distance(#coords{x=X1, y=Y1}, #coords{x=X2, y=Y2}) ->
  math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).