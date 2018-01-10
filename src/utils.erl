-module(utils).

-include("app_config.hrl").

%% API
-export([distance/2,
  concurrent_map/2,
  random_coords/0,
  random_coords/2,
  random_number/2]).

% private
-export([run_function/3]).


%%%===================================================================
%%% API
%%%===================================================================

distance(#coords{x=X1, y=Y1}, #coords{x=X2, y=Y2}) ->
  math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

concurrent_map(Function, List) ->
  try run_processes(Function, List) of
    X -> X
  catch
    error:Error -> {error, Error};
    exit:Exit -> {exit, Exit}
  end.

random_coords() ->
  X = ?CITY_WIDTH * rand:uniform(),
  Y = ?CITY_LENGTH * rand:uniform(),
  #coords{x=X, y=Y}.

random_coords(#coords{x=X, y=Y}, MAX_DISTANCE) ->
  Distance = MAX_DISTANCE * rand:uniform(),
  Angle = math:pi() * rand:uniform(),
  NewX = X + Distance * math:sin(Angle),
  NewY = Y + Distance * math:cos(Angle),
  #coords{
    x = min(max(NewX, 0), ?CITY_WIDTH),
    y = min(max(NewY, 0), ?CITY_LENGTH)}.

random_number(Min, Max) ->
  Min + (Max - Min) * rand:uniform().

%%%===================================================================
%%% private functions
%%%===================================================================

run_processes(Function, List) ->
  register(main_pid, self()),
  PidList = lists:map(fun(El) -> run_process(self(), El, Function) end, List),
  read_mapped_list(PidList, []).

run_process(From, El, Function) ->
  spawn_link(?MODULE, run_function, [From, El, Function]).

run_function(From, El, Function) ->
  From ! {self(), Function(El)}.

read_mapped_list([], Received) ->
  lists:reverse(Received);

read_mapped_list([Pid|T], Received) ->
  receive
    {Pid, Result} -> read_mapped_list(T, [Result | Received])
  end.
