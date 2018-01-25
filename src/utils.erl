-module(utils).

-include("app_config.hrl").

%% API
-export([distance/2,
  concurrent_map/2,
  concurrent_foreach/2,
  random_coords/0,
  random_coords/2,
  random_number/2,
  get_supervisor_children_pids/1,
  log_creating_process/1,
  create_child_spec/2,
  create_child_spec/3]).

% private
-export([]).


%%%===================================================================
%%% API
%%%===================================================================

distance(#coords{x=X1, y=Y1}, #coords{x=X2, y=Y2}) ->
  math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

concurrent_map(Function, List) when is_function(Function, 1)
    andalso is_list(List) ->
  map_run_processes(Function, List).

concurrent_foreach(Function, List) when is_function(Function, 1)
    andalso is_list(List) ->
  try run_processes_foreach(Function, List) of
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

get_supervisor_children_pids(Supervisor) ->
  ChildSpecs = supervisor:which_children(Supervisor),
  FilterPidsFun = fun({_,C,_,_}) ->
                    case is_pid(C) of
                      true -> {true,C};
                      _ -> false
                    end
                  end,
  lists:filtermap(FilterPidsFun, ChildSpecs).

log_creating_process(ModuleName) ->
  case ?DEBUG of
    on -> io:format("Creating ~p~n", [ModuleName]);
    _ -> ok
  end.

create_child_spec(Module, Worker) ->
  create_child_spec(Module, Worker, []).

create_child_spec(Module, Worker, Args) ->
  #{id => erlang:unique_integer(),
    start => {Module, start_link, Args},
    restart => permanent,
    shutdown => brutal_kill,
    type => Worker,
    modules => [Module]}.

%%%===================================================================
%%% private functions
%%%===================================================================

map_run_processes(Function, List) ->
  PidList = lists:map(fun(El) -> map_run_process(self(), El, Function) end, List),
  read_mapped_list(PidList, []).

map_run_process(From, El, Function) ->
  spawn_link(fun() -> From ! {self(), Function(El)} end).

read_mapped_list([], Received) ->
  lists:reverse(Received);

read_mapped_list([Pid|T], Received) ->
  receive
    {Pid, Result} -> read_mapped_list(T, [Result | Received])
  end.


run_processes_foreach(Function, List) ->
  lists:foreach(fun(El) -> spawn_link(fun() -> Function(El) end) end, List).