-module(utils).

-include("app_declarations.hrl").

%% API
-export([distance/2, concurrent_map/2, run_function/3]).

distance(#coords{x=X1, y=Y1}, #coords{x=X2, y=Y2}) ->
  math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).


concurrent_map(List, Function) ->
  try run_processes(List,Function) of
    X -> X
  catch
    error:Error -> {error, Error};
    exit:Exit -> {exit, Exit}
  end.

run_processes(List, Function) ->
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
