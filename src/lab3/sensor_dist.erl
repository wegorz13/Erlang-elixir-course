-module(sensor_dist).

-export([find_closest_parallel/2, find_closest_sequential/2, measure_time/3]).

get_rand_locations(Number) -> [ {random:uniform(10000), random:uniform(10000)} || _ <- lists:seq(1, Number)].

dist({X1, Y1}, {X2, Y2}) -> math:sqrt(math:pow((X2 - X1), 2) + math:pow((Y2 - Y1), 2)).

find_for_person(PersonLocation, SensorsLocations) ->
  Distances = [{dist(PersonLocation, SL), {PersonLocation, SL}} || SL <- SensorsLocations],
  lists:min(Distances).

find_closest_sequential(PeopleLocations, SensorsLocations)->
  lists:min([find_for_person(PersonLocation, SensorsLocations) || PersonLocation <- PeopleLocations]).

measure_time(Function, PeopleNumber, LocationNumber)->
  {Time, _} = timer:tc(Function, [get_rand_locations(PeopleNumber), get_rand_locations(LocationNumber)]),
  Time/1000000.

find_for_person(PersonLocation, SensorsLocations, ParentPID)->
  ParentPID ! {result, find_for_person(PersonLocation, SensorsLocations)}.

find_closest_parallel(PeopleLocations, SensorsLocations)->
  Parent = self(),
  [spawn(fun() -> _ = find_for_person(P, SensorsLocations, Parent) end) || P <- PeopleLocations],
  Results = [receive {result,ResultTuple} -> ResultTuple end || _ <- PeopleLocations],
  lists:min(Results).