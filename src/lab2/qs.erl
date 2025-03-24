-module(qs).

-export([qs/1, sample_list/0, random_elems/3, compare_speeds/3]).

sample_list() -> [3,6,1,0,9,6,8].

less_than(List, Arg) -> lists:filter(fun (X) -> X<Arg end, List).

grt_eq_than(List, Arg) -> lists:filter(fun (X) -> X>=Arg end, List).

qs([])->[];
qs([Pivot|Tail]) -> qs( less_than(Tail,Pivot) ) ++ [Pivot] ++ qs( grt_eq_than(Tail,Pivot) ).

random_elems(N,Min,Max) -> [rand:uniform(Max-Min)+Min || _ <-lists:seq(1,N) ].

compare_speeds(List, Fun1, Fun2) ->
  {Time1,_} = timer:tc(Fun1, [List]),
  {Time2,_} = timer:tc(Fun2, [List]),
  {Time1, Time2, Time1/(Time2+1)}.
