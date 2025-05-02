-module(pollution_server_supervisor).
-behaviour(supervisor).
-author("filip").

-export([init/1]).


start_link(InitialValue)->
  supervisor:start_link({local, pollution_sup}, ?MODULE, InitialValue).

init(InitialValue) ->
  {ok, {
    #{ strategy => one_for_one,
      intensity => 2,
      period => 3
    },
    [#{id => 'pollution_server',
      start => {pollution_gen_server, start, [InitialValue]},
      restart => permanent,
      shutdown => 2000,
      type => worker,
      modules => [pollution_gen_server]} ]} }.