-module(pingpong).

%% API
-export([play/1,start/0,stop/0]).

ping() ->
  receive
    {play,N} when N > 0 ->
      io:format("~w ~n", [N]),
      timer:sleep(500),
      pong ! {play, N - 1},
      ping();
    stop -> io:format("killed")
  after
    20000 -> io:format("died waiting")
  end.

pong() ->
  receive
    {play, 0} -> io:format("finished"), ping();
    {play, N} when N > 0 ->
      io:format("~w ~n", [N]),
      timer:sleep(1000),
      ping ! {play, N - 1},
      ping();
    stop -> io:format("killed");
    _ -> io:format("unknown message"), ping()
  after
    20000 -> io:format("died waiting")
  end.

play(N) when is_integer(N) andalso N > 0 -> ping ! {play, N}.

start() ->
  register(ping, spawn(fun() -> ping() end)),
  register(pong, spawn(fun() -> pong() end)).

stop() ->
  ping ! stop,
  pong ! stop.

