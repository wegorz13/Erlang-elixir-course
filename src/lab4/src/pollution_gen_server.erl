-module(pollution_gen_server).
-behaviour(gen_server).

-export([add_station/2, add_value/4, remove_value/3, get_one_value/3, get_station_min/2, get_daily_mean/2, get_daily_average_data_count/0, start/1, start/0, stop/0, crash/0, init/1, handle_call/3, handle_cast/2]).


start(InitialValue)->
  start_link(InitialValue).

start()->
  start_link(pollution:create_monitor()).

stop() ->
  gen_server: cast(pollution_server, stop).

crash()->
  1/0.

init(InitialValue) ->
  {ok, InitialValue}.

start_link(InitialValue)->
  gen_server:start_link({local, pollution_server}, pollution_gen_server, InitialValue, []).

terminate(Reason, Value) ->
  io:format("Server: exit with value ~p~n", [Value]),
  Reason.

add_station(Name, Cords) ->
  gen_server:cast(pollution_server, {add_station, Name, Cords}).

add_value(Identifier, Date, Type, Value) ->
  gen_server:cast(pollution_server, {add_value, Identifier, Date, Type, Value}).

remove_value(Identifier, Date, Type) ->
  gen_server:cast(pollution_server, {remove_value, Identifier, Date, Type}).

get_one_value(Identifier, Date, Type) ->
  gen_server:call(pollution_server, {get_one_value, Identifier, Date, Type}).

get_station_min(Identifier, Type) ->
  gen_server:call(pollution_server, {get_station_min, Identifier, Type}).

get_daily_mean(Type, Date) ->
  gen_server:call(pollution_server, {get_daily_mean, Type, Date}).

get_daily_average_data_count() ->
  gen_server:call(pollution_server, {get_daily_average_data_count}).


handle_cast({add_station, Name, Cords}, State) ->
  case pollution:add_station(Name, Cords, State) of
    {error, Msg} ->
      io:format("ERROR: ~s. ~n", [Msg]),
      {noreply, State};
    New_state ->
      {noreply, New_state}
  end;

handle_cast({add_value, Identifier, Date, Type, Value}, State) ->
  case pollution:add_value(Identifier, Date, Type, Value, State) of
    {error, Msg} ->
      io:format("ERROR: ~s. ~n", [Msg]),
      {noreply, State};
    New_state ->
      {noreply, New_state}
  end;

handle_cast({remove_value, Identifier, Date, Type}, State) ->
  case pollution:remove_value(Identifier, Date, Type, State) of
    {error, Msg} ->
      io:format("ERROR: ~s. ~n", [Msg]),
      {noreply, State};
    New_state ->
      {noreply, New_state}
  end;

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_call({get_one_value, Identifier, Date, Type}, _From, State) ->
  Value = pollution:get_one_value(Identifier, Date, Type, State),
  {reply, Value, State};

handle_call({get_station_min, Identifier, Type}, _From, State) ->
  Value = pollution:get_station_min(Identifier, Type, State),
  {reply, Value, State};

handle_call({get_daily_mean, Type, Date}, _From, State) ->
  Value = pollution:get_daily_mean(Type, Date, State),
  {reply, Value, State};

handle_call({get_daily_average_data_count}, _From, State) ->
  Value = pollution:get_daily_average_data_count(State),
  {reply, Value, State}.

handle_info(UnknownMessage, State) ->
  io:format("Server received unexpected message", [UnknownMessage]),
  {noreply, State}.



