-module(pollution_value_collector_gen_statem).
-behaviour(gen_statem).
-author("filip").

-export([init/1, callback_mode/0]).
%% API
-export([start_link/0, set_station/1, add_value/3, store_data/0]).

-define(SERVER_NAME, pollution_statem).

-record(state, {monitor, station=none, new_measurements=[]}).
-record(measurement, {type, cords, date, value}).

start_link()->
  gen_statem:start_link({local, ?SERVER_NAME}, ?MODULE, pollution:create_monitor(), []).

init(Monitor) ->
  {ok, no_station_set, #state{monitor = Monitor}}.

set_station(Identifier)->
  gen_statem:cast(?SERVER_NAME, {set_station, Identifier}).

add_value(Date, Type, Value)->
  gen_statem:cast(?SERVER_NAME, {add_value, Date, Type, Value}).

store_data()->
  gen_statem:cast(?SERVER_NAME, {save_data}).

no_station_set(_Event, {set_station, Identifier}, State = #state)->
  case pollution:find_station(Identifier, State#state.monitor) of
    {error, Msg} ->
      io:format("ERROR: ~s. ~n", [Msg]),
      {keep_state_and_data};
    Station ->
      {next_state, station_set, State#state{station = Station}}
  end.

station_set(_Event, {add_value, Date, Type, Value}, State = #state)->
  {_, Cords} = State#state.station,
  New_measurement = #measurement{type = Type, cords = Cords, date = Date, value = Value },
  Updated_measurements = [New_measurement | State#state.new_measurements],
  {keep_state, State#state{new_measurements = Updated_measurements}};

station_set(_Event, {save_data}, State = #state)->
  Updated_monitor = push_data(State#state.new_measurements, State#state.monitor),
  {next_state, no_station_set, #state{monitor = Updated_monitor}}.

push_data([], Monitor)->Monitor;

push_data([Measurement | T], Monitor)->
  {Date, Type, Cords, Value} = {Measurement#measurement.date, Measurement#measurement.type, Measurement#measurement.cords, Measurement#measurement.value},
  case pollution:add_value(Cords, Date, Type, Value, Monitor) of
    {error, Msg} -> push_data(T, Monitor);
    New_monitor -> push_data(T, New_monitor)
  end.

terminate(Reason, State, Data) ->
  io:format("Error ~w. ~n", [Reason]),
  io:format("In state ~w. ~n", [State]),
  io:format("With data ~w. ~n", [Data]),
  ok.

callback_mode() ->
  state_functions.