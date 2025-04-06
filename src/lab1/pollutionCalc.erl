-module(pollutionCalc).

-export([sample_values/0, number_of_readings/2, calculate_max/2, calculate_mean/2]).
%% [readings] reading: {"nazwa", {{dzien,miesiac,rok},{godzina, minuta, sekunda}}, [{pomiar, wartosc}...] }

sample_values() ->
  [
    {"Station_A", {{23, 3, 2025}, {14, 30, 45}},
      [{"PM10", 35.2}, {"PM2.5", 18.5}, {"Temperature", 12.3}, {"Pressure", 1015.4}, {"Humidity", 45.7}]
    },
    {"Station_B", {{23, 3, 2025}, {15, 10, 30}},
      [{"PM10", 40.1}, {"PM1", 15.3}, {"Temperature", 14.8}, {"Humidity", 50.2}]
    },
    {"Station_C", {{23, 3, 2025}, {16, 5, 20}},
      [{"PM2.5", 22.7}, {"PM1", 10.5}, {"Pressure", 1020.3}, {"Temperature", 13.0}, {"Humidity", 48.0}]
    },
    {"Station_D", {{23, 3, 2025}, {17, 45, 10}},
      [{"PM10", 30.4}, {"PM2.5", 17.2}, {"PM1", 9.8}, {"Temperature", 11.5}, {"Humidity", 42.3}, {"CO2", 400.2}]
    }
  ].

number_of_readings([{_, Date,_} |T], Date) -> 1 + number_of_readings(T, Date);
number_of_readings([_|T], Date) -> number_of_readings(T, Date);
number_of_readings([], _)-> 0.

calculate_max([], _) -> none;
calculate_max(Readings, Type) ->
  Values = get_values(Readings, Type),
  case Values of
    [] -> {error, "nieistniejacy pomiar"};
    _ -> lists:max(Values)
  end.

calculate_mean([], _) -> none;
calculate_mean(Readings, Type) ->
  Values = get_values(Readings, Type),
  case Values of
    [] -> {error, "nieistniejacy pomiar"};
    _ -> lists:sum(Values)/length(Values)
  end.

get_values([], _) -> [];
get_values([{_, _, Measurements}| T], Type) ->
  Value = find_value(Measurements, Type),
  case Value of
    none -> get_values(T, Type);
    _ -> [Value | get_values(T, Type)]
  end.

find_value([], _) -> none;
find_value([{Type, Value} | _], Type) -> Value;
find_value([_ | T], Type) -> find_value(T, Type).


