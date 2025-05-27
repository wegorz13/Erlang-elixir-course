-module(pollution).
-author("filip").

-export([create_monitor/0, add_value/5, add_station/3, get_daily_average_data_count/1, remove_value/4, get_one_value/4, get_daily_mean/3, get_station_min/3]).

-record(measurement, {type, cords, date, value}).

create_monitor() -> #{}.

add_station(Name, Cords, Monitor) ->
  Key = {Name, Cords},
  case maps:is_key(Key, Monitor) of
    false -> Monitor#{{Name, Cords} => []};
    true -> {error, "the station already exists"}
  end.


add_value(Identifier, Date, Type, Value, Monitor)->
  case find_station(Identifier, maps:keys(Monitor)) of
    {error, Msg} -> {error, Msg};

    {Name, Cords} ->
      Measurement = #measurement{type=Type, cords=Cords, date=Date, value = Value},
      M_list = maps:get({Name, Cords}, Monitor),
      case verify_measurements(Measurement, M_list) of
        ok -> maps:update({Name, Cords}, [Measurement | M_list], Monitor);
        {error, Msg} -> {error, Msg}
      end
  end.

remove_value(Identifier, Date, Type, Monitor)->
  case find_station(Identifier, maps:keys(Monitor)) of
    {error, Msg} -> {error, Msg};

    {Name, Cords} ->
      Original_list = maps:get({Name, Cords}, Monitor),
      Updated_list = lists:filter(
        fun (#measurement{type = Type_f, cords = Cords_f, date = Date_f}) ->
          not (Type_f == Type andalso Cords_f == Cords andalso Date_f == Date)
        end, Original_list),
      maps:update({Name, Cords}, Updated_list, Monitor)
  end.

get_one_value(Identifier, Date, Type, Monitor)->
  case find_station(Identifier, maps:keys(Monitor)) of
    {error, Msg} -> {error, Msg};
    {Name, Cords} ->
      Pred = fun (#measurement{type=Type_f, cords=_, date=Date_f, value = _}) -> Type_f == Type andalso Date_f == Date end,
      case lists:search(Pred, maps:get({Name, Cords}, Monitor)) of
        {value, M} -> M#measurement.value;
        false -> {error, "such measurement doesnt exist"}
      end
  end.

get_station_min(Identifier, Type, Monitor) ->
  case find_station(Identifier, maps:keys(Monitor)) of
    {error, Msg} -> {error, Msg};

    {Name, Cords} ->
      Compare = fun (Elem, Acc) ->
        if
          Acc#measurement.value < Elem#measurement.value -> Acc;
          true -> Elem
        end
                end,

      Type_measurements = lists:filter(fun (#measurement{type=F_type, cords=_, date=_, value=_}) -> Type==F_type end, maps:get({Name, Cords}, Monitor)),
      if
        Type_measurements == [] -> {error, "There are no measurements of provided type"};
        true ->
          Acc = lists:last(Type_measurements),
          (lists:foldl(Compare, Acc, Type_measurements))#measurement.value
      end
  end.

get_daily_mean(Type, Date, Monitor)->
  Ftr_by_date_nd_type = fun (#measurement{type=F_type, cords=_, date= {F_date, _}, value=_}) -> Date == F_date andalso Type == F_type end,
  Sum_values = fun (#measurement{type=_, cords=_, date=_, value=N_value}, Acc) -> N_value + Acc end,
  Measurements = lists:filter(Ftr_by_date_nd_type, lists:flatten(maps:values(Monitor))),
  case Measurements of
    [] -> {error, "No measurements found for provided type and date"};
    _  -> lists:foldl(Sum_values, 0, Measurements) / length(Measurements)
  end.

get_daily_average_data_count(Monitor) ->
  Unique_days = fun (#measurement{type=_, cords=_, date= {Date, _}, value=_}, Acc) ->
    case lists:member(Date,Acc) of
      true -> Acc;
      false -> [Date | Acc]
    end end,
  Day_average = fun (_, Measurements) ->
    case Measurements of
      [] -> 0;
      _ -> length(Measurements)/length(lists:foldl(Unique_days, [], Measurements))
    end end,

  Average_per_station = maps:map(Day_average, Monitor),
  lists:sum(maps:values(Average_per_station))/map_size(Average_per_station).

find_station(Identifier, [{Identifier, Cords} | _]) ->
  {Identifier,Cords};
find_station(Identifier, [{Name, Identifier} | _]) ->
  {Name, Identifier};
find_station(Identifier, [_ | T]) ->
  find_station(Identifier, T);
find_station(_, []) -> {error, "the station is missing"}.

verify_measurements(#measurement{type=Type, cords=Cords, date=Date, value = _}, [#measurement{type=Type, cords=Cords, date=Date, value = _} | _]) ->
  {error, "there is a conflicting measurement in the monitor"};
verify_measurements(Measurement, [_|T] )->verify_measurements(Measurement, T);
verify_measurements(_, [])->ok.