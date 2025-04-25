-module(pollution_server).

-export([add_station/2, add_value/4, remove_value/3, get_one_value/3, get_station_min/2, get_daily_mean/2, get_daily_average_data_count/0, start/0, stop/0]).

-record(measurement, {type, cords, date, value}).

start()->
  register(server, spawn(fun() -> init() end)).

stop()->
  server ! stop.

init()->
  State = create_monitor(),
  loop(State).

add_station(Name, Cords) ->
  server ! {add_station, self(), {Name, Cords}},
  receive Result -> Result end.

add_value(Identifier, Date, Type, Value) ->
  server ! {add_value, self(), {Identifier, Date, Type, Value}},
  receive Result -> Result end.

remove_value(Identifier, Date, Type) ->
  server ! {remove_value, self(), {Identifier, Date, Type}},
  receive Result -> Result end.

get_one_value(Identifier, Date, Type) ->
  server ! {get_one_value, self(), {Identifier, Date, Type}},
  receive Result -> Result end.

get_station_min(Identifier, Type) ->
  server ! {get_station_min, self(), {Identifier, Type}},
  receive Result -> Result end.

get_daily_mean(Type, Date) ->
  server ! {get_daily_mean, self(), {Type, Date}},
  receive Result -> Result end.

get_daily_average_data_count() ->
  server ! {get_daily_average_data_count, self()},
  receive Result -> Result end.

loop(State)->
  receive
    stop ->
      io:format("Stopping server~n"),
      ok;
    {add_station, SenderPID, {Name, Cords}} ->
      handle_change_of_state_call(fun add_station/3, [Name, Cords], State, SenderPID);
    {add_value, SenderPID, {Identifier, Date, Type, Value}} ->
      handle_change_of_state_call(fun add_value/5, [Identifier, Date, Type, Value], State, SenderPID);
    {remove_value, SenderPID, {Identifier, Date, Type}} ->
      handle_change_of_state_call(fun remove_value/4, [Identifier, Date, Type], State, SenderPID);
    {get_one_value, SenderPID, {Identifier, Date, Type}} ->
      SenderPID ! get_one_value(Identifier, Date, Type, State),
      loop(State);
    {get_station_min, SenderPID, {Identifier, Type}} ->
      SenderPID ! get_station_min(Identifier, Type, State),
      loop(State);
    {get_daily_mean, SenderPID, {Identifier, Type}} ->
      SenderPID ! get_daily_mean(Identifier, Type, State),
      loop(State);
    {get_daily_average_data_count, SenderPID} ->
      SenderPID ! get_daily_average_data_count(State),
      loop(State)
  end.

handle_change_of_state_call(Server_function, Args_list, State, SenderPID)->
  case apply(Server_function, Args_list ++ [State]) of
    {error, Msg} ->
      io:format("ERROR: ~s. ~n", [Msg]),
      SenderPID ! {error, Msg},
      loop(State);
    New_state ->
      SenderPID ! New_state,
      loop(New_state)
  end.

create_monitor() -> #{}.

add_station(Name, Cords, Monitor) ->
  Station_by_name = lists:keyfind(Name, 1, maps:keys(Monitor)),
  Station_by_cords = lists:keyfind(Cords, 2, maps:keys(Monitor)),
  if
    Station_by_cords == false andalso Station_by_name == false -> Monitor#{{Name, Cords} => []};
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