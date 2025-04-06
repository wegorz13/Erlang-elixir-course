%%%-------------------------------------------------------------------
%%% @author Wojciech Turek
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. mar 2019 12:50
%%%-------------------------------------------------------------------
-module(pollution_test).
-author("Wojciech Turek").

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_monitor_test() ->
  M1 = pollution:create_monitor(),
  M_ = pollution:create_monitor(),
  ?assertEqual(M_, M1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_station_test() ->
  M1 = pollution:create_monitor(),
  M2 = pollution:add_station("Stacja 1", {1,1}, M1),
  ?assertNotMatch({error, _}, M2),
  ?assertMatch({error, _}, pollution:add_station("Stacja 1", {1,1}, M2)),
  ?assertMatch({error, _}, pollution:add_station("Stacja 1", {2,2}, M2)),
  ?assertMatch({error, _}, pollution:add_station("Stacja 2", {1,1}, M2)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_test() ->
  M = pollution:add_station("Stacja 1", {1,1}, pollution:create_monitor()),
  Time = calendar:local_time(),
  ?assertNotMatch({error, _}, pollution:add_value("Stacja 1", Time, "PM10", 46.3, M)),
  ?assertNotMatch({error, _}, pollution:add_value("Stacja 1", Time, "PM1", 46.3, M)),
  ?assertNotMatch({error, _}, pollution:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3, M)),

  M1 = pollution:add_value("Stacja 1", Time, "PM10", 46.3, M),
  M2 = pollution:add_value("Stacja 1", Time, "PM1", 46.3, M1),
  M3 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3, M2),
  ?assertNotMatch({error, _},M3),

  timer:sleep(1100),
  Time2 = calendar:local_time(),
  ?assertNotMatch({error, _}, pollution:add_value( {1,1}, Time2, "PM10", 46.3, M3)),
  ?assertNotMatch({error, _}, pollution:add_value( {1,1}, Time2, "PM1", 46.3, M3)),
  ?assertNotMatch({error, _}, pollution:add_value( {1,1}, {{2023,3,27},{11,16,10}}, "PM10", 46.3, M3)),

  M4 = pollution:add_value({1,1}, Time2, "PM10", 46.3, M3),
  M5 = pollution:add_value({1,1}, Time2, "PM1", 46.3, M4),
  M6 = pollution:add_value({1,1}, {{2023,3,27},{11,16,10}}, "PM10", 46.3, M5),
  ?assertNotMatch({error, _},M6).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_fail_test() ->
  M = pollution:add_station("Stacja 1", {1,1}, pollution:create_monitor()),
  Time = calendar:local_time(),
  ?assertNotMatch({error, _}, pollution:add_value("Stacja 1", Time, "PM10", 46.3, M)),
  ?assertMatch({error, _}, pollution:add_value("Stacja 1", Time, "PM10", 46.3, M)),
  ?assertMatch({error, _}, pollution:add_value("Stacja 1", Time, "PM10", 36.3, M)),
  ?assertMatch({error, _}, pollution:add_value({1,1}, Time, "PM10", 46.3, M)),
  ?assertMatch({error, _}, pollution:add_value({1,1}, Time, "PM10", 36.3, M)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
add_value_non_existing_station_test() ->
  M = pollution:add_station("Stacja 1", {1,1}, pollution:create_monitor()),
  ?assertMatch({error, _}, pollution:add_value("Stacja 2", calendar:local_time(), "PM10", 46.3, M)),
  ?assertMatch({error, _}, pollution:add_value({1,2}, calendar:local_time(), "PM10", 46.3, M)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_test() ->
  M = pollution:add_station("Stacja 1", {1,1}, pollution:create_monitor()),
  Time = calendar:local_time(),
  M1 = pollution:add_value("Stacja 1", Time, "PM10", 46.3, M),
  M2 = pollution:add_value("Stacja 1", Time, "PM1", 46.3, M1),
  M3 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3, M2),

  M4 = pollution:remove_value("Stacja 1", Time, "PM10", M3),
  ?assertNotMatch({error, _}, M4),
  ?assertNotEqual(M4, M3),
  M5 = pollution:remove_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", M4),
  ?assertNotMatch({error, _}, M5),
  ?assertNotEqual(M5, M4).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_and_add_back_test() ->
  M = pollution:add_station("Stacja 1", {1,1}, pollution:create_monitor()),
  Time = calendar:local_time(),
  M1 = pollution:add_value("Stacja 1", Time, "PM10", 46.3, M),
  M2 = pollution:add_value("Stacja 1", Time, "PM1", 46.3, M1),
  M3 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3, M2),

  M4 = pollution:remove_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", M3),
  ?assertNotEqual(M4, M3),

  M5 = pollution:add_value({1,1}, {{2023,3,27},{11,16,9}}, "PM10", 46.3, M4),
  ?assertEqual(M5, M3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
remove_value_fail_test() ->
  M = pollution:add_station("Stacja 1", {1,1}, pollution:create_monitor()),
  Time = calendar:local_time(),
  M1 = pollution:add_value("Stacja 1", Time, "PM10", 46.3, M),
  M2 = pollution:add_value("Stacja 1", Time, "PM1", 46.3, M1),
  M3 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 46.3, M2),

  ?assertMatch({error, _}, pollution:remove_value("Stacja 1", Time, "PM25", M3)),
  ?assertMatch({error, _}, pollution:remove_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", M3)),
  ?assertMatch({error, _}, pollution:remove_value({1,2}, Time, "PM10", M3)),
  ?assertMatch({error, _}, pollution:remove_value("Stacja 2", Time, "PM10", M3)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_one_value_test() ->
  M = pollution:add_station("Stacja 1", {1,1}, pollution:create_monitor()),
  Time = calendar:local_time(),
  M1 = pollution:add_value("Stacja 1", Time, "PM10", 46.3, M),
  M2 = pollution:add_value("Stacja 1", Time, "PM1", 36.3, M1),
  M3 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 26.3, M2),

  ?assertMatch(46.3, pollution:get_one_value("Stacja 1", Time, "PM10", M3)),
  ?assertMatch(36.3, pollution:get_one_value("Stacja 1", Time, "PM1", M3)),
  ?assertMatch(46.3, pollution:get_one_value({1,1}, Time, "PM10", M3)),
  ?assertMatch(26.3, pollution:get_one_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", M3)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_one_value_fail_test() ->
  M = pollution:add_station("Stacja 1", {1,1}, pollution:create_monitor()),
  Time = calendar:local_time(),
  M1 = pollution:add_value("Stacja 1", Time, "PM10", 46.3, M),
  M2 = pollution:add_value("Stacja 1", Time, "PM1", 36.3, M1),
  M3 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,9}}, "PM10", 26.3, M2),

  ?assertMatch({error, _}, pollution:get_one_value("Stacja 1", Time, "PM25", M3)),
  ?assertMatch({error, _}, pollution:get_one_value({1,1}, Time, "PM25", M3)),
  ?assertMatch({error, _}, pollution:get_one_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", M3)),
  ?assertMatch({error, _}, pollution:get_one_value("Stacja 2", Time, "PM1", M3)),
  ?assertMatch({error, _}, pollution:get_one_value({1,2}, Time, "PM10", M3)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_station_min_test() ->
  M = pollution:add_station("Stacja 1", {1,1}, pollution:create_monitor()),
  M1 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10, M),
  M2 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,11}}, "PM10", 20, M1),
  M3 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 5, M2),

  ?assertMatch(10, pollution:get_station_min("Stacja 1", "PM10", M2)),
  ?assertMatch(5, pollution:get_station_min("Stacja 1", "PM10", M3)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_station_min_fail_test() ->
  M = pollution:add_station("Stacja 1", {1,1}, pollution:create_monitor()),
  ?assertMatch({error, _}, pollution:get_station_min("Stacja 1", "PM10", M)),
  M1 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10, M),
  ?assertMatch({error, _}, pollution:get_station_min("Stacja 1", "PM25", M1)),
  ?assertMatch({error, _}, pollution:get_station_min("Stacja 2", "PM25", M1)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_daily_mean_test() ->
  M = pollution:add_station("Stacja 3", {3,3}, pollution:add_station("Stacja 2", {2,2}, pollution:add_station("Stacja 1", {1,1}, pollution:create_monitor()))),
  M1 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10, M),
  M2 = pollution:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20, M1),
  M3 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,12}}, "PM10", 10, M2),
  M4 = pollution:add_value("Stacja 2", {{2023,3,27},{11,16,13}}, "PM10", 20, M3),

  M5 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,14}}, "PM25", 100, M4),
  M6 = pollution:add_value("Stacja 2", {{2023,3,27},{11,16,15}}, "PM25", 220, M5),

  M7 = pollution:add_value("Stacja 1", {{2023,3,28},{11,16,16}}, "PM10", 2000, M6),
  M8 = pollution:add_value("Stacja 2", {{2023,3,28},{11,16,17}}, "PM10", 3000, M7),

  M9 = pollution:add_value("Stacja 3", {{2023,3,27},{11,16,18}}, "PM10", 1234, M8),

  ?assertMatch(15.0, pollution:get_daily_mean("PM10",{2023,3,27}, M2)),
  ?assertMatch(15.0, pollution:get_daily_mean("PM10",{2023,3,27}, M6)),
  ?assertMatch(258.8, pollution:get_daily_mean("PM10",{2023,3,27}, M9)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_daily_mean_fail_test() ->
  M = pollution:add_station("Stacja 2", {2,2}, pollution:add_station("Stacja 1", {1,1}, pollution:create_monitor())),
  ?assertMatch({error, _}, pollution:get_daily_mean("PM10",{2023,3,27}, M)),
  M1 = pollution:add_value("Stacja 1", {{2023,3,27},{11,16,10}}, "PM10", 10, M),
  M2 = pollution:add_value("Stacja 2", {{2023,3,27},{11,16,11}}, "PM10", 20, M1),

  ?assertMatch({error, _}, pollution:get_daily_mean("PM25",{2023,3,27}, M2)),
  ?assertMatch({error, _}, pollution:get_daily_mean("PM10",{2023,3,29}, M2)).

