-module(lab1).

%% API
-export([hello_world/0,hello_world/1,power/2]).

hello_world()->io:format("hello world!~n").
hello_world(Whom)->io:format("hello ~s!~n",[Whom]).

power(_,0)->1;
power(Number,1)->Number;
power(Number,P)->Number*power(Number,P-1).



