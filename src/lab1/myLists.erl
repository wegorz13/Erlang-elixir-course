-module(myLists).
-author("filip").

%% API
-export([contains/2, duplicateElements/1, sumFloats/1, sumFloats/2]).

contains([H|T],Value)->H==Value or contains(T,Value);
contains([],_)->false.

duplicateElements([H|T]) -> [H,H | duplicateElements(T)];
duplicateElements([])->[].

sumFloats([H|T]) -> H + sumFloats(T);
sumFloats([]) -> 0.

sumFloats([H|T], Sum) -> sumFloats(T, Sum+H);
sumFloats([], Sum) -> Sum.