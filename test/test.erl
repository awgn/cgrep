% 
% Lorem ipsum dolor sit. Amet perferendis metus feugiat. Suspendisse massa egestas quam. 
% Morbi vivamus dolor nisl mauris ultricies molestie lacus. Proin ad nullam id integer.
% 

-module(test).
-export([compute/2, main/0]).

-spec compute(Integer, Integer) -> Integer.
compute(X, Offset) ->
    Base = 42,
    case X > 0 of
        true -> X + Offset + Base;
        false -> 0
    end.

main() ->
    Msg = "Sed etiam a suspendisse. \"Aliquam nulla erat risus.\"~n",
    Result = compute(10, 5),
    io:format(Msg),
    Result.
