%% Erlang test example file

-module(test_erlang_example).
-include_lib("eunit/include/eunit.hrl").

%% Regular helper functions (not tests)
add(A, B) -> A + B.

multiply(X, Y) -> X * Y.

factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).

%% Regular record (not a test)
-record(calculator, {value = 0}).

%% Helper functions for calculator
make_calculator() -> #calculator{value = 0}.

make_calculator(Initial) -> #calculator{value = Initial}.

add_to_calc(#calculator{value = V} = Calc, N) ->
    Calc#calculator{value = V + N}.

get_value(#calculator{value = V}) -> V.

reset_calc(Calc) -> Calc#calculator{value = 0}.

%% EUnit tests with _test suffix
addition_test() ->
    Result = add(2, 3),
    ?assertEqual(5, Result).

multiplication_test() ->
    Result = multiply(4, 5),
    ?assertEqual(20, Result).

factorial_5_test() ->
    Result = factorial(5),
    ?assertEqual(120, Result).

factorial_0_test() ->
    Result = factorial(0),
    ?assertEqual(1, Result).

%% Helper function (not a test)
process_data(List) ->
    Filtered = lists:filter(fun(X) -> X > 0 end, List),
    lists:map(fun(X) -> X * 2 end, Filtered).

%% Calculator tests
calculator_init_test() ->
    Calc = make_calculator(),
    ?assertEqual(0, get_value(Calc)).

calculator_with_value_test() ->
    Calc = make_calculator(10),
    ?assertEqual(10, get_value(Calc)).

calculator_add_test() ->
    Calc1 = make_calculator(),
    Calc2 = add_to_calc(Calc1, 5),
    Calc3 = add_to_calc(Calc2, 10),
    ?assertEqual(15, get_value(Calc3)).

calculator_negative_test() ->
    Calc1 = make_calculator(),
    Calc2 = add_to_calc(Calc1, -5),
    ?assertEqual(-5, get_value(Calc2)).

calculator_reset_test() ->
    Calc1 = make_calculator(),
    Calc2 = add_to_calc(Calc1, 100),
    Calc3 = reset_calc(Calc2),
    ?assertEqual(0, get_value(Calc3)).

%% EUnit test generator with _test_ suffix
string_operations_test_() ->
    [?_assertEqual("olleh", reverse_string("hello")),
     ?_assertEqual("WORLD", string:uppercase("world")),
     ?_assertEqual("", reverse_string(""))].

%% Helper function
reverse_string(Str) ->
    lists:reverse(Str).

%% Regular module helper (not a test)
-record(person, {name, age}).

make_person(Name, Age) ->
    #person{name = Name, age = Age}.

get_person_name(#person{name = Name}) -> Name.

%% Helper function
format_number(N) ->
    io_lib:format("~.2f", [N]).

%% Test fixture with setup and cleanup
calculator_fixture_test_() ->
    {setup,
     fun() -> make_calculator(100) end,
     fun(_Calc) -> ok end,
     fun(Calc) ->
         [?_assertEqual(100, get_value(Calc)),
          ?_assertEqual(105, get_value(add_to_calc(Calc, 5)))]
     end}.

%% More helper code
-export([add/2, multiply/2, factorial/1, process_data/1]).
-export([make_calculator/0, make_calculator/1, get_value/1]).