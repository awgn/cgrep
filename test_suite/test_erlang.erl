-module(test_erlang).
-export([compute_cgrep/1, runTestHarness_cgrep/1]).

compute_cgrep(X) ->
    cgrep_prod_1 = 1,
    X * 2.

my_test() ->
    cgrep_test_1 = 1,
    true.

my_test_() ->
    cgrep_test_2 = 2,
    [?_assert(true)].

runTestHarness_cgrep(X) ->
    cgrep_prod_2 = 2,
    X + 1.
