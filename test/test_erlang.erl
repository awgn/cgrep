-module(test_erlang).
-export([compute_cgrep/1, runTestHarness_cgrep/1]).

compute_cgrep(X) ->
    CGREP_IDENTIFIER = 1,
    X * 2.

my_test() ->
    CGREP_IDENTIFIER_TEST = 1,
    true.

my_test_() ->
    CGREP_IDENTIFIER_TEST = 2,
    [?_assert(true)].

runTestHarness_cgrep(X) ->
    CGREP_IDENTIFIER = 2,
    X + 1.
