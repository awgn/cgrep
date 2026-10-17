-module(test_erlang).
-export([compute_cgrep/1, runTestHarness_cgrep/1, execute_test/1, check_mode/1]).

compute_cgrep(X) ->
    cgrep_prod_1 = 1,
    X * 2.

%% Production function whose name ends with `_test` but has arity 1 (takes an argument):
%% EUnit only treats 0-arity `*_test()` and `*_test_()` as tests!
execute_test(Input) ->
    cgrep_prod_3 = 3,
    Input + cgrep_prod_3.

%% Production function referencing an atom ending in `_test` inside its body:
%% Must NOT swallow the rest of the production function!
check_mode(Env) ->
    Mode = smoke_test,
    cgrep_prod_4 = 4,
    {Mode, Env, cgrep_prod_4}.

my_test() ->
    cgrep_test_1 = 1,
    true.

my_test_() ->
    cgrep_test_2 = 2,
    [?_assert(true)].

%% EUnit test using Erlang record field syntax `Rec#cfg.val` (which contains a `.` token!):
%% Must NOT stop at `.val` and leak `cgrep_test_3` into production code!
record_access_test() ->
    Val = Rec#cfg.val,
    cgrep_test_3 = 3,
    Val =:= cgrep_test_3.

runTestHarness_cgrep(X) ->
    cgrep_prod_2 = 2,
    X + 1.
