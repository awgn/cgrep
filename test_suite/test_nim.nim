proc compute_cgrep*(x: int): int =
  var cgrep_prod_1 = 1
  return x * 2

# Production proc named `test` or using a variable named `test`/`suite`:
# Must NOT be treated as a `unittest` `test "..." :` or `suite "..." :` block!
proc test*(x: int): int =
  var cgrep_prod_3 = 3
  return x + cgrep_prod_3

proc checkSuiteConfig*(suite: string): string =
  var cgrep_prod_4 = 4
  return suite

suite "my test":
  var cgrep_test_1 = 1
  test "inner":
    var cgrep_test_2 = 2
  test "with anonymous proc callback":
    let cb = proc(v: int): int =
      var cgrep_test_3 = 3
      return v + cgrep_test_3
    discard cb(1)

proc runTestHarness_cgrep*(x: int): int =
  var cgrep_prod_2 = 2
  return x + 1
