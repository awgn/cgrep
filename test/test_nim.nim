proc compute_cgrep*(x: int): int =
  var CGREP_IDENTIFIER = 1
  return x * 2

suite "my test":
  var CGREP_IDENTIFIER_TEST = 1
  test "inner":
    var CGREP_IDENTIFIER_TEST = 2

proc runTestHarness_cgrep*(x: int): int =
  var CGREP_IDENTIFIER = 2
  return x + 1
