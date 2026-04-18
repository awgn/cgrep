let compute_cgrep x =
  let cgrep_identifier = 1 in
  x * 2

let%test "my test" =
  let cgrep_identifier_test = 1 in
  true

let%expect_test "my test expect" =
  let cgrep_identifier_test = 2 in
  ()

let runTestHarness_cgrep x =
  let cgrep_identifier = 2 in
  x + 1
