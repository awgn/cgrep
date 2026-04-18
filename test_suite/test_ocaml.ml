
let compute_cgrep x =
  let cgrep_prod_1 = 1 in
  x * 2

module Test = struct
  let%test "my test" =
    let cgrep_test_1 = 1 in
    true

  let%expect_test "my test expect" =
    let cgrep_test_2 = 2 in
    ()
end

let runTestHarness_cgrep x =
  let cgrep_prod_2 = 2 in
  x + 1
