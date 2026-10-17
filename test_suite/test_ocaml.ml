let compute_cgrep x =
  let cgrep_prod_1 = 1 in
  x * 2

module Test = struct
  let%test "my test" =
    let cgrep_test_1 = 1 in
    true

  let%expect_test "my test expect with begin..end" =
    begin
      let x = 1 in
      ignore x
    end;
    let cgrep_test_2 = 2 in
    ()
end

(* Top-level ppx_inline_test / ppx_expect with local `let ... in` bindings: *)
(* Must NOT stop at the inner `let ... in`! *)
let%test "top level inline test with local let..in" =
  let cgrep_test_3 = 3 in
  cgrep_test_3 = 3

let%expect_test "top level expect test with local let..in" =
  let cgrep_test_4 = 4 in
  ignore cgrep_test_4;
  [%expect {| |}]

(* Production module whose name starts with "Test" as part of a word (`Testimonial`): *)
(* NOT a test module! *)
module Testimonial = struct
  let author_id =
    let cgrep_prod_3 = 3 in
    cgrep_prod_3
end

let runTestHarness_cgrep x =
  let cgrep_prod_2 = 2 in
  x + 1
