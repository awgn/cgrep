(* OCaml test example file *)

open OUnit2

(* Regular helper function (not a test) *)
let add a b = a + b

(* Another helper function *)
let multiply x y = x * y

(* Helper function *)
let rec factorial n =
  if n <= 0 then 1
  else n * factorial (n - 1)

(* Regular type (not a test) *)
type calculator = {
  mutable value : int;
}

(* Helper functions for calculator *)
let make_calculator initial = { value = initial }

let add_to_calc calc n =
  calc.value <- calc.value + n;
  calc

let get_value calc = calc.value

let reset_calc calc = calc.value <- 0

(* OUnit test with test_* naming *)
let test_addition _ =
  let result = add 2 3 in
  assert_equal 5 result

let test_multiplication _ =
  let result = multiply 4 5 in
  assert_equal 20 result

let test_factorial_5 _ =
  let result = factorial 5 in
  assert_equal 120 result

let test_factorial_0 _ =
  let result = factorial 0 in
  assert_equal 1 result

(* Helper function (not a test) *)
let process_data lst =
  List.filter (fun x -> x > 0) lst
  |> List.map (fun x -> x * 2)

(* More OUnit tests *)
let test_calculator_init _ =
  let calc = make_calculator 0 in
  assert_equal 0 (get_value calc)

let test_calculator_with_value _ =
  let calc = make_calculator 10 in
  assert_equal 10 (get_value calc)

let test_calculator_add _ =
  let calc = make_calculator 0 in
  let _ = add_to_calc calc 5 in
  let _ = add_to_calc calc 10 in
  assert_equal 15 (get_value calc)

let test_calculator_negative _ =
  let calc = make_calculator 0 in
  let _ = add_to_calc calc (-5) in
  assert_equal (-5) (get_value calc)

let test_calculator_reset _ =
  let calc = make_calculator 0 in
  let _ = add_to_calc calc 100 in
  reset_calc calc;
  assert_equal 0 (get_value calc)

(* Helper module (not a test) *)
module StringHelper = struct
  let reverse s =
    String.to_seq s
    |> List.of_seq
    |> List.rev
    |> List.to_seq
    |> String.of_seq

  let capitalize s = String.uppercase_ascii s
end

(* String tests *)
let test_string_reverse _ =
  let result = StringHelper.reverse "hello" in
  assert_equal "olleh" result

let test_string_capitalize _ =
  let result = StringHelper.capitalize "world" in
  assert_equal "WORLD" result

(* Regular type (not a test) *)
type person = {
  name : string;
  age : int;
}

let make_person name age = { name; age }

(* Helper function *)
let format_number n = Printf.sprintf "%.2f" n

(* Test suite *)
let suite =
  "Test Suite" >::: [
    "test_addition" >:: test_addition;
    "test_multiplication" >:: test_multiplication;
    "test_factorial_5" >:: test_factorial_5;
    "test_factorial_0" >:: test_factorial_0;
    "test_calculator_init" >:: test_calculator_init;
    "test_calculator_with_value" >:: test_calculator_with_value;
    "test_calculator_add" >:: test_calculator_add;
    "test_calculator_negative" >:: test_calculator_negative;
    "test_calculator_reset" >:: test_calculator_reset;
    "test_string_reverse" >:: test_string_reverse;
    "test_string_capitalize" >:: test_string_capitalize;
  ]

let () = run_test_tt_main suite