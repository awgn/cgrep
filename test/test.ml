(*
 * Eget orci rutrum vel. Elit nullam amet integer. Fusce tellus ut massa. Maecenas risus dictum risus. 
 * Augue aliquam molestie id. Commodo ultricies pede massa fusce ullamcorper dapibus dui. 
 * Maecenas elementum duis porttitor facilisis lectus eleifend nec. Arcu et pellentesque 
 * tellus non tristique suscipit nec. Tempor iaculis orci nec enim ac.
 *)

let compute (x : int) (label : string) : int =
  let base = 42 in
  if x > 0 then x + base else 0

let count : int = 10
let result : int = compute count "prod"

 print_endline "Sed etiam a suspendisse. \"Aliquam nulla erat risus.\"";
