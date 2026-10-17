//
// Lorem ipsum dolor sit. Amet perferendis metus feugiat. Suspendisse massa egestas quam. 
// Morbi vivamus dolor nisl mauris ultricies molestie lacus. Proin ad nullam id integer.
//

(*
 * Eget orci rutrum vel. Elit nullam amet integer. Fusce tellus ut massa. Maecenas risus dictum risus. 
 * Augue aliquam molestie id. Commodo ultricies pede massa fusce ullamcorper dapibus dui. 
 * Maecenas elementum duis porttitor facilisis lectus eleifend nec. Arcu et pellentesque 
 * tellus non tristique suscipit nec. Tempor iaculis orci nec enim ac.
 *)

let compute (x: int) (label: string) : int =
    let baseVal = 42
    if x > 0 then
        x + baseVal
    else
        0

let count : int = 10
let result : int = compute count "prod"

printfn "%s" "Sed etiam a suspendisse. \"Aliquam nulla erat risus.\"";
printfn "%s" @"Sed etiam a suspendisse. ""Aliquam nulla erat risus.""";
