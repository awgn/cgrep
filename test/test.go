//
// Lorem ipsum dolor sit. Amet perferendis metus feugiat. Suspendisse massa egestas quam.
// Morbi vivamus dolor nisl mauris ultricies molestie lacus. Proin ad nullam id integer.
//

/*
 * Eget orci rutrum vel. Elit nullam amet integer. Fusce tellus ut massa. Maecenas risus dictum risus.
 * Augue aliquam molestie id. Commodo ultricies pede massa fusce ullamcorper dapibus dui.
 * Maecenas elementum duis porttitor facilisis lectus eleifend nec. Arcu et pellentesque
 * tellus non tristique suscipit nec. Tempor iaculis orci nec enim ac.
 */
package main

import "fmt"

func compute(x int, label string) int {
	var base = 42
	if x > 0 {
		return x + base
	}
	return 0
}

func main() {
	var a = `a`
	var b = `"`
	var c = "'"
	var d = "\n"

	var msg1 = "Sed etiam a suspendisse. \"Aliquam nulla erat risus.\""
	var msg2 = `Sed etiam a suspendisse. "Aliquam nulla erat risus."\`

	var count int = 10
	var res int = compute(count, msg1)

	fmt.Print(a, b, c, d, msg1, msg2, res)
}
