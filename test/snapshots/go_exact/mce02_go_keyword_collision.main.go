package main

import (
	"fmt"
	mshim_std_basics "marmoset_out/shims/std/basics"
)

type Fmt__Point struct{x int64; y int64}

func extern__std_basics__puts_str(value string) struct{} {
	defer func() {
		if __panic := recover(); __panic != nil {
			panic(fmt.Sprintf("shim ABI violation at std/basics.puts_str: %v", __panic))
		}
	}()
	mshim_std_basics.PutsStr(value)
	return struct{}{}
}

func fmt__join_string_string(a string, b string) string {
    return ((a + ":") + b)
}

func fmt__format_u005fpoint_record_x_int64_y_int64_closed(p Fmt__Point) string {
    return fmt__join_string_string("x", "y")
}

func std__basics__puts_u005fstr_string(value string) struct{} {
    return extern__std_basics__puts_str(value)
}

func puts_string(value string) struct{} {
    return std__basics__puts_u005fstr_string(show_show_string(value))
}

func show_show_string(x string) string {
	return x
}
func main() {
    var main__p Fmt__Point = Fmt__Point{x: int64(1), y: int64(2)}
    _ = main__p
    _ = puts_string(fmt__join_string_string("left", "right"))
    _ = puts_string(fmt__format_u005fpoint_record_x_int64_y_int64_closed(main__p))
}
