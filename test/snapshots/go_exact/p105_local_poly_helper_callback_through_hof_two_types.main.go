package main

import (
	"fmt"
	mshim_std_basics "marmoset_out/shims/std/basics"
)

func extern__std_basics__puts_str(value string) struct{} {
	defer func() {
		if __panic := recover(); __panic != nil {
			panic(fmt.Sprintf("shim ABI violation at std/basics.puts_str: %v", __panic))
		}
	}()
	mshim_std_basics.PutsStr(value)
	return struct{}{}
}

func run() string {
    if apply2_fn_int64_fn_int64_bool_int64_int64(__local_same_7_int64_int64, int64(1), int64(1)) {
            if apply2_fn_string_fn_string_bool_string_string(__local_same_7_string_string, "ok", "ok") {
                return "ok"
            } else {
                return "bad-str"
            }
    } else {
        return "bad-int"
    }
}

func puts_string(value string) struct{} {
    return extern__std_basics__puts_str(show_show_string(value))
}

func __local_same_7_int64_int64(x int64, y int64) bool {
    return (x == y)
}

func __local_same_7_string_string(x string, y string) bool {
    return (x == y)
}

func apply2_fn_int64_fn_int64_bool_int64_int64(f func(int64, int64) bool, a int64, b int64) bool {
    return f(a, b)
}

func apply2_fn_string_fn_string_bool_string_string(f func(string, string) bool, a string, b string) bool {
    return f(a, b)
}

func show_show_string(x string) string {
	return x
}
func main() {
    _ = puts_string(run())
}
