package main

import (
	"fmt"
	mshim_std_basics "marmoset_out/shims/std/basics"
	"strconv"
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

func __section_neg28_int64(it int64) int64 {
    return (it + int64(41))
}

func puts_int64(value int64) struct{} {
    return std__basics__puts_u005fstr_string(show_show_int64(value))
}

func std__basics__puts_u005fstr_string(value string) struct{} {
    return extern__std_basics__puts_str(value)
}

func show_show_int64(x int64) string {
	return strconv.FormatInt(x, 10)
}
func Child_cast_int64__int64(x int64, f func(int64) int64) int64 {
        return f(x)
}

func main() {
    _ = puts_int64(Child_cast_int64__int64(int64(1), __section_neg28_int64))
}
