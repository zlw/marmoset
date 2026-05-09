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

func puts_int64(value int64) struct{} {
    return std__basics__puts_u005fstr_string(show_show_int64(value))
}

func std__basics__puts_u005fstr_string(value string) struct{} {
    return extern__std_basics__puts_str(value)
}

func show_show_int64(x int64) string {
	return strconv.FormatInt(x, 10)
}
func main() {
    foo__bar__value := int64(1)
    _ = foo__bar__value
    foo_u005f_u005fbar__value := int64(2)
    _ = foo_u005f_u005fbar__value
    _ = puts_int64(foo__bar__value)
    _ = puts_int64(foo_u005f_u005fbar__value)
}
