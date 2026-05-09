package main

import (
	"fmt"
	mshim_std_basics "marmoset_out/shims/std/basics"
	"strconv"
)

type Record_active_bool_profile_record_age_int64_name_string_closed struct{active bool; profile Record_age_int64_name_string}
type Record_age_int64_name_string struct{age int64; name string}

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
    user := Record_active_bool_profile_record_age_int64_name_string_closed{active: true, profile: Record_age_int64_name_string{age: int64(10), name: "Ada"}}
    _ = user
    var out int64
    __scrutinee_0 := user
	if (((__scrutinee_0).profile).name == "Ada") && ((__scrutinee_0).active == true) {
		age := ((__scrutinee_0).profile).age
		_ = age
            out = age
	} else {
            out = int64(0)
	}
    _ = out
    _ = puts_int64(out)
}
