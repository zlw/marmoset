package main

type Record_active_bool_profile_record_age_int64_name_string_closed struct{active bool; profile Record_age_int64_name_string}
type Record_age_int64_name_string struct{age int64; name string}


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
    _ = puts(out)
}
