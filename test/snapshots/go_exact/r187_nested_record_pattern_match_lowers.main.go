package main

import "strconv"

type ordering struct {
	Tag int8
}

const ordering_less_tag = 0
const ordering_equal_tag = 1
const ordering_greater_tag = 2

func ordering_less() ordering {
	return ordering{Tag: ordering_less_tag}
}

func ordering_equal() ordering {
	return ordering{Tag: ordering_equal_tag}
}

func ordering_greater() ordering {
	return ordering{Tag: ordering_greater_tag}
}

func (e ordering) String() string {
	switch e.Tag {
	case ordering_less_tag:
		return "less"
	case ordering_equal_tag:
		return "equal"
	case ordering_greater_tag:
		return "greater"
	default:
		panic("unreachable: invalid enum tag")
	}
}


type Record_active_bool_profile_record_age_int64_name_string_closed struct{active bool; profile Record_age_int64_name_string}
type Record_age_int64_name_string struct{age int64; name string}

func show_show_int64(x int64) string {
	return strconv.FormatInt(x, 10)
}

func show_show_bool(x bool) string {
	return strconv.FormatBool(x)
}

func show_show_string(x string) string {
	return x
}

func show_show_float64(x float64) string {
	return strconv.FormatFloat(x, 'g', -1, 64)
}

func debug_debug_int64(x int64) string {
	return strconv.FormatInt(x, 10)
}

func debug_debug_bool(x bool) string {
	return strconv.FormatBool(x)
}

func debug_debug_string(x string) string {
	return strconv.Quote(x)
}

func debug_debug_float64(x float64) string {
	return strconv.FormatFloat(x, 'g', -1, 64)
}

func eq_eq_int64(x, y int64) bool {
	return x == y
}

func eq_eq_bool(x, y bool) bool {
	return x == y
}

func eq_eq_string(x, y string) bool {
	return x == y
}

func eq_eq_float64(x, y float64) bool {
	return x == y
}

func ord_compare_int64(x, y int64) ordering {
	if x < y { return ordering_less() } else if x == y { return ordering_equal() } else { return ordering_greater() }
}

func ord_compare_bool(x, y bool) ordering {
	if !x && y { return ordering_less() } else if x == y { return ordering_equal() } else { return ordering_greater() }
}

func ord_compare_string(x, y string) ordering {
	if x < y { return ordering_less() } else if x == y { return ordering_equal() } else { return ordering_greater() }
}

func ord_compare_float64(x, y float64) ordering {
	if x < y { return ordering_less() } else if x == y { return ordering_equal() } else { return ordering_greater() }
}

func hash_hash_int64(x int64) int64 {
	return x
}

func hash_hash_bool(x bool) int64 {
	if x { return 1 } else { return 0 }
}

func hash_hash_string(x string) int64 {
	var h int64 = 0
	for _, c := range x { h = h*31 + int64(c) }
	return h
}

func num_add_int64(x, y int64) int64 {
	return x + y
}
func num_sub_int64(x, y int64) int64 {
	return x - y
}
func num_mul_int64(x, y int64) int64 {
	return x * y
}
func num_div_int64(x, y int64) int64 {
	return x / y
}

func num_add_float64(x, y float64) float64 {
	return x + y
}
func num_sub_float64(x, y float64) float64 {
	return x - y
}
func num_mul_float64(x, y float64) float64 {
	return x * y
}
func num_div_float64(x, y float64) float64 {
	return x / y
}

func rem_rem_int64(x, y int64) int64 {
	return x % y
}

func neg_neg_int64(x int64) int64 {
	return -x
}

func neg_neg_float64(x float64) float64 {
	return -x
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
    _ = puts(out)
}
