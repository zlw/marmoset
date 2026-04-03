package main

import "fmt"

type marmosetDyn struct{ typeID string; payload any; witness any }

type marmosetDynWitness_Pack struct{label func(any) string; show func(any) string}

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


type Point struct{x int64}

func apply_record_x_int64_closed_fn_record_x_int64_closed_string(x Point, f func(Point) string) string {
    return f(x)
}

func show_show_int64(x int64) string {
	return fmt.Sprintf("%d", x)
}

func show_show_bool(x bool) string {
	return fmt.Sprintf("%t", x)
}

func show_show_string(x string) string {
	return x
}

func show_show_float64(x float64) string {
	return fmt.Sprintf("%g", x)
}

func debug_debug_int64(x int64) string {
	return fmt.Sprintf("%d", x)
}

func debug_debug_bool(x bool) string {
	return fmt.Sprintf("%t", x)
}

func debug_debug_string(x string) string {
	return fmt.Sprintf("%q", x)
}

func debug_debug_float64(x float64) string {
	return fmt.Sprintf("%g", x)
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
func Label_label_record_x_int64_closed(self Point) string {
        return apply_record_x_int64_closed_fn_record_x_int64_closed_string(self, func(x Point) string {
        return show_show_record_x_int64_closed(x)
    })
}

func Pack_label_record_x_int64_closed(self Point) string {
        return apply_record_x_int64_closed_fn_record_x_int64_closed_string(self, func(x Point) string {
        return show_show_record_x_int64_closed(x)
    })
}

func show_show_record_x_int64_closed(x Point) string {
	return fmt.Sprintf("{ x: %v }", x.x)
}

func main() {
    var value marmosetDyn = (func() marmosetDyn { __payload := Point{x: int64(1)}; return marmosetDyn{typeID: "{ x: Int }", payload: __payload, witness: marmosetDynWitness_Pack{label: func(__receiver any) string { return Pack_label_record_x_int64_closed(__receiver.(Point)) }, show: func(__receiver any) string { return show_show_record_x_int64_closed(__receiver.(Point)) }}} })()
    _ = value
    _ = puts((func() string { __dyn := value; __witness := __dyn.witness.(marmosetDynWitness_Pack); return __witness.show(__dyn.payload) })())
    _ = puts((func() string { __dyn := value; __witness := __dyn.witness.(marmosetDynWitness_Pack); return __witness.label(__dyn.payload) })())
}
