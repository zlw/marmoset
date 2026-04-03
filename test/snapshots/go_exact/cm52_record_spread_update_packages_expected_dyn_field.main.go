package main

import "fmt"

type marmosetDyn struct{ typeID string; payload any; witness any }

type marmosetDynWitness_show struct{show func(any) string}

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


type Box struct{tag int64; value marmosetDyn}
type Record_tag_int64_value_int64 struct{tag int64; value int64}

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
func main() {
    var box Box = (func(__src Record_tag_int64_value_int64) Box { return Box{tag: __src.tag, value: (func() marmosetDyn { __payload := __src.value; return marmosetDyn{typeID: "Int", payload: __payload, witness: marmosetDynWitness_show{show: func(__receiver any) string { return show_show_int64(__receiver.(int64)) }}} })()} })(Record_tag_int64_value_int64{tag: int64(0), value: int64(1)})
    _ = box
    __spread_0 := box
    updated := Box{tag: __spread_0.tag, value: (func() marmosetDyn { __payload := func() int64 {
        if true {
            return int64(42)
        } else {
            return int64(43)
        }
    }(); return marmosetDyn{typeID: "Int", payload: __payload, witness: marmosetDynWitness_show{show: func(__receiver any) string { return show_show_int64(__receiver.(int64)) }}} })()}
    _ = updated
    _ = puts((func() string { __dyn := (updated).value; __witness := __dyn.witness.(marmosetDynWitness_show); return __witness.show(__dyn.payload) })())
}
