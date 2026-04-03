package main

import "fmt"

type Box struct {
	Data0 *Option_int64
	Tag int8
}

const Box_Wrap_tag = 0
const Box_Empty_tag = 1

func Box_Wrap(v0 Option_int64) Box {
	return Box{Tag: Box_Wrap_tag, Data0: &v0}
}

func Box_Empty() Box {
	return Box{Tag: Box_Empty_tag}
}

func (e Box) String() string {
	switch e.Tag {
	case Box_Wrap_tag:
		return fmt.Sprintf("Wrap(%v)", (*e.Data0))
	case Box_Empty_tag:
		return "Empty"
	default:
		panic("unreachable: invalid enum tag")
	}
}


type Option_int64 struct {
	Data0 int64
	Tag int8
}

const Option_int64_Some_tag = 0
const Option_int64_None_tag = 1

func Option_int64_Some(v0 int64) Option_int64 {
	return Option_int64{Tag: Option_int64_Some_tag, Data0: v0}
}

func Option_int64_None() Option_int64 {
	return Option_int64{Tag: Option_int64_None_tag}
}

func (e Option_int64) String() string {
	switch e.Tag {
	case Option_int64_Some_tag:
		return fmt.Sprintf("Some(%v)", e.Data0)
	case Option_int64_None_tag:
		return "None"
	default:
		panic("unreachable: invalid enum tag")
	}
}


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
    value := Box_Wrap(Option_int64_Some(int64(41)))
    _ = value
    var out int64
    __scrutinee_0 := value
    switch __scrutinee_0.Tag {
	case Box_Wrap_tag:
		if (((*((__scrutinee_0).Data0))).Tag == Option_int64_Some_tag) {
			n := ((*((__scrutinee_0).Data0))).Data0
			_ = n
                out = (n + int64(1))
		} else if (((*((__scrutinee_0).Data0))).Tag == Option_int64_None_tag) {
                out = int64(0)
		} else {
			panic("non-exhaustive enum match")
		}
	case Box_Empty_tag:
            out = int64(0)

	default:
		panic("unreachable: invalid enum tag")
    }
    _ = out
    _ = puts(out)
}
