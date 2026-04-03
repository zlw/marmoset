package main

import (
	"fmt"
	"strconv"
)

type marmosetDyn struct{ payload any; witness any }

type marmosetDynWitness_show struct{show func(any) string}
func __marmoset_dyn_adapter_marmosetDynWitness_show_int64_show(__receiver any) string {
	return show_show_int64(__receiver.(int64))
}
var __marmoset_dyn_witness_marmosetDynWitness_show_int64 = marmosetDynWitness_show{show: __marmoset_dyn_adapter_marmosetDynWitness_show_int64_show}

func __marmoset_dyn_adapter_marmosetDynWitness_show_string_show(__receiver any) string {
	return show_show_string(__receiver.(string))
}
var __marmoset_dyn_witness_marmosetDynWitness_show_string = marmosetDynWitness_show{show: __marmoset_dyn_adapter_marmosetDynWitness_show_string_show}


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


type Option_string struct {
	Data0 string
	Tag int8
}

const Option_string_Some_tag = 0
const Option_string_None_tag = 1

func Option_string_Some(v0 string) Option_string {
	return Option_string{Tag: Option_string_Some_tag, Data0: v0}
}

func Option_string_None() Option_string {
	return Option_string{Tag: Option_string_None_tag}
}

func (e Option_string) String() string {
	switch e.Tag {
	case Option_string_Some_tag:
		return fmt.Sprintf("Some(%v)", e.Data0)
	case Option_string_None_tag:
		return "None"
	default:
		panic("unreachable: invalid enum tag")
	}
}


type Option_dyn_Show struct {
	Data0 marmosetDyn
	Tag int8
}

const Option_dyn_Show_Some_tag = 0
const Option_dyn_Show_None_tag = 1

func Option_dyn_Show_Some(v0 marmosetDyn) Option_dyn_Show {
	return Option_dyn_Show{Tag: Option_dyn_Show_Some_tag, Data0: v0}
}

func Option_dyn_Show_None() Option_dyn_Show {
	return Option_dyn_Show{Tag: Option_dyn_Show_None_tag}
}

func (e Option_dyn_Show) String() string {
	switch e.Tag {
	case Option_dyn_Show_Some_tag:
		return fmt.Sprintf("Some(%v)", e.Data0)
	case Option_dyn_Show_None_tag:
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


func render_Option_dyn_Show(value Option_dyn_Show) string {
    __scrutinee_0 := value
    switch __scrutinee_0.Tag {
	case Option_dyn_Show_Some_tag:
		v := (__scrutinee_0).Data0
		_ = v
            return (func() string { __dyn := v; __witness := __dyn.witness.(marmosetDynWitness_show); return __witness.show(__dyn.payload) })()

	case Option_dyn_Show_None_tag:
            return "none"

	default:
		panic("unreachable: invalid enum tag")
    }
}

func via_if_bool(flag bool) Option_dyn_Show {
    if flag {
        return Option_dyn_Show_Some(marmosetDyn{payload: int64(42), witness: __marmoset_dyn_witness_marmosetDynWitness_show_int64})
    } else {
        return Option_dyn_Show_Some(marmosetDyn{payload: "ok", witness: __marmoset_dyn_witness_marmosetDynWitness_show_string})
    }
}

func via_match_bool(flag bool) Option_dyn_Show {
    __scrutinee_1 := flag
    switch __scrutinee_1 {
        case true:
            return Option_dyn_Show_Some(marmosetDyn{payload: "left", witness: __marmoset_dyn_witness_marmosetDynWitness_show_string})
        case false:
            return Option_dyn_Show_Some(marmosetDyn{payload: int64(0), witness: __marmoset_dyn_witness_marmosetDynWitness_show_int64})

    default:
    	panic("unreachable: exhaustive match")
    }
}

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
    var direct_payload Option_dyn_Show = Option_dyn_Show_Some(func() marmosetDyn {
        if true {
            return marmosetDyn{payload: int64(42), witness: __marmoset_dyn_witness_marmosetDynWitness_show_int64}
        } else {
            return marmosetDyn{payload: "unused", witness: __marmoset_dyn_witness_marmosetDynWitness_show_string}
        }
    }())
    _ = direct_payload
    _ = puts(render_Option_dyn_Show(direct_payload))
    _ = puts(render_Option_dyn_Show(via_if_bool(false)))
    _ = puts(render_Option_dyn_Show(via_match_bool(true)))
    _ = puts(render_Option_dyn_Show(via_match_bool(false)))
}
