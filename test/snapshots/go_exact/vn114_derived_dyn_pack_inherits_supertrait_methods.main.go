package main

import (
	"fmt"
	mshim_std_basics "marmoset_out/shims/std/basics"
)

type marmosetDyn struct{ payload any; witness any }

type marmosetDynWitness_Pack struct{label func(any) string; show func(any) string}
func __marmoset_dyn_adapter_marmosetDynWitness_Pack_record_x_int64_closed_label(__receiver any) string {
	return Pack_label_record_x_int64_closed(__receiver.(Point))
}

func __marmoset_dyn_adapter_marmosetDynWitness_Pack_record_x_int64_closed_show(__receiver any) string {
	return show_show_record_x_int64_closed(__receiver.(Point))
}
var __marmoset_dyn_witness_marmosetDynWitness_Pack_record_x_int64_closed = marmosetDynWitness_Pack{label: __marmoset_dyn_adapter_marmosetDynWitness_Pack_record_x_int64_closed_label, show: __marmoset_dyn_adapter_marmosetDynWitness_Pack_record_x_int64_closed_show}


type Point struct{x int64}

func extern__std_basics__puts_str(value string) struct{} {
	defer func() {
		if __panic := recover(); __panic != nil {
			panic(fmt.Sprintf("shim ABI violation at std/basics.puts_str: %v", __panic))
		}
	}()
	mshim_std_basics.PutsStr(value)
	return struct{}{}
}

func apply_record_x_int64_closed_fn_record_x_int64_closed_string(x Point, f func(Point) string) string {
    return f(x)
}

func puts_string(value string) struct{} {
    return extern__std_basics__puts_str(show_show_string(value))
}

func show_show_string(x string) string {
	return x
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
    var value marmosetDyn = marmosetDyn{payload: Point{x: int64(1)}, witness: __marmoset_dyn_witness_marmosetDynWitness_Pack_record_x_int64_closed}
    _ = value
    _ = puts_string((func() string { __dyn := value; __witness := __dyn.witness.(marmosetDynWitness_Pack); return __witness.show(__dyn.payload) })())
    _ = puts_string((func() string { __dyn := value; __witness := __dyn.witness.(marmosetDynWitness_Pack); return __witness.label(__dyn.payload) })())
}
