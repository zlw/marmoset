package main

import (
	"fmt"
	mshim_std_basics "marmoset_out/shims/std/basics"
)

type marmosetDyn struct{ payload any; witness any }

type marmosetDynWitness_show struct{show func(any) string}
func __marmoset_dyn_adapter_marmosetDynWitness_show_record_value_int64_closed_show(__receiver any) string {
	return show_show_record_value_int64_closed(__receiver.(Record_value_int64))
}
var __marmoset_dyn_witness_marmosetDynWitness_show_record_value_int64_closed = marmosetDynWitness_show{show: __marmoset_dyn_adapter_marmosetDynWitness_show_record_value_int64_closed_show}


type Record_value_int64 struct{value int64}

func extern__std_basics__puts_str(value string) struct{} {
	defer func() {
		if __panic := recover(); __panic != nil {
			panic(fmt.Sprintf("shim ABI violation at std/basics.puts_str: %v", __panic))
		}
	}()
	mshim_std_basics.PutsStr(value)
	return struct{}{}
}

func puts_string(value string) struct{} {
    return std__basics__puts_u005fstr_string(show_show_string(value))
}

func std__basics__puts_u005fstr_string(value string) struct{} {
    return extern__std_basics__puts_str(value)
}

func show_show_string(x string) string {
	return x
}
func Boxed_box_record_value_int64_closed(self Record_value_int64) marmosetDyn {
        return marmosetDyn{payload: self, witness: __marmoset_dyn_witness_marmosetDynWitness_show_record_value_int64_closed}
}

func show_show_record_value_int64_closed(x Record_value_int64) string {
	return fmt.Sprintf("{ value: %v }", x.value)
}

func main() {
    value := Boxed_box_record_value_int64_closed(Record_value_int64{value: int64(1)})
    _ = value
    _ = puts_string((func() string { __dyn := value; __witness := __dyn.witness.(marmosetDynWitness_show); return __witness.show(__dyn.payload) })())
}
