package main

import "strconv"

type marmosetDyn struct{ payload any; witness any }

type marmosetDynWitness_show struct{show func(any) string}
func __marmoset_dyn_adapter_marmosetDynWitness_show_int64_show(__receiver any) string {
	return show_show_int64(__receiver.(int64))
}
var __marmoset_dyn_witness_marmosetDynWitness_show_int64 = marmosetDynWitness_show{show: __marmoset_dyn_adapter_marmosetDynWitness_show_int64_show}


type Box struct{tag int64; value marmosetDyn}
type Record_tag_int64_value_int64 struct{tag int64; value int64}

func show_show_int64(x int64) string {
	return strconv.FormatInt(x, 10)
}
func main() {
    var box Box = (func(__src Record_tag_int64_value_int64) Box { return Box{tag: __src.tag, value: marmosetDyn{payload: __src.value, witness: __marmoset_dyn_witness_marmosetDynWitness_show_int64}} })(Record_tag_int64_value_int64{tag: int64(0), value: int64(1)})
    _ = box
    __spread_0 := box
    var __field_payload_1 int64
        if true {
            __field_payload_1 = int64(42)
        } else {
            __field_payload_1 = int64(43)
        }
    updated := Box{tag: __spread_0.tag, value: marmosetDyn{payload: __field_payload_1, witness: __marmoset_dyn_witness_marmosetDynWitness_show_int64}}
    _ = updated
    _ = puts((func() string { __dyn := (updated).value; __witness := __dyn.witness.(marmosetDynWitness_show); return __witness.show(__dyn.payload) })())
}
