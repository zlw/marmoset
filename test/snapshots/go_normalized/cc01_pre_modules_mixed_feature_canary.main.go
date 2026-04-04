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

type Event struct {
	Data0 int64
	Data1 *Record_box_record_tag_int64_value_marmoset_dyn_closed_owner_record_name_string_score_int64_closed
	Tag int8
}

const Event_Number_tag = 0
const Event_Wrapped_tag = 1
const Event_Empty_tag = 2

func Event_Number(v0 int64) Event {
	return Event{Tag: Event_Number_tag, Data0: v0}
}

func Event_Wrapped(v0 Record_box_record_tag_int64_value_marmoset_dyn_closed_owner_record_name_string_score_int64_closed) Event {
	return Event{Tag: Event_Wrapped_tag, Data1: &v0}
}

func Event_Empty() Event {
	return Event{Tag: Event_Empty_tag}
}

func (e Event) String() string {
	switch e.Tag {
	case Event_Number_tag:
		return fmt.Sprintf("Number(%v)", e.Data0)
	case Event_Wrapped_tag:
		return fmt.Sprintf("Wrapped(%v)", (*e.Data1))
	case Event_Empty_tag:
		return "Empty"
	default:
		panic("unreachable: invalid enum tag")
	}
}

type Box struct{tag int64; value marmosetDyn}
type Person struct{name string; score int64}
type Record_box_record_tag_int64_value_marmoset_dyn_closed_owner_record_name_string_score_int64_closed struct{box Box; owner Person}
type Record_tag_int64_value_int64 struct{tag int64; value int64}

func make_box_bool_int64(flag bool, seed int64) Box {
    return (func(__src Record_tag_int64_value_int64) Box { return Box{tag: __src.tag, value: marmosetDyn{payload: __src.value, witness: __marmoset_dyn_witness_marmosetDynWitness_show_int64}} })(Record_tag_int64_value_int64{tag: seed, value: func() int64 {
    if flag {
        return seed
    } else {
        return (seed + int64(1))
    }
}()})
}

func with_suffix_record_name_string_score_int64_closed_string(value Person, suffix string) string {
    return ((value).name + suffix)
}

func render_twice_int64_fn_int64_string(value int64, render func(int64) string) string {
    return ((render(value) + "|") + render(value))
}

func render_twice_record_name_string_score_int64_closed_fn_record_name_string_score_int64_closed_string(value Person, render func(Person) string) string {
    return ((render(value) + "|") + render(value))
}

func classify_Event(event Event) string {
    __scrutinee_3 := event
    switch __scrutinee_3.Tag {
	case Event_Number_tag:
		if ((__scrutinee_3).Data0 == int64(0)) {
                return "tiny"
		} else if ((__scrutinee_3).Data0 == int64(1)) {
                return "tiny"
		} else {
			panic("non-exhaustive enum match")
		}
	case Event_Wrapped_tag:
		if ((((*((__scrutinee_3).Data1))).owner).score == int64(7)) {
			name := (((*((__scrutinee_3).Data1))).owner).name
			box := ((*((__scrutinee_3).Data1))).box
			_ = name
			_ = box
                return ((name + ":7:") + inherent_describe_record_tag_int64_value_dyn_Show_closed(box))
		} else {
			owner := ((*((__scrutinee_3).Data1))).owner
			box := ((*((__scrutinee_3).Data1))).box
			_ = owner
			_ = box
                return ((Label_label_record_name_string_score_int64_closed(owner) + ":") + inherent_describe_record_tag_int64_value_dyn_Show_closed(box))
		}
	case Event_Empty_tag:
            return "empty"

	default:
		panic("unreachable: invalid enum tag")
    }
}

func describe_input_union_int64_string(input interface{}) string {
    switch input_typed := input.(type) {
    case int64:
                _ = input_typed
        return ("int:" + show_show_int64((input_typed + int64(1))))
        default:
                input_complement := input.(string)
                _ = input_complement
        return ("str:" + input_complement)
    }
}

func show_show_int64(x int64) string {
	return strconv.FormatInt(x, 10)
}

func show_show_string(x string) string {
	return x
}
func Label_label_record_name_string_score_int64_closed(self Person) string {
        return ((show_show_string((self).name) + "=") + show_show_record_name_string_score_int64_closed(self))
}

func inherent_rename_record_name_string_score_int64_closed(person Person, suffix string) Person {
        return (func() Person { __base := person; return Person{name: ((person).name + suffix), score: __base.score} })()
}

func inherent_describe_record_tag_int64_value_dyn_Show_closed(box Box) string {
        return (((func() string { __dyn := (box).value; __witness := __dyn.witness.(marmosetDynWitness_show); return __witness.show(__dyn.payload) })() + ":") + show_show_int64((box).tag))
}

func show_show_record_name_string_score_int64_closed(x Person) string {
	return fmt.Sprintf("{ name: %v, score: %v }", x.name, x.score)
}

func main() {
    person := inherent_rename_record_name_string_score_int64_closed(Person{name: "milo", score: int64(8)}, "!")
    _ = person
    box := make_box_bool_int64(true, int64(2))
    _ = box
    _ = box
    var __field_payload_1 int64
        __scrutinee_2 := Event_Number(int64(2))
        switch __scrutinee_2.Tag {
	case Event_Number_tag:
		n := (__scrutinee_2).Data0
		_ = n
                __field_payload_1 = (n + int64(40))

	case Event_Wrapped_tag:
                __field_payload_1 = int64(0)

	case Event_Empty_tag:
                __field_payload_1 = int64(0)

	default:
		panic("unreachable: invalid enum tag")
        }
    boosted := Box{tag: ((box).tag + int64(1)), value: marmosetDyn{payload: __field_payload_1, witness: __marmoset_dyn_witness_marmosetDynWitness_show_int64}}
    _ = boosted
    exact := Event_Wrapped(Record_box_record_tag_int64_value_marmoset_dyn_closed_owner_record_name_string_score_int64_closed{box: make_box_bool_int64(true, int64(2)), owner: Person{name: "ada", score: int64(7)}})
    _ = exact
    event := Event_Wrapped(Record_box_record_tag_int64_value_marmoset_dyn_closed_owner_record_name_string_score_int64_closed{box: boosted, owner: person})
    _ = event
    _ = puts(classify_Event(Event_Number(int64(1))))
    _ = puts(classify_Event(exact))
    _ = puts(inherent_describe_record_tag_int64_value_dyn_Show_closed(boosted))
    _ = puts(classify_Event(event))
    _ = puts(render_twice_record_name_string_score_int64_closed_fn_record_name_string_score_int64_closed_string(person, func(p Person) string {
        return with_suffix_record_name_string_score_int64_closed_string(p, "?")
    }))
    _ = puts(render_twice_int64_fn_int64_string(int64(3), show_show_int64))
    _ = puts(describe_input_union_int64_string(int64(4)))
    _ = puts(describe_input_union_int64_string("ok"))
}
