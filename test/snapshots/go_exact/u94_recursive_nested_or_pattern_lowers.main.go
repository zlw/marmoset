package main

import "fmt"

type Box struct {
	Data0 *Record_payload_u94_u005frecursive_u005fnested_u005for_u005fpattern_u005flowers__Option_int64_tag_int64
	Tag int8
}

const Box_Wrap_tag = 0
const Box_Empty_tag = 1

func Box_Wrap(v0 Record_payload_u94_u005frecursive_u005fnested_u005for_u005fpattern_u005flowers__Option_int64_tag_int64) Box {
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


type u94_u005frecursive_u005fnested_u005for_u005fpattern_u005flowers__Option_int64 struct {
	Data0 int64
	Tag int8
}

const u94_u005frecursive_u005fnested_u005for_u005fpattern_u005flowers__Option_int64_Some_tag = 0
const u94_u005frecursive_u005fnested_u005for_u005fpattern_u005flowers__Option_int64_None_tag = 1

func u94_u005frecursive_u005fnested_u005for_u005fpattern_u005flowers__Option_int64_Some(v0 int64) u94_u005frecursive_u005fnested_u005for_u005fpattern_u005flowers__Option_int64 {
	return u94_u005frecursive_u005fnested_u005for_u005fpattern_u005flowers__Option_int64{Tag: u94_u005frecursive_u005fnested_u005for_u005fpattern_u005flowers__Option_int64_Some_tag, Data0: v0}
}

func u94_u005frecursive_u005fnested_u005for_u005fpattern_u005flowers__Option_int64_None() u94_u005frecursive_u005fnested_u005for_u005fpattern_u005flowers__Option_int64 {
	return u94_u005frecursive_u005fnested_u005for_u005fpattern_u005flowers__Option_int64{Tag: u94_u005frecursive_u005fnested_u005for_u005fpattern_u005flowers__Option_int64_None_tag}
}

func (e u94_u005frecursive_u005fnested_u005for_u005fpattern_u005flowers__Option_int64) String() string {
	switch e.Tag {
	case u94_u005frecursive_u005fnested_u005for_u005fpattern_u005flowers__Option_int64_Some_tag:
		return fmt.Sprintf("Some(%v)", e.Data0)
	case u94_u005frecursive_u005fnested_u005for_u005fpattern_u005flowers__Option_int64_None_tag:
		return "None"
	default:
		panic("unreachable: invalid enum tag")
	}
}


type Record_payload_u94_u005frecursive_u005fnested_u005for_u005fpattern_u005flowers__Option_int64_tag_int64 struct{payload u94_u005frecursive_u005fnested_u005for_u005fpattern_u005flowers__Option_int64; tag int64}


func main() {
    value := Box_Wrap(Record_payload_u94_u005frecursive_u005fnested_u005for_u005fpattern_u005flowers__Option_int64_tag_int64{payload: u94_u005frecursive_u005fnested_u005for_u005fpattern_u005flowers__Option_int64_Some(int64(5)), tag: int64(1)})
    _ = value
    var out int64
    __scrutinee_0 := value
    switch __scrutinee_0.Tag {
	case Box_Wrap_tag:
		if (((*((__scrutinee_0).Data0))).tag == int64(0)) && ((((*((__scrutinee_0).Data0))).payload).Tag == u94_u005frecursive_u005fnested_u005for_u005fpattern_u005flowers__Option_int64_Some_tag) {
			v := (((*((__scrutinee_0).Data0))).payload).Data0
			_ = v
                out = v
		} else if (((*((__scrutinee_0).Data0))).tag == int64(1)) && ((((*((__scrutinee_0).Data0))).payload).Tag == u94_u005frecursive_u005fnested_u005for_u005fpattern_u005flowers__Option_int64_Some_tag) {
			v := (((*((__scrutinee_0).Data0))).payload).Data0
			_ = v
                out = v
		} else {
                out = int64(0)
		}
	case Box_Empty_tag:
            out = int64(0)

	default:
		panic("unreachable: invalid enum tag")
    }
    _ = out
    _ = puts(out)
}
