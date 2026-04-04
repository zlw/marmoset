package main

import "fmt"

type Box struct {
	Data0 *u92_u005fnested_u005fconstructor_u005fpayload_u005fpattern_u005flowers__Option_int64
	Tag int8
}

const Box_Wrap_tag = 0
const Box_Empty_tag = 1

func Box_Wrap(v0 u92_u005fnested_u005fconstructor_u005fpayload_u005fpattern_u005flowers__Option_int64) Box {
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


type u92_u005fnested_u005fconstructor_u005fpayload_u005fpattern_u005flowers__Option_int64 struct {
	Data0 int64
	Tag int8
}

const u92_u005fnested_u005fconstructor_u005fpayload_u005fpattern_u005flowers__Option_int64_Some_tag = 0
const u92_u005fnested_u005fconstructor_u005fpayload_u005fpattern_u005flowers__Option_int64_None_tag = 1

func u92_u005fnested_u005fconstructor_u005fpayload_u005fpattern_u005flowers__Option_int64_Some(v0 int64) u92_u005fnested_u005fconstructor_u005fpayload_u005fpattern_u005flowers__Option_int64 {
	return u92_u005fnested_u005fconstructor_u005fpayload_u005fpattern_u005flowers__Option_int64{Tag: u92_u005fnested_u005fconstructor_u005fpayload_u005fpattern_u005flowers__Option_int64_Some_tag, Data0: v0}
}

func u92_u005fnested_u005fconstructor_u005fpayload_u005fpattern_u005flowers__Option_int64_None() u92_u005fnested_u005fconstructor_u005fpayload_u005fpattern_u005flowers__Option_int64 {
	return u92_u005fnested_u005fconstructor_u005fpayload_u005fpattern_u005flowers__Option_int64{Tag: u92_u005fnested_u005fconstructor_u005fpayload_u005fpattern_u005flowers__Option_int64_None_tag}
}

func (e u92_u005fnested_u005fconstructor_u005fpayload_u005fpattern_u005flowers__Option_int64) String() string {
	switch e.Tag {
	case u92_u005fnested_u005fconstructor_u005fpayload_u005fpattern_u005flowers__Option_int64_Some_tag:
		return fmt.Sprintf("Some(%v)", e.Data0)
	case u92_u005fnested_u005fconstructor_u005fpayload_u005fpattern_u005flowers__Option_int64_None_tag:
		return "None"
	default:
		panic("unreachable: invalid enum tag")
	}
}



func main() {
    value := Box_Wrap(u92_u005fnested_u005fconstructor_u005fpayload_u005fpattern_u005flowers__Option_int64_Some(int64(41)))
    _ = value
    var out int64
    __scrutinee_0 := value
    switch __scrutinee_0.Tag {
	case Box_Wrap_tag:
		if (((*((__scrutinee_0).Data0))).Tag == u92_u005fnested_u005fconstructor_u005fpayload_u005fpattern_u005flowers__Option_int64_Some_tag) {
			n := ((*((__scrutinee_0).Data0))).Data0
			_ = n
                out = (n + int64(1))
		} else if (((*((__scrutinee_0).Data0))).Tag == u92_u005fnested_u005fconstructor_u005fpayload_u005fpattern_u005flowers__Option_int64_None_tag) {
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
