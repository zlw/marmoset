package main

import (
	"fmt"
	mshim_std_basics "marmoset_out/shims/std/basics"
	"strconv"
)

type Geometry__Point struct{x int64; y int64}

func extern__std_basics__puts_str(value string) struct{} {
	defer func() {
		if __panic := recover(); __panic != nil {
			panic(fmt.Sprintf("shim ABI violation at std/basics.puts_str: %v", __panic))
		}
	}()
	mshim_std_basics.PutsStr(value)
	return struct{}{}
}

func geometry__make_u005fpoint_int64_int64(x int64, y int64) Geometry__Point {
    return Geometry__Point{x: x, y: y}
}

func geometry__distance_record_x_int64_y_int64_closed(p Geometry__Point) int64 {
    return inherent_distance_record_x_int64_y_int64_closed(p)
}

func puts_int64(value int64) struct{} {
    return std__basics__puts_u005fstr_string(show_show_int64(value))
}

func puts_string(value string) struct{} {
    return std__basics__puts_u005fstr_string(show_show_string(value))
}

func std__basics__puts_u005fstr_string(value string) struct{} {
    return extern__std_basics__puts_str(value)
}

func show_show_int64(x int64) string {
	return strconv.FormatInt(x, 10)
}

func show_show_string(x string) string {
	return x
}
func geometry__Show_show_record_x_int64_y_int64_closed(self Geometry__Point) string {
        return "Point"
}

func inherent_distance_record_x_int64_y_int64_closed(self Geometry__Point) int64 {
        return ((self).x + (self).y)
}

func main() {
    var main__p Geometry__Point = geometry__make_u005fpoint_int64_int64(int64(2), int64(3))
    _ = main__p
    main__d := inherent_distance_record_x_int64_y_int64_closed(main__p)
    _ = main__d
    main__s := geometry__Show_show_record_x_int64_y_int64_closed(main__p)
    _ = main__s
    _ = puts_int64((geometry__distance_record_x_int64_y_int64_closed(main__p) + main__d))
    _ = puts_string(main__s)
}
