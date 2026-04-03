package main

import "strconv"

type math__Color struct {
	Tag int8
}

const math__Color_Red_tag = 0
const math__Color_Blue_tag = 1

func math__Color_Red() math__Color {
	return math__Color{Tag: math__Color_Red_tag}
}

func math__Color_Blue() math__Color {
	return math__Color{Tag: math__Color_Blue_tag}
}

func (e math__Color) String() string {
	switch e.Tag {
	case math__Color_Red_tag:
		return "Red"
	case math__Color_Blue_tag:
		return "Blue"
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


type Geometry__Point struct{x int64; y int64}

func math__describe_math__Color(c math__Color) string {
    __scrutinee_0 := c
    switch __scrutinee_0.Tag {
	case math__Color_Red_tag:
            return "red"

	case math__Color_Blue_tag:
            return "blue"

	default:
		panic("unreachable: invalid enum tag")
    }
}

func geometry__make_u005fpoint_int64_int64(x int64, y int64) Geometry__Point {
    return Geometry__Point{x: x, y: y}
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
func geometry__Show_show_record_x_int64_y_int64_closed(self Geometry__Point) string {
        return "Point"
}

func inherent_distance_record_x_int64_y_int64_closed(self Geometry__Point) int64 {
        return ((self).x + (self).y)
}

func main() {
    math__pi := int64(3)
    _ = math__pi
    main__p := geometry__make_u005fpoint_int64_int64(int64(2), int64(3))
    _ = main__p
    _ = puts(math__pi)
    _ = puts(math__describe_math__Color(math__Color_Red()))
    _ = puts(inherent_distance_record_x_int64_y_int64_closed(main__p))
    _ = puts(geometry__Show_show_record_x_int64_y_int64_closed(main__p))
}
