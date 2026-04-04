package main

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
