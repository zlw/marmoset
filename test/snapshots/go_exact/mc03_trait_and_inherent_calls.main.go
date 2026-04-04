package main

type Geometry__Point struct{x int64; y int64}

func geometry__make_u005fpoint_int64_int64(x int64, y int64) Geometry__Point {
    return Geometry__Point{x: x, y: y}
}

func geometry__distance_record_x_int64_y_int64_closed(p Geometry__Point) int64 {
    return inherent_distance_record_x_int64_y_int64_closed(p)
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
    _ = puts((geometry__distance_record_x_int64_y_int64_closed(main__p) + main__d))
    _ = puts(main__s)
}
