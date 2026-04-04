package main

type Fmt__Point struct{x int64; y int64}

func fmt__join_string_string(a string, b string) string {
    return ((a + ":") + b)
}

func fmt__format_u005fpoint_record_x_int64_y_int64_closed(p Fmt__Point) string {
    return fmt__join_string_string("x", "y")
}


func main() {
    var main__p Fmt__Point = Fmt__Point{x: int64(1), y: int64(2)}
    _ = main__p
    _ = puts(fmt__join_string_string("left", "right"))
    _ = puts(fmt__format_u005fpoint_record_x_int64_y_int64_closed(main__p))
}
