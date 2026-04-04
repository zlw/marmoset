package main

type Math__Point struct{x int64; y int64}

func math__add_int64_int64(x int64, y int64) int64 {
    return (x + y)
}

func math__move_record_x_int64_y_int64_closed_int64_int64(p Math__Point, dx int64, dy int64) Math__Point {
    return Math__Point{x: ((p).x + dx), y: ((p).y + dy)}
}

func values__identity_int64(x int64) int64 {
    return x
}


func main() {
    var main__p Math__Point = Math__Point{x: int64(1), y: int64(2)}
    _ = main__p
    main__moved := math__move_record_x_int64_y_int64_closed_int64_int64(main__p, int64(3), int64(4))
    _ = main__moved
    main__total := math__add_int64_int64((main__moved).x, (main__moved).y)
    _ = main__total
    main__final_u005fvalue := values__identity_int64(main__total)
    _ = main__final_u005fvalue
    _ = puts(main__final_u005fvalue)
}
