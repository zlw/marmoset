package main

func __section_neg28_int64(it int64) int64 {
    return (it + int64(41))
}


func Child_cast_int64__int64(x int64, f func(int64) int64) int64 {
        return f(x)
}

func main() {
    _ = puts(Child_cast_int64__int64(int64(1), __section_neg28_int64))
}
