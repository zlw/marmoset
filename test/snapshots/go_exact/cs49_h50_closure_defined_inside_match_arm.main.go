package main


func main() {
    n := int64(1)
    _ = n
    var f func(int64) int64
    __scrutinee_0 := n
    switch __scrutinee_0 {
        case int64(1):
            f = func(x int64) int64 {
                return (x + int64(5))
            }
        default:
            f = func(x int64) int64 {
                return x
            }

    }
    _ = f
    _ = puts(f(int64(5)))
}
