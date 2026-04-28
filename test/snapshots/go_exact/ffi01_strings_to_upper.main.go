package main

import mext_strings "strings"

func extern__strings__ToUpper(s string) string {
	return mext_strings.ToUpper(s)
}


func main() {
    _ = puts(extern__strings__ToUpper("hello"))
}
