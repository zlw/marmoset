package main

import (
	mext_path_filepath "path/filepath"
	mext_strings "strings"
)

func extern__path_filepath__Base(path string) string {
	return mext_path_filepath.Base(path)
}

func extern__strings__ToUpper(s string) string {
	return mext_strings.ToUpper(s)
}


func main() {
    _ = puts(extern__strings__ToUpper(extern__path_filepath__Base("/tmp/marmoset.txt")))
}
