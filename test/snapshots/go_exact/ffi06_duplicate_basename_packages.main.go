package main

import (
	mext_other_filepath "other/filepath"
	mext_path_filepath "path/filepath"
)

func extern__other_filepath__Base(path string) string {
	return mext_other_filepath.Base(path)
}

func extern__path_filepath__Base(path string) string {
	return mext_path_filepath.Base(path)
}


func main() {
    _ = extern__path_filepath__Base("a")
    _ = extern__other_filepath__Base("b")
}
