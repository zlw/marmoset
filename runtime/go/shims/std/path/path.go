package path

import "path/filepath"

func Join(left string, right string) string {
	return filepath.Join(left, right)
}

func Dirname(path string) string {
	return filepath.Dir(path)
}

func Basename(path string) string {
	return filepath.Base(path)
}

func Extname(path string) string {
	return filepath.Ext(path)
}

func Clean(path string) string {
	return filepath.Clean(path)
}

func AbsoluteQ(path string) bool {
	return filepath.IsAbs(path)
}
