package marmoset

import "reflect"

func TypeIs[T any](v T, typeName string) bool {
	return reflect.TypeOf(v).String() == typeName
}

func IndexArr[T any](arr []T, i int64) T {
	if i < 0 {
		i = int64(len(arr)) + i
	}
	return arr[i]
}

func IndexStr(s string, i int64) string {
	if i < 0 {
		i = int64(len(s)) + i
	}
	return string(s[i])
}

func First[T any](arr []T) T {
	if len(arr) == 0 {
		var zero T
		return zero
	}
	return arr[0]
}

func Last[T any](arr []T) T {
	if len(arr) == 0 {
		var zero T
		return zero
	}
	return arr[len(arr)-1]
}

func Rest[T any](arr []T) []T {
	if len(arr) == 0 {
		return []T{}
	}
	return arr[1:]
}

func Push[T any](arr []T, v T) []T {
	return append(arr, v)
}
