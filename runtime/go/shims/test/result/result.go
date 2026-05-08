package result

import (
	"strconv"

	"marmoset_out/marmoset"
)

func Parse(s string) marmoset.Result[int64, string] {
	value, err := strconv.ParseInt(s, 10, 64)
	if err != nil {
		return marmoset.Failure[int64, string](err.Error())
	}
	return marmoset.Success[int64, string](value)
}

func Invalid() marmoset.Result[int64, string] {
	var zero marmoset.Result[int64, string]
	return zero
}
