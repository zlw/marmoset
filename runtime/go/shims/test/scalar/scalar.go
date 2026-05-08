package scalar

import (
	"strings"

	"marmoset_out/marmoset"
)

func Upcase(s string) string {
	return strings.ToUpper(s)
}

func AddOne(i int64) int64 {
	return i + 1
}

func Maybe(flag bool) marmoset.Option[int64] {
	if flag {
		return marmoset.Some[int64](42)
	}
	return marmoset.None[int64]()
}
