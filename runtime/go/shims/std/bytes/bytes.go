package bytes

import (
	stdbytes "bytes"

	"marmoset_out/marmoset"
)

func FromStr(input string) marmoset.Bytes {
	return marmoset.BytesFromString(input)
}

func ToStrLossy(input marmoset.Bytes) string {
	return string(input.Copy())
}

func Length(input marmoset.Bytes) int64 {
	return int64(len(input.Copy()))
}

func EqualQ(left marmoset.Bytes, right marmoset.Bytes) bool {
	return stdbytes.Equal(left.Copy(), right.Copy())
}
