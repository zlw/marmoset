package bytes

import (
	stdbytes "bytes"
	"unicode/utf8"

	bytesapi "marmoset_out/api/std/bytes"
	"marmoset_out/marmoset"
)

func FromStr(input string) marmoset.Bytes {
	return marmoset.BytesFromString(input)
}

func ToStrLossy(input marmoset.Bytes) string {
	return string(input.Copy())
}

func ToStr(input marmoset.Bytes) marmoset.Result[string, bytesapi.DecodeError] {
	data := input.Copy()
	if !utf8.Valid(data) {
		return marmoset.Failure[string, bytesapi.DecodeError](bytesapi.DecodeErrorInvalidUtf8{})
	}
	return marmoset.Success[string, bytesapi.DecodeError](string(data))
}

func Length(input marmoset.Bytes) int64 {
	return int64(len(input.Copy()))
}

func EqualQ(left marmoset.Bytes, right marmoset.Bytes) bool {
	return stdbytes.Equal(left.Copy(), right.Copy())
}
