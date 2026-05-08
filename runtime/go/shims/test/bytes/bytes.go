package bytes

import "marmoset_out/marmoset"

func FromStr(input string) marmoset.Bytes {
	return marmoset.BytesFromString(input)
}

func ToStrLossy(input marmoset.Bytes) string {
	return string(input.Copy())
}

func AddSuffix(input marmoset.Bytes) marmoset.Bytes {
	data := input.Copy()
	data = append(data, []byte(":shim")...)
	return marmoset.BytesCopy(data)
}

func MutateAfterReturn(input string) marmoset.Bytes {
	data := []byte(input)
	result := marmoset.BytesCopy(data)
	if len(data) > 0 {
		data[0] = 'X'
	}
	return result
}

func MutateParam(input marmoset.Bytes) string {
	data := input.Copy()
	if len(data) > 0 {
		data[0] = 'X'
	}
	return string(data)
}
