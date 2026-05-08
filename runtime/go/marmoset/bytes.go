package marmoset

type Bytes struct {
	data []byte
}

func BytesCopy(input []byte) Bytes {
	copied := append([]byte(nil), input...)
	return Bytes{data: copied}
}

func BytesFromString(input string) Bytes {
	return BytesCopy([]byte(input))
}

func (bytes Bytes) Copy() []byte {
	return append([]byte(nil), bytes.data...)
}
