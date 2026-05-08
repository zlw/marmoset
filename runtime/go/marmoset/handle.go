package marmoset

import "sync/atomic"

type Handle[Tag any] struct {
	id uint64
}

var nextHandleID uint64

func NewHandle[Tag any]() Handle[Tag] {
	id := atomic.AddUint64(&nextHandleID, 1)
	if id == 0 {
		id = atomic.AddUint64(&nextHandleID, 1)
	}
	return Handle[Tag]{id: id}
}

func HandleIsValid[Tag any](handle Handle[Tag]) bool {
	return handle.id != 0
}
