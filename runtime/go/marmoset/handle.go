package marmoset

import (
	"sync"
	"sync/atomic"
)

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

type HandleTable[Tag any, Value any] struct {
	mu     sync.Mutex
	next   uint64
	values map[uint64]Value
}

func NewHandleTable[Tag any, Value any]() *HandleTable[Tag, Value] {
	return &HandleTable[Tag, Value]{
		values: make(map[uint64]Value),
	}
}

func (table *HandleTable[Tag, Value]) Insert(value Value) Handle[Tag] {
	table.mu.Lock()
	defer table.mu.Unlock()

	if table.values == nil {
		table.values = make(map[uint64]Value)
	}
	table.next++
	if table.next == 0 {
		table.next++
	}
	handle := Handle[Tag]{id: table.next}
	table.values[handle.id] = value
	return handle
}

func (table *HandleTable[Tag, Value]) Get(handle Handle[Tag]) (Value, bool) {
	table.mu.Lock()
	defer table.mu.Unlock()

	var zero Value
	if handle.id == 0 || table.values == nil {
		return zero, false
	}
	value, ok := table.values[handle.id]
	return value, ok
}

func (table *HandleTable[Tag, Value]) Delete(handle Handle[Tag]) bool {
	table.mu.Lock()
	defer table.mu.Unlock()

	if handle.id == 0 || table.values == nil {
		return false
	}
	if _, ok := table.values[handle.id]; !ok {
		return false
	}
	delete(table.values, handle.id)
	return true
}
