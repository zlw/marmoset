package marmoset

type OptionState int

const (
	OptionInvalid OptionState = iota
	OptionSome
	OptionNone
)

type Option[T any] struct {
	state OptionState
	value T
}

func Some[T any](value T) Option[T] {
	return Option[T]{state: OptionSome, value: value}
}

func None[T any]() Option[T] {
	return Option[T]{state: OptionNone}
}

func InspectOption[T any](option Option[T]) (OptionState, T) {
	return option.state, option.value
}
