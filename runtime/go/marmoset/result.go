package marmoset

type ResultState int

const (
	ResultInvalid ResultState = iota
	ResultSuccess
	ResultFailure
)

type Result[T, E any] struct {
	state   ResultState
	success T
	failure E
}

func Success[T, E any](value T) Result[T, E] {
	return Result[T, E]{state: ResultSuccess, success: value}
}

func Failure[T, E any](failure E) Result[T, E] {
	return Result[T, E]{state: ResultFailure, failure: failure}
}

func InspectResult[T, E any](result Result[T, E]) (ResultState, T, E) {
	return result.state, result.success, result.failure
}
