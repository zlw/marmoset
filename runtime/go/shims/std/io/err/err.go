package err

import (
	"errors"
	"os"
	"syscall"

	errapi "marmoset_out/api/std/io/err"
	"marmoset_out/marmoset"
)

func Write(value string) marmoset.Result[marmoset.Unit, errapi.Error] {
	if _, err := os.Stderr.WriteString(value); err != nil {
		return marmoset.Failure[marmoset.Unit, errapi.Error](writeError(err))
	}
	return marmoset.Success[marmoset.Unit, errapi.Error](marmoset.NewUnit())
}

func Flush() marmoset.Result[marmoset.Unit, errapi.Error] {
	return marmoset.Success[marmoset.Unit, errapi.Error](marmoset.NewUnit())
}

func writeError(err error) errapi.Error {
	if errors.Is(err, syscall.EPIPE) {
		return errapi.ErrorBrokenPipe{}
	}
	if errors.Is(err, syscall.EINTR) {
		return errapi.ErrorInterrupted{}
	}
	return errapi.ErrorOther{Field0: err.Error()}
}
