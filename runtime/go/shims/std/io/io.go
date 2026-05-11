package io

import (
	"bufio"
	"errors"
	goio "io"
	"os"
	"strings"
	"syscall"

	ioapi "marmoset_out/api/std/io"
	"marmoset_out/marmoset"
)

var stdin = bufio.NewReader(os.Stdin)

func Read() marmoset.Result[string, ioapi.Error] {
	line, err := stdin.ReadString('\n')
	if err != nil {
		if errors.Is(err, goio.EOF) && len(line) > 0 {
			return marmoset.Success[string, ioapi.Error](trimLineEnding(line))
		}
		if errors.Is(err, goio.EOF) {
			return marmoset.Failure[string, ioapi.Error](ioapi.ErrorEndOfFile{})
		}
		return marmoset.Failure[string, ioapi.Error](readError(err))
	}
	return marmoset.Success[string, ioapi.Error](trimLineEnding(line))
}

func Write(value string) marmoset.Result[marmoset.Unit, ioapi.Error] {
	if _, err := os.Stdout.WriteString(value); err != nil {
		return marmoset.Failure[marmoset.Unit, ioapi.Error](writeError(err))
	}
	return marmoset.Success[marmoset.Unit, ioapi.Error](marmoset.NewUnit())
}

func Flush() marmoset.Result[marmoset.Unit, ioapi.Error] {
	return marmoset.Success[marmoset.Unit, ioapi.Error](marmoset.NewUnit())
}

func readError(err error) ioapi.Error {
	if errors.Is(err, syscall.EINTR) {
		return ioapi.ErrorInterrupted{}
	}
	return ioapi.ErrorOther{Field0: err.Error()}
}

func writeError(err error) ioapi.Error {
	if errors.Is(err, syscall.EPIPE) {
		return ioapi.ErrorBrokenPipe{}
	}
	if errors.Is(err, syscall.EINTR) {
		return ioapi.ErrorInterrupted{}
	}
	return ioapi.ErrorOther{Field0: err.Error()}
}

func trimLineEnding(line string) string {
	line = strings.TrimSuffix(line, "\n")
	return strings.TrimSuffix(line, "\r")
}
