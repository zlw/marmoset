package io

import (
	"bufio"
	"errors"
	"fmt"
	goio "io"
	"os"
	"strings"

	ioapi "marmoset_out/api/std/io"
	"marmoset_out/marmoset"
)

var stdin = bufio.NewReader(os.Stdin)

func PrintStr(value string) marmoset.Unit {
	fmt.Print(value)
	return marmoset.NewUnit()
}

func PrintlnStr(value string) marmoset.Unit {
	fmt.Println(value)
	return marmoset.NewUnit()
}

func readLineValue() (string, ioapi.ReadLineError, bool) {
	line, err := stdin.ReadString('\n')
	if err != nil {
		if errors.Is(err, goio.EOF) && len(line) > 0 {
			return trimLineEnding(line), nil, true
		}
		if errors.Is(err, goio.EOF) {
			return "", ioapi.ReadLineErrorEndOfFile{}, false
		}
		return "", ioapi.ReadLineErrorOther{Field0: err.Error()}, false
	}
	return trimLineEnding(line), nil, true
}

func ReadLine() marmoset.Result[string, ioapi.ReadLineError] {
	line, readErr, ok := readLineValue()
	if !ok {
		return marmoset.Failure[string, ioapi.ReadLineError](readErr)
	}
	return marmoset.Success[string, ioapi.ReadLineError](line)
}

func MapLine(body func(string) string) marmoset.Result[string, ioapi.ReadLineError] {
	line, readErr, ok := readLineValue()
	if !ok {
		return marmoset.Failure[string, ioapi.ReadLineError](readErr)
	}
	return marmoset.Success[string, ioapi.ReadLineError](body(line))
}

func WithLine(body func(string) marmoset.Unit) marmoset.Result[marmoset.Unit, ioapi.ReadLineError] {
	line, readErr, ok := readLineValue()
	if !ok {
		return marmoset.Failure[marmoset.Unit, ioapi.ReadLineError](readErr)
	}
	body(line)
	return marmoset.Success[marmoset.Unit, ioapi.ReadLineError](marmoset.NewUnit())
}

func trimLineEnding(line string) string {
	line = strings.TrimSuffix(line, "\n")
	return strings.TrimSuffix(line, "\r")
}
