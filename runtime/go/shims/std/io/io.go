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

func ReadLine() marmoset.Result[string, ioapi.ReadLineError] {
	line, err := stdin.ReadString('\n')
	if err != nil {
		if errors.Is(err, goio.EOF) && len(line) > 0 {
			return marmoset.Success[string, ioapi.ReadLineError](trimLineEnding(line))
		}
		if errors.Is(err, goio.EOF) {
			return marmoset.Failure[string, ioapi.ReadLineError](ioapi.ReadLineErrorEndOfFile{})
		}
		return marmoset.Failure[string, ioapi.ReadLineError](ioapi.ReadLineErrorOther{Field0: err.Error()})
	}
	return marmoset.Success[string, ioapi.ReadLineError](trimLineEnding(line))
}

func trimLineEnding(line string) string {
	line = strings.TrimSuffix(line, "\n")
	return strings.TrimSuffix(line, "\r")
}
