package file

import (
	"bufio"
	"errors"
	"io"
	"os"
	"syscall"

	fileapi "marmoset_out/api/std/file"
	"marmoset_out/marmoset"
)

type fileResource struct {
	file   *os.File
	reader *bufio.Reader
}

var files = marmoset.NewHandleTable[fileapi.FileTag, *fileResource]()

func isDirectoryPath(path string) bool {
	info, err := os.Stat(path)
	return err == nil && info.IsDir()
}

func isNotExist(err error) bool {
	return errors.Is(err, os.ErrNotExist) || os.IsNotExist(err)
}

func isAlreadyExist(err error) bool {
	return errors.Is(err, os.ErrExist) || os.IsExist(err)
}

func isPermission(err error) bool {
	return errors.Is(err, os.ErrPermission) || os.IsPermission(err)
}

func isDirectoryError(path string, err error) bool {
	return isDirectoryPath(path) || errors.Is(err, syscall.EISDIR)
}

func isNotDirectory(err error) bool {
	return errors.Is(err, syscall.ENOTDIR)
}

func isInvalidPath(err error) bool {
	return errors.Is(err, syscall.EINVAL)
}

func fileError(path string, err error) fileapi.Error {
	switch {
	case err == nil:
		return fileapi.ErrorOther{Field0: ""}
	case isNotExist(err):
		return fileapi.ErrorNotFound{}
	case isPermission(err):
		return fileapi.ErrorPermissionDenied{}
	case isDirectoryError(path, err):
		return fileapi.ErrorIsDirectory{}
	case isNotDirectory(err):
		return fileapi.ErrorNotDirectory{}
	case isInvalidPath(err):
		return fileapi.ErrorInvalidPath{Field0: err.Error()}
	case isAlreadyExist(err):
		return fileapi.ErrorAlreadyExists{}
	default:
		return fileapi.ErrorOther{Field0: err.Error()}
	}
}

func handleFile(file fileapi.File) (*fileResource, bool) {
	return files.Get(file)
}

func (resource *fileResource) readBuffer() *bufio.Reader {
	if resource.reader == nil {
		resource.reader = bufio.NewReader(resource.file)
	}
	return resource.reader
}

func trimLineEnding(line []byte) []byte {
	if len(line) > 0 && line[len(line)-1] == '\n' {
		line = line[:len(line)-1]
	}
	if len(line) > 0 && line[len(line)-1] == '\r' {
		line = line[:len(line)-1]
	}
	return line
}

func ReadPath(path string) marmoset.Result[marmoset.Bytes, fileapi.Error] {
	data, err := os.ReadFile(path)
	if err != nil {
		return marmoset.Failure[marmoset.Bytes, fileapi.Error](fileError(path, err))
	}
	return marmoset.Success[marmoset.Bytes, fileapi.Error](marmoset.BytesCopy(data))
}

func WritePath(path string, bytes marmoset.Bytes) marmoset.Result[marmoset.Unit, fileapi.Error] {
	err := os.WriteFile(path, bytes.Copy(), 0o666)
	if err != nil {
		return marmoset.Failure[marmoset.Unit, fileapi.Error](fileError(path, err))
	}
	return marmoset.Success[marmoset.Unit, fileapi.Error](marmoset.NewUnit())
}

func AppendPath(path string, bytes marmoset.Bytes) marmoset.Result[marmoset.Unit, fileapi.Error] {
	file, err := os.OpenFile(path, os.O_WRONLY|os.O_CREATE|os.O_APPEND, 0o666)
	if err != nil {
		return marmoset.Failure[marmoset.Unit, fileapi.Error](fileError(path, err))
	}
	data := bytes.Copy()
	written, err := file.Write(data)
	if err != nil {
		_ = file.Close()
		return marmoset.Failure[marmoset.Unit, fileapi.Error](fileError(path, err))
	}
	if written != len(data) {
		_ = file.Close()
		return marmoset.Failure[marmoset.Unit, fileapi.Error](fileError(path, io.ErrShortWrite))
	}
	if err := file.Close(); err != nil {
		return marmoset.Failure[marmoset.Unit, fileapi.Error](fileError(path, err))
	}
	return marmoset.Success[marmoset.Unit, fileapi.Error](marmoset.NewUnit())
}

func openFlags(mode fileapi.Mode) int {
	switch mode.(type) {
	case fileapi.ModeRead:
		return os.O_RDONLY
	case fileapi.ModeWrite:
		return os.O_WRONLY | os.O_CREATE | os.O_TRUNC
	case fileapi.ModeAppend:
		return os.O_WRONLY | os.O_CREATE | os.O_APPEND
	default:
		panic("unknown std.file.Mode variant")
	}
}

func OpenHandle(path string, mode fileapi.Mode) marmoset.Result[fileapi.File, fileapi.Error] {
	if isDirectoryPath(path) {
		return marmoset.Failure[fileapi.File, fileapi.Error](fileapi.ErrorIsDirectory{})
	}
	file, err := os.OpenFile(path, openFlags(mode), 0o666)
	if err != nil {
		return marmoset.Failure[fileapi.File, fileapi.Error](fileError(path, err))
	}
	return marmoset.Success[fileapi.File, fileapi.Error](files.Insert(&fileResource{file: file}))
}

func CloseHandle(file fileapi.File) marmoset.Result[marmoset.Unit, fileapi.Error] {
	resource, ok := handleFile(file)
	if !ok {
		return marmoset.Failure[marmoset.Unit, fileapi.Error](fileapi.ErrorAlreadyClosed{})
	}
	if !files.Delete(file) {
		return marmoset.Failure[marmoset.Unit, fileapi.Error](fileapi.ErrorAlreadyClosed{})
	}
	if err := resource.file.Close(); err != nil {
		return marmoset.Failure[marmoset.Unit, fileapi.Error](fileError("", err))
	}
	return marmoset.Success[marmoset.Unit, fileapi.Error](marmoset.NewUnit())
}

func ReadHandle(file fileapi.File) marmoset.Result[marmoset.Bytes, fileapi.Error] {
	resource, ok := handleFile(file)
	if !ok {
		return marmoset.Failure[marmoset.Bytes, fileapi.Error](fileapi.ErrorAlreadyClosed{})
	}
	data, err := io.ReadAll(resource.readBuffer())
	if err != nil {
		return marmoset.Failure[marmoset.Bytes, fileapi.Error](fileError("", err))
	}
	return marmoset.Success[marmoset.Bytes, fileapi.Error](marmoset.BytesCopy(data))
}

func ReadLineHandle(file fileapi.File) marmoset.Result[marmoset.Option[marmoset.Bytes], fileapi.Error] {
	resource, ok := handleFile(file)
	if !ok {
		return marmoset.Failure[marmoset.Option[marmoset.Bytes], fileapi.Error](fileapi.ErrorAlreadyClosed{})
	}
	line, err := resource.readBuffer().ReadBytes('\n')
	if err != nil {
		if errors.Is(err, io.EOF) {
			if len(line) == 0 {
				return marmoset.Success[marmoset.Option[marmoset.Bytes], fileapi.Error](marmoset.None[marmoset.Bytes]())
			}
			return marmoset.Success[marmoset.Option[marmoset.Bytes], fileapi.Error](
				marmoset.Some[marmoset.Bytes](marmoset.BytesCopy(trimLineEnding(line))),
			)
		}
		return marmoset.Failure[marmoset.Option[marmoset.Bytes], fileapi.Error](fileError("", err))
	}
	return marmoset.Success[marmoset.Option[marmoset.Bytes], fileapi.Error](
		marmoset.Some[marmoset.Bytes](marmoset.BytesCopy(trimLineEnding(line))),
	)
}

func ReadChunkHandle(file fileapi.File, size int64) marmoset.Result[marmoset.Option[marmoset.Bytes], fileapi.Error] {
	resource, ok := handleFile(file)
	if !ok {
		return marmoset.Failure[marmoset.Option[marmoset.Bytes], fileapi.Error](fileapi.ErrorAlreadyClosed{})
	}
	if size <= 0 {
		return marmoset.Success[marmoset.Option[marmoset.Bytes], fileapi.Error](marmoset.None[marmoset.Bytes]())
	}
	maxInt := int64(^uint(0) >> 1)
	if size > maxInt {
		return marmoset.Failure[marmoset.Option[marmoset.Bytes], fileapi.Error](
			fileapi.ErrorOther{Field0: "chunk size exceeds host int"},
		)
	}
	buf := make([]byte, int(size))
	n, err := resource.readBuffer().Read(buf)
	if err != nil {
		if errors.Is(err, io.EOF) {
			if n == 0 {
				return marmoset.Success[marmoset.Option[marmoset.Bytes], fileapi.Error](marmoset.None[marmoset.Bytes]())
			}
			return marmoset.Success[marmoset.Option[marmoset.Bytes], fileapi.Error](
				marmoset.Some[marmoset.Bytes](marmoset.BytesCopy(buf[:n])),
			)
		}
		return marmoset.Failure[marmoset.Option[marmoset.Bytes], fileapi.Error](fileError("", err))
	}
	if n == 0 {
		return marmoset.Success[marmoset.Option[marmoset.Bytes], fileapi.Error](marmoset.None[marmoset.Bytes]())
	}
	return marmoset.Success[marmoset.Option[marmoset.Bytes], fileapi.Error](
		marmoset.Some[marmoset.Bytes](marmoset.BytesCopy(buf[:n])),
	)
}

func WriteHandle(file fileapi.File, bytes marmoset.Bytes) marmoset.Result[marmoset.Unit, fileapi.Error] {
	resource, ok := handleFile(file)
	if !ok {
		return marmoset.Failure[marmoset.Unit, fileapi.Error](fileapi.ErrorAlreadyClosed{})
	}
	data := bytes.Copy()
	written, err := resource.file.Write(data)
	if err != nil {
		return marmoset.Failure[marmoset.Unit, fileapi.Error](fileError("", err))
	}
	if written != len(data) {
		return marmoset.Failure[marmoset.Unit, fileapi.Error](fileError("", io.ErrShortWrite))
	}
	return marmoset.Success[marmoset.Unit, fileapi.Error](marmoset.NewUnit())
}

func FlushHandle(file fileapi.File) marmoset.Result[marmoset.Unit, fileapi.Error] {
	resource, ok := handleFile(file)
	if !ok {
		return marmoset.Failure[marmoset.Unit, fileapi.Error](fileapi.ErrorAlreadyClosed{})
	}
	if err := resource.file.Sync(); err != nil {
		return marmoset.Failure[marmoset.Unit, fileapi.Error](fileError("", err))
	}
	return marmoset.Success[marmoset.Unit, fileapi.Error](marmoset.NewUnit())
}
