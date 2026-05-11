package file

import (
	"errors"
	"io"
	"os"
	"syscall"

	fileapi "marmoset_out/api/std/file"
	"marmoset_out/marmoset"
)

type fileResource struct {
	file *os.File
}

var files = marmoset.NewHandleTable[fileapi.FileTag, *fileResource]()

func isDirectoryPath(path string) bool {
	info, err := os.Stat(path)
	return err == nil && info.IsDir()
}

func isNotExist(err error) bool {
	return errors.Is(err, os.ErrNotExist) || os.IsNotExist(err)
}

func isPermission(err error) bool {
	return errors.Is(err, os.ErrPermission) || os.IsPermission(err)
}

func isDirectoryError(path string, err error) bool {
	return isDirectoryPath(path) || errors.Is(err, syscall.EISDIR)
}

func readError(path string, err error) fileapi.FileReadError {
	switch {
	case isNotExist(err):
		return fileapi.FileReadErrorNotFound{}
	case isPermission(err):
		return fileapi.FileReadErrorPermissionDenied{}
	case isDirectoryError(path, err):
		return fileapi.FileReadErrorIsDirectory{}
	default:
		return fileapi.FileReadErrorOther{Field0: err.Error()}
	}
}

func writeError(path string, err error) fileapi.FileWriteError {
	switch {
	case isNotExist(err):
		return fileapi.FileWriteErrorNotFound{}
	case isPermission(err):
		return fileapi.FileWriteErrorPermissionDenied{}
	case isDirectoryError(path, err):
		return fileapi.FileWriteErrorIsDirectory{}
	default:
		return fileapi.FileWriteErrorOther{Field0: err.Error()}
	}
}

func openError(path string, err error) fileapi.FileOpenError {
	switch {
	case isNotExist(err):
		return fileapi.FileOpenErrorNotFound{}
	case isPermission(err):
		return fileapi.FileOpenErrorPermissionDenied{}
	case isDirectoryError(path, err):
		return fileapi.FileOpenErrorIsDirectory{}
	default:
		return fileapi.FileOpenErrorOther{Field0: err.Error()}
	}
}

func closeError(err error) fileapi.FileCloseError {
	if err == nil {
		return fileapi.FileCloseErrorAlreadyClosed{}
	}
	return fileapi.FileCloseErrorOther{Field0: err.Error()}
}

func ReadBytes(path string) marmoset.Result[marmoset.Bytes, fileapi.FileReadError] {
	data, err := os.ReadFile(path)
	if err != nil {
		return marmoset.Failure[marmoset.Bytes, fileapi.FileReadError](readError(path, err))
	}
	return marmoset.Success[marmoset.Bytes, fileapi.FileReadError](marmoset.BytesCopy(data))
}

func WriteBytes(path string, bytes marmoset.Bytes) marmoset.Result[marmoset.Unit, fileapi.FileWriteError] {
	err := os.WriteFile(path, bytes.Copy(), 0o666)
	if err != nil {
		return marmoset.Failure[marmoset.Unit, fileapi.FileWriteError](writeError(path, err))
	}
	return marmoset.Success[marmoset.Unit, fileapi.FileWriteError](marmoset.NewUnit())
}

func FlushPath(path string) marmoset.Result[marmoset.Unit, fileapi.FileWriteError] {
	return marmoset.Success[marmoset.Unit, fileapi.FileWriteError](marmoset.NewUnit())
}

func ReadAll(file fileapi.File) marmoset.Result[marmoset.Bytes, fileapi.FileReadError] {
	resource, ok := files.Get(file)
	if !ok {
		return marmoset.Failure[marmoset.Bytes, fileapi.FileReadError](fileapi.FileReadErrorAlreadyClosed{})
	}
	data, err := io.ReadAll(resource.file)
	if err != nil {
		return marmoset.Failure[marmoset.Bytes, fileapi.FileReadError](fileapi.FileReadErrorOther{Field0: err.Error()})
	}
	return marmoset.Success[marmoset.Bytes, fileapi.FileReadError](marmoset.BytesCopy(data))
}

func OpenHandle(path string) marmoset.Result[fileapi.File, fileapi.FileOpenError] {
	if isDirectoryPath(path) {
		return marmoset.Failure[fileapi.File, fileapi.FileOpenError](fileapi.FileOpenErrorIsDirectory{})
	}
	file, err := os.Open(path)
	if err != nil {
		return marmoset.Failure[fileapi.File, fileapi.FileOpenError](openError(path, err))
	}
	return marmoset.Success[fileapi.File, fileapi.FileOpenError](files.Insert(&fileResource{file: file}))
}

func CloseHandle(file fileapi.File) marmoset.Result[marmoset.Unit, fileapi.FileCloseError] {
	resource, ok := files.Get(file)
	if !ok {
		return marmoset.Failure[marmoset.Unit, fileapi.FileCloseError](fileapi.FileCloseErrorAlreadyClosed{})
	}
	if !files.Delete(file) {
		return marmoset.Failure[marmoset.Unit, fileapi.FileCloseError](fileapi.FileCloseErrorAlreadyClosed{})
	}
	if err := resource.file.Close(); err != nil {
		return marmoset.Failure[marmoset.Unit, fileapi.FileCloseError](closeError(err))
	}
	return marmoset.Success[marmoset.Unit, fileapi.FileCloseError](marmoset.NewUnit())
}
