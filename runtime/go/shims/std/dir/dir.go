package dir

import (
	"errors"
	"os"
	"path/filepath"
	"syscall"

	dirapi "marmoset_out/api/std/dir"
	"marmoset_out/marmoset"
)

func isNotExist(err error) bool {
	return errors.Is(err, os.ErrNotExist) || os.IsNotExist(err)
}

func isAlreadyExist(err error) bool {
	return errors.Is(err, os.ErrExist) || os.IsExist(err)
}

func isPermission(err error) bool {
	return errors.Is(err, os.ErrPermission) || os.IsPermission(err)
}

func dirError(err error) dirapi.Error {
	switch {
	case err == nil:
		return dirapi.ErrorOther{Field0: ""}
	case isNotExist(err):
		return dirapi.ErrorNotFound{}
	case isPermission(err):
		return dirapi.ErrorPermissionDenied{}
	case errors.Is(err, syscall.ENOTDIR):
		return dirapi.ErrorNotDirectory{}
	case errors.Is(err, syscall.ENOTEMPTY):
		return dirapi.ErrorNotEmpty{}
	case errors.Is(err, syscall.EINVAL):
		return dirapi.ErrorInvalidPath{Field0: err.Error()}
	case isAlreadyExist(err):
		return dirapi.ErrorAlreadyExists{}
	default:
		return dirapi.ErrorOther{Field0: err.Error()}
	}
}

func kindOf(info os.FileInfo) dirapi.Kind {
	mode := info.Mode()
	switch {
	case mode&os.ModeSymlink != 0:
		return dirapi.KindSymlink{}
	case info.IsDir():
		return dirapi.KindDirectory{}
	case mode.IsRegular():
		return dirapi.KindFile{}
	default:
		return dirapi.KindOther{}
	}
}

func Read(path string) marmoset.Result[[]dirapi.RawEntry, dirapi.Error] {
	entries, err := os.ReadDir(path)
	if err != nil {
		return marmoset.Failure[[]dirapi.RawEntry, dirapi.Error](dirError(err))
	}

	out := make([]dirapi.RawEntry, 0, len(entries))
	for _, entry := range entries {
		info, err := entry.Info()
		if err != nil {
			return marmoset.Failure[[]dirapi.RawEntry, dirapi.Error](dirError(err))
		}
		out = append(out, dirapi.RawEntry{
			Field0: filepath.Join(path, entry.Name()),
			Field1: kindOf(info),
		})
	}

	return marmoset.Success[[]dirapi.RawEntry, dirapi.Error](out)
}

func Make(path string) marmoset.Result[marmoset.Unit, dirapi.Error] {
	if err := os.Mkdir(path, 0o777); err != nil {
		return marmoset.Failure[marmoset.Unit, dirapi.Error](dirError(err))
	}
	return marmoset.Success[marmoset.Unit, dirapi.Error](marmoset.NewUnit())
}

func MakeAll(path string) marmoset.Result[marmoset.Unit, dirapi.Error] {
	if err := os.MkdirAll(path, 0o777); err != nil {
		return marmoset.Failure[marmoset.Unit, dirapi.Error](dirError(err))
	}
	return marmoset.Success[marmoset.Unit, dirapi.Error](marmoset.NewUnit())
}

func Remove(path string) marmoset.Result[marmoset.Unit, dirapi.Error] {
	if err := os.Remove(path); err != nil {
		return marmoset.Failure[marmoset.Unit, dirapi.Error](dirError(err))
	}
	return marmoset.Success[marmoset.Unit, dirapi.Error](marmoset.NewUnit())
}

func RemoveAll(path string) marmoset.Result[marmoset.Unit, dirapi.Error] {
	if err := os.RemoveAll(path); err != nil {
		return marmoset.Failure[marmoset.Unit, dirapi.Error](dirError(err))
	}
	return marmoset.Success[marmoset.Unit, dirapi.Error](marmoset.NewUnit())
}

func ExistsQ(path string) marmoset.Result[bool, dirapi.Error] {
	_, err := os.Stat(path)
	if err == nil {
		return marmoset.Success[bool, dirapi.Error](true)
	}
	if isNotExist(err) {
		return marmoset.Success[bool, dirapi.Error](false)
	}
	return marmoset.Failure[bool, dirapi.Error](dirError(err))
}

func Current() marmoset.Result[string, dirapi.Error] {
	cwd, err := os.Getwd()
	if err != nil {
		return marmoset.Failure[string, dirapi.Error](dirError(err))
	}
	return marmoset.Success[string, dirapi.Error](cwd)
}
