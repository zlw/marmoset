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

func isNotDirectory(err error) bool {
	return errors.Is(err, syscall.ENOTDIR)
}

func isNotEmpty(err error) bool {
	return errors.Is(err, syscall.ENOTEMPTY)
}

func isInvalidPath(err error) bool {
	return errors.Is(err, syscall.EINVAL)
}

func lsError(err error) dirapi.LsError {
	switch {
	case isNotExist(err):
		return dirapi.LsErrorNotFound{}
	case isPermission(err):
		return dirapi.LsErrorPermissionDenied{}
	case isNotDirectory(err):
		return dirapi.LsErrorNotDirectory{}
	case isInvalidPath(err):
		return dirapi.LsErrorInvalidPath{Field0: err.Error()}
	default:
		return dirapi.LsErrorOther{Field0: err.Error()}
	}
}

func mkdirError(err error) dirapi.MkdirError {
	switch {
	case isNotExist(err):
		return dirapi.MkdirErrorNotFound{}
	case isPermission(err):
		return dirapi.MkdirErrorPermissionDenied{}
	case isAlreadyExist(err):
		return dirapi.MkdirErrorAlreadyExists{}
	case isNotDirectory(err):
		return dirapi.MkdirErrorNotDirectory{}
	case isInvalidPath(err):
		return dirapi.MkdirErrorInvalidPath{Field0: err.Error()}
	default:
		return dirapi.MkdirErrorOther{Field0: err.Error()}
	}
}

func rmdirError(err error) dirapi.RmdirError {
	switch {
	case isNotExist(err):
		return dirapi.RmdirErrorNotFound{}
	case isPermission(err):
		return dirapi.RmdirErrorPermissionDenied{}
	case isNotDirectory(err):
		return dirapi.RmdirErrorNotDirectory{}
	case isNotEmpty(err):
		return dirapi.RmdirErrorNotEmpty{}
	case isInvalidPath(err):
		return dirapi.RmdirErrorInvalidPath{Field0: err.Error()}
	default:
		return dirapi.RmdirErrorOther{Field0: err.Error()}
	}
}

func existsError(err error) dirapi.ExistsError {
	switch {
	case isPermission(err):
		return dirapi.ExistsErrorPermissionDenied{}
	case isInvalidPath(err):
		return dirapi.ExistsErrorInvalidPath{Field0: err.Error()}
	default:
		return dirapi.ExistsErrorOther{Field0: err.Error()}
	}
}

func pwdError(err error) dirapi.PwdError {
	switch {
	case isNotExist(err):
		return dirapi.PwdErrorNotFound{}
	case isPermission(err):
		return dirapi.PwdErrorPermissionDenied{}
	default:
		return dirapi.PwdErrorOther{Field0: err.Error()}
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

func Ls(path string) marmoset.Result[[]dirapi.Entry, dirapi.LsError] {
	entries, err := os.ReadDir(path)
	if err != nil {
		return marmoset.Failure[[]dirapi.Entry, dirapi.LsError](lsError(err))
	}

	out := make([]dirapi.Entry, 0, len(entries))
	for _, entry := range entries {
		info, err := entry.Info()
		if err != nil {
			return marmoset.Failure[[]dirapi.Entry, dirapi.LsError](lsError(err))
		}
		out = append(out, dirapi.Entry{
			Field0: filepath.Join(path, entry.Name()),
			Field1: kindOf(info),
		})
	}

	return marmoset.Success[[]dirapi.Entry, dirapi.LsError](out)
}

func Mkdir(path string) marmoset.Result[marmoset.Unit, dirapi.MkdirError] {
	if err := os.Mkdir(path, 0o777); err != nil {
		return marmoset.Failure[marmoset.Unit, dirapi.MkdirError](mkdirError(err))
	}
	return marmoset.Success[marmoset.Unit, dirapi.MkdirError](marmoset.NewUnit())
}

func MkdirTree(path string) marmoset.Result[marmoset.Unit, dirapi.MkdirError] {
	if err := os.MkdirAll(path, 0o777); err != nil {
		return marmoset.Failure[marmoset.Unit, dirapi.MkdirError](mkdirError(err))
	}
	return marmoset.Success[marmoset.Unit, dirapi.MkdirError](marmoset.NewUnit())
}

func Rmdir(path string) marmoset.Result[marmoset.Unit, dirapi.RmdirError] {
	info, err := os.Lstat(path)
	if err != nil {
		return marmoset.Failure[marmoset.Unit, dirapi.RmdirError](rmdirError(err))
	}
	if !info.IsDir() {
		return marmoset.Failure[marmoset.Unit, dirapi.RmdirError](dirapi.RmdirErrorNotDirectory{})
	}
	if err := os.Remove(path); err != nil {
		return marmoset.Failure[marmoset.Unit, dirapi.RmdirError](rmdirError(err))
	}
	return marmoset.Success[marmoset.Unit, dirapi.RmdirError](marmoset.NewUnit())
}

func RmdirTree(path string) marmoset.Result[marmoset.Unit, dirapi.RmdirError] {
	info, err := os.Lstat(path)
	if err != nil {
		return marmoset.Failure[marmoset.Unit, dirapi.RmdirError](rmdirError(err))
	}
	if !info.IsDir() {
		return marmoset.Failure[marmoset.Unit, dirapi.RmdirError](dirapi.RmdirErrorNotDirectory{})
	}
	if err := os.RemoveAll(path); err != nil {
		return marmoset.Failure[marmoset.Unit, dirapi.RmdirError](rmdirError(err))
	}
	return marmoset.Success[marmoset.Unit, dirapi.RmdirError](marmoset.NewUnit())
}

func ExistsQ(path string) marmoset.Result[bool, dirapi.ExistsError] {
	info, err := os.Stat(path)
	if err == nil {
		return marmoset.Success[bool, dirapi.ExistsError](info.IsDir())
	}
	if isNotExist(err) || isNotDirectory(err) {
		return marmoset.Success[bool, dirapi.ExistsError](false)
	}
	return marmoset.Failure[bool, dirapi.ExistsError](existsError(err))
}

func Pwd() marmoset.Result[string, dirapi.PwdError] {
	cwd, err := os.Getwd()
	if err != nil {
		return marmoset.Failure[string, dirapi.PwdError](pwdError(err))
	}
	return marmoset.Success[string, dirapi.PwdError](cwd)
}
