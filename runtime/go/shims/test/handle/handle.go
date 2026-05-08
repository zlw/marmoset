package handle

import (
	handleapi "marmoset_out/api/test/handle"
	"marmoset_out/marmoset"
)

type fileResource struct {
	name string
}

var files = marmoset.NewHandleTable[handleapi.FileTag, *fileResource]()

func Open(name string) handleapi.File {
	return files.Insert(&fileResource{name: name})
}

func Label(file handleapi.File) string {
	resource, ok := files.Get(file)
	if !ok {
		panic("stale handle")
	}
	return "file:" + resource.name
}

func Close(file handleapi.File) string {
	resource, ok := files.Get(file)
	if !ok {
		panic("stale handle")
	}
	files.Delete(file)
	return "closed:" + resource.name
}

func Invalid() handleapi.File {
	var zero handleapi.File
	return zero
}
