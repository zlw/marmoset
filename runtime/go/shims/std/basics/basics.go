package basics

import (
	"fmt"

	"marmoset_out/marmoset"
)

func PutsStr(value string) marmoset.Unit {
	fmt.Println(value)
	return marmoset.NewUnit()
}
