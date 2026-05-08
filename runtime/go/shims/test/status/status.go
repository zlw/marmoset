package status

import statusapi "marmoset_out/api/test/status"

func Label(status statusapi.Status) string {
	switch value := status.(type) {
	case statusapi.StatusReady:
		return "ready"
	case statusapi.StatusMessage:
		return "message:" + value.Field0
	default:
		panic("unknown status")
	}
}

func Flip(flag bool) statusapi.Status {
	if flag {
		return statusapi.StatusMessage{Field0: "flipped"}
	}
	return statusapi.StatusReady{}
}

func NilStatus() statusapi.Status {
	return nil
}
