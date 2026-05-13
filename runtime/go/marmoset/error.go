package marmoset

import "strconv"

type ErrorFrame struct {
	File     string
	Function string
	Position int64
}

type ErrorContext struct {
	Message string
	Frames  []ErrorFrame
}

func NewErrorContext(message string, file string, function string, position int64) ErrorContext {
	return ErrorContext{
		Message: message,
		Frames: []ErrorFrame{
			{File: file, Function: function, Position: position},
		},
	}
}

func ErrorMessage(ctx ErrorContext) string {
	return ctx.Message
}

func ErrorFrames(ctx ErrorContext) []ErrorFrame {
	out := make([]ErrorFrame, len(ctx.Frames))
	copy(out, ctx.Frames)
	return out
}

func ErrorFrameString(frame ErrorFrame) string {
	location := frame.File
	if frame.Position > 0 {
		location = location + ":" + strconv.FormatInt(frame.Position, 10)
	}
	if frame.Function == "" {
		return location
	}
	return location + " in " + frame.Function
}

func ErrorFrameStrings(ctx ErrorContext) []string {
	frames := ErrorFrames(ctx)
	out := make([]string, len(frames))
	for i, frame := range frames {
		out[i] = ErrorFrameString(frame)
	}
	return out
}
