package compiler

import "github.com/vc2402/goscript/runtime"

var builtInTypesMapping = [...]runtime.VarKind{
	runtime.VKInt,
	runtime.VKFloat,
	runtime.VKBool,
	runtime.VKChar,
	runtime.VKString,
	runtime.VKAny,
	runtime.VKError,
}

var builtInTypes = [...]string{
	"int",
	"float",
	"bool",
	"char",
	"string",
	"any",
	"error",
}
