package runtime

// Func is a function that can be called
//type Func struct {
//	scope      *Scope
//	descriptor *FuncDescriptor
//}

type FuncParam struct {
	Name       string
	Descriptor *VarDescriptor
}

// FuncDescriptor describes a function
type FuncDescriptor struct {
	name    string
	params  []FuncParam
	results []FuncParam
}

func NewFuncDescriptor(name string) *FuncDescriptor {
	return &FuncDescriptor{name: name}
}

func (fd *FuncDescriptor) AddParam(param ...FuncParam) *FuncDescriptor {
	fd.params = append(fd.params, param...)
	return fd
}

func (fd *FuncDescriptor) AddResult(res ...FuncParam) *FuncDescriptor {
	fd.results = append(fd.results, res...)
	return fd
}

func (fd *FuncDescriptor) Params() []FuncParam {
	return fd.params
}

func (fd *FuncDescriptor) Result() []FuncParam {
	return fd.results
}
