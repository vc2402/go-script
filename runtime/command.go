package runtime

type CommandClass uint8

const (
	CKALU CommandClass = iota
	CKExec
	CKScope
)

var executors = [][]CommandExecutor{
	aluExecutors,
	execExecutors,
	scopeExecutors,
}

const ClassMask uint32 = 0xff000000

type Command uint32

type CommandWithClass uint32

type Operation struct {
	class   CommandClass
	command Command
	ip1     int
	ip2     int
	sp      string
	fp      float64
	debug   any
}

type Program []Operation

func (cc CommandWithClass) split() (CommandClass, Command) {
	return CommandClass((uint32(cc) & ClassMask) >> 24), Command(uint32(cc) & (^ClassMask))
}

type CommandParams any

type CommandExecutor func(eng *Engine, op *Operation)

func (p *Program) Add(class CommandClass, com Command, params ...any) *Program {
	op := Operation{class: class, command: com}
	if len(params) > 0 {
		op.ip1 = params[0].(int)
	}
	if len(params) > 1 {
		op.ip2 = params[1].(int)
	}
	if len(params) > 2 {
		op.sp = params[2].(string)
	}
	if len(params) > 3 {
		op.fp = params[3].(float64)
	}
	if len(*p) > 0 {
		op.debug = (*p)[len(*p)-1].debug
	}
	*p = append(*p, op)
	return p
}

func (p *Program) SetDebugInfo(di any) *Program {
	if len(*p) > 0 {
		(*p)[len(*p)-1].debug = di
	}
	return p
}
