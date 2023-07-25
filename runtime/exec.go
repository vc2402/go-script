package runtime

const (
	ExeccCall Command = iota // calls function identified by var at stack top or ip1&ip2 if ip1 is not null; params should be pushed into stack
	ExecCallMethod
	ExeccCont
	ExeccBreak
	ExeccReturn
	ExeccSkipIf
	ExeccSkipIfNot
	ExeccGoTo
	ExeccIfThen
	ExeccIfThenElse
	ExecEnterScope
)

var execExecutors = []CommandExecutor{
	execCall,
	execCallMethod,
	execCont,
	execBreak,
	execReturn,
	execSkipIf,
	execSkipIfNot,
	execGoTo,
	execIfThen,
	execIfThenElse,
	execEnterScope,
}

func execCall(eng *Engine, op *Operation) {
	var fn *Value
	var s *Scope
	if op.ip1 != 0 {
		s = eng.scopeAt(op.ip1)
		fn = s.getVar(op.ip2)
	} else {
		fn = eng.pop()
		s = fn.getEffectiveScope()
		if s == nil {
			s = eng.Scope()
		}
	}
	fun := fn.deref(eng)
	s.callFunction(eng, fun)
}

func execCallMethod(eng *Engine, op *Operation) {
	eng.callMethod(op.sp)
}

func execCont(eng *Engine, op *Operation) {
	eng.Scope().cont(eng)
}

func execBreak(eng *Engine, op *Operation) {
	eng.Scope().brek(eng)
}

func execReturn(eng *Engine, op *Operation) {
	eng.Scope().ret(eng)
}

func execSkipIf(eng *Engine, op *Operation) {
	v := eng.pop()
	if v.getBool() {
		eng.Scope().pc++
	}
}

func execSkipIfNot(eng *Engine, op *Operation) {
	v := eng.pop()
	if !v.getBool() {
		eng.Scope().pc++
	}
}

func execGoTo(eng *Engine, op *Operation) {
	eng.Scope().pc += op.ip1
}

func execIfThen(eng *Engine, op *Operation) {
	v := eng.pop()
	if v.getBool() {
		scope := eng.Scope()
		scope.createScope(eng, op.ip1)
	}
}

func execIfThenElse(eng *Engine, op *Operation) {
	v := eng.pop()
	var scopeIdx int
	if v.getBool() {
		scopeIdx = op.ip1
	} else {
		scopeIdx = op.ip2
	}
	scope := eng.Scope()
	scope.createScope(eng, scopeIdx)
}

func execEnterScope(eng *Engine, op *Operation) {
	scope := eng.Scope()
	scope.createScope(eng, op.ip1)
}
