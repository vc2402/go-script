package runtime

const (
	SCGetVar Command = iota // load var to stack (ip1&ip2 - scopeIdx & idx)
	SCLoadConst
	SCAssign // assign value at stack top to var (ip1&ip2 - scopeIdx & idx)
	SCCreateScope
	SCCreateLoopScope
	SCPopN
	SCIndex
	SCGetMap
	SCSetMap
	SCDot
)

var scopeExecutors = []CommandExecutor{
	scopeGetVar,
	scopeLoadConst,
	scopeAssign,
	scopeCreateScope,
	scopeCreateLoopScope,
	scopePopN,
	scopeIndex,
	scopeGetMap,
	scopeSetMap,
	scopeDot,
}

func scopeGetVar(eng *Engine, op *Operation) {
	eng.push(eng.scopeAt(op.ip1).getVar(op.ip2).deref(eng))
}

// only simple types may be loaded
// ip1 - ValueKind, ip2, sp, fp - value; if kind == VKUndef - value is nil
func scopeLoadConst(eng *Engine, op *Operation) {
	constant := &Value{VarDescriptor: &VarDescriptor{kind: VarKind(op.ip1)}}
	switch constant.kind {
	case VKInt:
		constant.val = Int(op.ip2)
	case VKFloat:
		constant.val = Float(op.fp)
	case VKString:
		constant.val = op.sp
	case VKBool:
		constant.val = op.ip1 != 0
	}
	eng.push(constant)
}

func scopeAssign(eng *Engine, op *Operation) {
	//eng.assign(eng.scopeAt(op.ip1).getVar(op.ip2).deref(eng), eng.pop())
	eng.assign(eng.pop(), eng.pop())
}

func scopeCreateScope(eng *Engine, op *Operation) {
	scope := eng.Scope()
	scope.createScope(eng, op.ip1)
}

func scopeCreateLoopScope(eng *Engine, op *Operation) {
	scope := eng.Scope()
	scope.createScope(eng, op.ip1)
}

func scopePopN(eng *Engine, op *Operation) {
	eng.popN(op.ip1)
}

func scopeIndex(eng *Engine, op *Operation) {
	lv := eng.stackItem(1)
	idx := eng.pop()
	eng.setStackItem(0, eng.index(lv, idx))
}

func scopeSetMap(eng *Engine, op *Operation) {
	val := eng.pop()
	idx := eng.pop()
	lv := eng.pop()
	eng.setMapValue(lv, idx, val)
}

func scopeGetMap(eng *Engine, op *Operation) {
	lv := eng.stackItem(1)
	idx := eng.pop()
	eng.setStackItem(0, eng.mapValue(lv, idx))
}

func scopeDot(eng *Engine, op *Operation) {
	lv := eng.stackItem(1)
	fld := eng.pop()
	eng.setStackItem(0, eng.dot(lv, fld))
}
