package runtime

const (
	AlucAdd Command = iota
	AlucSub
	AlucMul
	AlucDiv
	AlucInc
	AlucDec
	AlucAnd
	AlucOr
	AlucNot
	AlucEqualTo
	AlucNotEqualTo
	AlucGreaterThan
	AlucLessThan
	AlucGreaterOrEqual
	AlucLessOrEqual
	AlucConvert
)

var aluExecutors = []CommandExecutor{
	aluAdd,
	aluSub,
	aluMul,
	aluDiv,
	aluInc,
	aluDec,
	aluAnd,
	aluOr,
	aluNot,
	aluEqualTo,
	aluNotEqualTo,
	aluGreaterThan,
	aluLessThan,
	aluGreaterThanOrEqualTo,
	aluLessThanOrEqualTo,
	aluConvert,
}

func aluAdd(eng *Engine, op *Operation) {
	left := eng.pop()
	right := eng.stackItem(0)
	eng.setStackItem(0, eng.sum(left, right))
}

func aluSub(eng *Engine, op *Operation) {
	right := eng.pop()
	left := eng.stackItem(0)
	eng.setStackItem(0, eng.sub(left, right))
}

func aluMul(eng *Engine, op *Operation) {
	right := eng.pop()
	left := eng.stackItem(0)
	eng.setStackItem(0, eng.mul(left, right))
}

func aluDiv(eng *Engine, op *Operation) {
	right := eng.pop()
	left := eng.stackItem(0)
	eng.setStackItem(0, eng.div(left, right))
}

func aluInc(eng *Engine, op *Operation) {
	operand := eng.stackItem(0)
	eng.setStackItem(0, eng.inc(operand))
}

func aluDec(eng *Engine, op *Operation) {
	operand := eng.stackItem(0)
	eng.setStackItem(0, eng.dec(operand))
}

func aluAnd(eng *Engine, op *Operation) {
	right := eng.pop()
	left := eng.stackItem(0)
	eng.setStackItem(0, eng.and(left, right))
}

func aluOr(eng *Engine, op *Operation) {
	right := eng.pop()
	left := eng.stackItem(0)
	eng.setStackItem(0, eng.or(left, right))
}

func aluNot(eng *Engine, op *Operation) {
	operand := eng.stackItem(0)
	eng.setStackItem(0, eng.not(operand))
}

func aluEqualTo(eng *Engine, op *Operation) {
	right := eng.pop()
	left := eng.stackItem(0)
	eng.setStackItem(0, eng.eq(left, right))
}

func aluNotEqualTo(eng *Engine, op *Operation) {
	right := eng.pop()
	left := eng.stackItem(0)
	eng.setStackItem(0, eng.ne(left, right))
}

func aluGreaterThan(eng *Engine, op *Operation) {
	right := eng.pop()
	left := eng.stackItem(0)
	eng.setStackItem(0, eng.gt(left, right))
}

func aluLessThan(eng *Engine, op *Operation) {
	right := eng.pop()
	left := eng.stackItem(0)
	eng.setStackItem(0, eng.lt(left, right))
}

func aluGreaterThanOrEqualTo(eng *Engine, op *Operation) {
	right := eng.pop()
	left := eng.stackItem(0)
	eng.setStackItem(0, eng.gte(left, right))
}

func aluLessThanOrEqualTo(eng *Engine, op *Operation) {
	right := eng.pop()
	left := eng.stackItem(0)
	eng.setStackItem(0, eng.lte(left, right))
}

// converts to simple type in ip1
func aluConvert(eng *Engine, op *Operation) {
	eng.setStackItem(op.ip2, eng.convert(eng.stackItem(op.ip2), VarKind(op.ip1)))
}
