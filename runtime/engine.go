package runtime

import (
	"errors"
	"fmt"
	"reflect"
)

type Engine struct {
	// we always have at least one scope - top
	scope ExecutionScope
	stack []*Value
	*Registry
	options EngineOptions
}

func (eng *Engine) Run(function string, params []*Value) (ret []*Value, err error) {
	defer func() {
		if r := recover(); r != nil {
			fmt.Println("Recovered: \n", r)
			err = fmt.Errorf("panic: %v", r)
		}
	}()
	if len(eng.scope) != 1 {
		return nil, errors.New("invalid state")
	}
	for i := range eng.scope[0].vars {
		if eng.scope[0].vars[i].name == function {
			if eng.scope[0].vars[i].kind != VKFunc {
				return nil, errors.New("not a function")
			}
			for _, param := range params {
				eng.push(param)
			}
			eng.push(&Value{
				VarDescriptor: &VarDescriptor{kind: VKInt},
				val:           Int(len(params)),
			})
			eng.scope[0].callFunction(eng, &eng.scope[0].vars[i])
			ret = eng.exec()
			//for len(eng.stack) > 0 {
			//	ret = append(ret, eng.pop())
			//}
			return
		}
	}
	return nil, errors.New("function not found")
}

func (eng *Engine) DescribeFunction(function string) (params []FuncParam, rets []FuncParam, err error) {
	if len(eng.scope) != 1 {
		return nil, nil, errors.New("invalid state")
	}
	for i := range eng.scope[0].vars {
		if eng.scope[0].vars[i].name == function {
			if eng.scope[0].vars[i].kind != VKFunc {
				return nil, nil, errors.New("not a function")
			}
			fd := eng.scope[0].vars[i].GetFunction().(*ScopeDescriptor).ScopeSpecific.(FuncScopeDescriptor).Func
			return fd.params, fd.results, nil
		}
	}
	return nil, nil, errors.New("function not found")
}

func (eng *Engine) Var(name string) (ret Value, err error) {
	if len(eng.scope) != 1 {
		err = errors.New("invalid state")
		return
	}
	for _, v := range eng.scope[0].vars {
		if v.VarDescriptor.name == name {
			return v, nil
		}
	}
	err = errors.New("var not found")
	return
}

func (eng *Engine) exec() (ret []*Value) {
	for eng.next() {
	}
	ret = eng.stack
	eng.stack = nil
	return
}

func (eng *Engine) callMethod(name string) {
	object := eng.pop().deref(eng)
	if object.kind != VKExternal {
		panic("method for not reflect object")
	}
	ro := object.Reflect()
	m, ok := ro.Type().MethodByName(name)
	if !ok {
		panic(fmt.Sprintf("method '%s' not found for type %s", name, ro.Type().Name()))
	}
	argsCount := int(eng.pop().getInt())
	args := make([]reflect.Value, argsCount+1)
	for i := argsCount - 1; i >= 0; i-- {
		args[i+1] = eng.pop().toReflect()
	}
	args[0] = ro
	args = m.Func.Call(args)
	for _, arg := range args {
		eng.push(&Value{VarDescriptor: &VarDescriptor{kind: VKExternal}, val: arg})
	}
}

func (eng *Engine) next() bool {
	scope := eng.Scope()
	if len(scope.Program) <= scope.pc {
		scope.leave(eng)
		return len(eng.scope) > 1 ||
			eng.scope[0].pc < len(eng.scope[0].Program)
	}
	op := &scope.Program[scope.pc]
	exec := executors[op.class][op.command]
	exec(eng, op)
	scope.pc++
	return true
}

func (eng *Engine) push(v *Value) {
	//TODO check stack overflow
	eng.stack = append(eng.stack, v)
}

func (eng *Engine) pop() (v *Value) {
	sl := len(eng.stack)
	if sl == 0 {
		//TODO add debug information
		panic("stack underflow")
	}
	v = eng.stack[sl-1]
	eng.stack = eng.stack[:sl-1]
	return
}

func (eng *Engine) popN(n int) {
	sl := len(eng.stack)
	if sl < n {
		//TODO add debug information
		panic("stack underflow")
	}
	eng.stack = eng.stack[:sl-n]
	return
}

func (eng *Engine) stackItem(idx int) (v *Value) {
	sl := len(eng.stack)
	if idx >= sl {
		//TODO add debug information
		panic("stack underflow")
	}
	return eng.stack[sl-idx-1]
}

func (eng *Engine) setStackItem(idx int, v *Value) {
	sl := len(eng.stack)
	if idx >= sl {
		//TODO add debug information
		panic("stack underflow")
	}
	eng.stack[sl-idx-1] = v
}

func (eng *Engine) pushScope(scope *Scope) {
	//TODO check scope stack overflow
	eng.scope = append(eng.scope, scope)
}

func (eng *Engine) popScope() (scope *Scope) {
	sl := len(eng.scope)
	if sl == 0 {
		panic("scope stack underflow")
	}
	scope = eng.scope[sl-1]
	eng.scope = eng.scope[:sl-1]
	return
}

func (eng *Engine) Scope() (scope *Scope) {
	if len(eng.scope) == 0 {
		panic("empty execution scope")
	}
	scope = eng.scope[len(eng.scope)-1]
	return
}

func (eng *Engine) scopeAt(idx int) (scope *Scope) {
	if len(eng.scope) <= idx {
		panic("scopeAt: idx too big for execution scope")
	}
	scope = eng.scope[len(eng.scope)-idx-1]
	return
}
