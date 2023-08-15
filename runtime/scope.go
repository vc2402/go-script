package runtime

import (
	"errors"
	"fmt"
	"reflect"
	"sync"
)

var ErrTypeNotFound = errors.New("type not found")

type ScopeKind int

const (
	SKFunction ScopeKind = iota
	SKLoop
	SKBlock
	SKTop
)

type ScopeDescriptor struct {
	Kind          ScopeKind
	Program       Program
	Locals        []VarDescriptor
	Scopes        []ScopeDescriptor
	ScopeSpecific any
}

type Scope struct {
	*ScopeDescriptor
	vars          []Value
	pc            int
	scopeSpecific any
}

type LoopScopeDescriptor struct {
	Cond int // idx of condition in program
	//after int // idx of action that should be executed after loop block
	//loop int //idx of loop's scopeqs
}

type FuncScopeDescriptor struct {
	Func *FuncDescriptor
}

type funcScope struct {
	caller ExecutionScope
}

type Package struct {
	//functions and vars should be stored also in vars
	//functions []*FuncDescriptor
	//consts    []*Value
	//vars  []*Value
	Scope
	path      string
	alias     string
	types     map[string]reflect.Type
	varsIndex map[string]int
	mapLock   sync.RWMutex
}

type ExecutionScope []*Scope

func (s *Scope) leave(eng *Engine) {
	switch s.Kind {
	case SKBlock:
		eng.popScope()
	case SKFunction:
		//ret values should be already in stack
		fs := s.scopeSpecific.(*funcScope)
		if fs.caller != nil {
			eng.scope = fs.caller
		} else {
			eng.popScope()
		}
	case SKLoop:
		ls := s.scopeSpecific.(*LoopScopeDescriptor)
		//program is init+Cond+[lop]+after; we are at last command in after
		s.pc = ls.Cond
	case SKTop:
	default:
		panic(fmt.Sprintf("leave for scope %d", s.Kind))
	}
}

func (s *Scope) ret(eng *Engine) {
	for s != nil && s.Kind != SKFunction {
		s = eng.popScope()
	}
	if s == nil {
		panic("ret outside function")
	}
	s.leave(eng)
}

func (s *Scope) cont(eng *Engine) {
	for s != nil && s.Kind != SKLoop {
		s = eng.popScope()
	}
	if s == nil {
		panic("cont outside loop")
	}
	//actually pc should be set correctly
	//ls := s.ScopeSpecific.(*loopScope)
	//program is init+Cond+[lop]+after
	//s.pc = len(s.program) - len(ls.after)
}

func (s *Scope) brek(eng *Engine) {
	for s != nil && s.Kind != SKLoop {
		s = eng.popScope()
	}
	if s == nil {
		panic("brek outside loop")
	}
	eng.popScope()
}

func (s *Scope) getVar(idx int) (v *Value) {
	if idx >= len(s.vars) {
		panic(fmt.Sprintf("var '%d' requested with %d vars", idx, len(s.vars)))
	}
	return &s.vars[idx]
}

//createScope initializes new scope with descriptor with idx in current scope
func (s *Scope) createScope(eng *Engine, idx int) {
	if len(s.Scopes) <= idx {
		panic("scope idx is out of range")
	}
	sd := &s.Scopes[idx]
	switch sd.Kind {
	case SKBlock:
		eng.pushScope(createNewScope(eng, sd))
	case SKLoop:
		eng.pushScope(s.createNewLoopScope(eng, sd))
	}

}

//createNewScope can be used for creating block or top scope
func createNewScope(eng *Engine, sd *ScopeDescriptor) *Scope {
	newScope := &Scope{
		ScopeDescriptor: sd,
	}
	newScope.init(eng)
	return newScope
}

func (s *Scope) createNewLoopScope(eng *Engine, sd *ScopeDescriptor) *Scope {
	newScope := &Scope{
		ScopeDescriptor: sd,
	}
	lsd := sd.ScopeSpecific.(*LoopScopeDescriptor)
	newScope.scopeSpecific = lsd
	newScope.init(eng)
	return newScope
}

func (s *Scope) callFunction(eng *Engine, funcVar *Value) {
	fun := funcVar.deref(eng).GetFunction()
	switch v := fun.(type) {
	case *ScopeDescriptor:
		fs := &Scope{
			ScopeDescriptor: v,
			vars:            nil,
			pc:              0,
			scopeSpecific:   &funcScope{},
		}
		if s.Kind == SKTop {
			fs.scopeSpecific = &funcScope{eng.scope}
			eng.scope = ExecutionScope{s, fs}
		} else {
			eng.pushScope(fs)
		}
		fs.init(eng)
		fd := v.ScopeSpecific.(FuncScopeDescriptor).Func
		// when we have variadic functions here will be actual args count
		eng.pop()
		// load call params
		for i := len(fd.params) - 1; i >= 0; i-- {
			// may be better call assign...
			fs.vars[i] = *eng.pop()
		}
	case reflect.Value:
		//argsCount := v.Type().NumIn()
		argsCount := int(eng.pop().getInt())
		args := make([]reflect.Value, argsCount)
		for i := argsCount - 1; i >= 0; i-- {
			args[i] = eng.pop().toReflect()
		}
		args = v.Call(args)
		for _, arg := range args {
			eng.push(&Value{VarDescriptor: &VarDescriptor{kind: VKExternal}, val: arg})
		}
	default:
		panic("undefined function type")
	}
}

func (s *Scope) init(eng *Engine) {
	if len(s.Locals) > 0 {
		s.vars = make([]Value, len(s.Locals))
		for i, local := range s.Locals {
			s.vars[i] = eng.newValue(local)
		}
	}
}

func (p *Package) GetReflectType(name string) (reflect.Type, error) {
	p.mapLock.RLock()
	defer p.mapLock.RUnlock()
	if t, ok := p.types[name]; ok {
		return t, nil
	}
	return reflect.TypeOf(ErrTypeNotFound), ErrTypeNotFound
}

func (p *Package) FindVarByName(name string) (v *Value, ok bool) {
	if p.varsIndex != nil {
		p.mapLock.RLock()
		defer p.mapLock.RUnlock()
		idx, ok := p.varsIndex[name]
		if ok {
			return p.getVar(idx), true
		}
	} else {
		for i := range p.vars {
			if p.vars[i].name == name {
				return &p.vars[i], true
			}
		}
	}
	return nil, false
}

func (p *Package) GetVarByName(name string) (v *Value) {
	if v, ok := p.FindVarByName(name); ok {
		return v
	}
	panic(fmt.Sprintf("var %s not found in package %s", name, p.alias))
}

func (p *Package) RegisterReflectType(tip reflect.Type) error {
	p.mapLock.Lock()
	defer p.mapLock.Unlock()
	p.types[tip.Name()] = tip
	return nil
}

func (p *Package) RegisterReflectValue(name string, val reflect.Value) error {
	p.mapLock.Lock()
	defer p.mapLock.Unlock()
	if p.varsIndex == nil {
		p.varsIndex = map[string]int{}
	}
	// TODO: theoretically it is possible concurrent request to Scope.vars...
	p.vars = append(p.vars, Value{&VarDescriptor{name: name, kind: VKExternal}, val})
	p.varsIndex[name] = len(p.vars) - 1
	return nil
}

func (s *ScopeDescriptor) AddScope(sd ScopeDescriptor) {
	s.Scopes = append(s.Scopes, sd)
}

func (s *ScopeDescriptor) AddVar(vd VarDescriptor) int {
	idx := len(s.Locals)
	s.Locals = append(s.Locals, vd)
	return idx
}

func (s *ScopeDescriptor) AddVarIdx(vd VarDescriptor, idx int) int {
	for i := len(s.Locals); i <= idx; i++ {
		s.Locals = append(s.Locals, vd)
	}
	s.Locals[idx] = vd
	return idx
}
