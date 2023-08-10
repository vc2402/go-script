package compiler

//go:generate goyacc -o gos.go -p "Gos" -l gos.y
///go:generate goyacc -o gos.go -p "Gos" gos.y

import (
	"errors"
	"fmt"
	"github.com/vc2402/go-script/runtime"
	"go/token"
	"reflect"
	"strconv"
)

type Error struct {
	error
	pos token.Pos
}

type InternalError struct {
	problem string
}

type Compiler struct {
	*Project
	registry  *runtime.Registry
	cpackages map[string]cpckg
	Options   Options
}

type Options struct {
	AllowImplicitNumbersConversion  bool
	AllowImplicitConversionToBool   bool
	AllowImplicitConversionToString bool
	AddDebugInfo                    bool
}

type DebugInfo struct {
	Position token.Position
	Text     string
}

//cpckg package for compiler
type cpckg struct {
	*Package
	name       string
	topScope   scope
	functions  []*scope
	statements []*statement
}

type cv struct {
	name       string
	descriptor any // *VarDescriptor or *FuncDescriptor from runtime
}

type scope struct {
	vars       []cv
	scopes     []*scope
	parent     *scope
	descriptor *runtime.ScopeDescriptor
	source     any
	idx        int
}

func (p *Project) Compiler(r *runtime.Registry) Compiler {
	return Compiler{p, r, map[string]cpckg{}, Options{}}
}
func (p *Compiler) Compile() error {
	err := p.pass1()
	if err != nil {
		return err
	}
	err = p.pass2()
	if err != nil {
		return err
	}
	return nil
}

func (p *Compiler) Commit() error {
	for _, pckg := range p.cpackages {
		_, err := p.registry.SetTopScope(pckg.name, "", pckg.topScope.descriptor)
		if err != nil {
			return err
		}
	}
	return nil
}

func (p *Compiler) pass1() error {
	for name, pckg := range p.packages {
		cpkg := cpckg{
			Package: &pckg,
			name:    name,
			topScope: scope{
				descriptor: &runtime.ScopeDescriptor{Kind: runtime.SKTop},
			},
		}
		for _, file := range pckg.files {
			for _, fp := range file.parts {
				var err error
				var fs *scope
				switch v := fp.(type) {
				case *function:
					fs, err = p.processFunction1(&cpkg.topScope, v)
					cpkg.functions = append(cpkg.functions, fs)
				case *statement:
					err = p.processStatement1(&cpkg.topScope, v)
					cpkg.statements = append(cpkg.statements, v)
				}
				if err != nil {
					return WrapError(err, file.pos)
				}
			}
		}
		p.cpackages[name] = cpkg
	}
	return nil
}

func (p *Compiler) pass2() error {
	for _, pckg := range p.cpackages {
		for _, stmt := range pckg.statements {
			err := p.processStatement2(&pckg.topScope, stmt)
			if err != nil {
				return err
			}
		}
		for _, fun := range pckg.functions {
			err := p.processFunction2(fun)
			if err != nil {
				return err
			}
		}
	}
	return nil
}
func (p *Compiler) processFunction1(s *scope, f *function) (*scope, error) {
	fd := runtime.NewFuncDescriptor(f.name)
	funcArgs, err := p.varDefsToFuncParams(s, f.args)
	if err != nil {
		return nil, WrapError(err, f.pos)
	}
	retCount := 0
	getRetName := func() string { retCount++; return fmt.Sprintf("$_r%d", retCount) }
	for _, arg := range f.ret {
		if len(arg.names) == 0 {
			arg.names = []string{getRetName()}
		} else {
			for i, name := range arg.names {
				if name == "" {
					arg.names[i] = getRetName()
				}
				retCount++
			}
		}
	}
	rets, err := p.varDefsToFuncParams(s, f.ret)
	if err != nil {
		return nil, WrapError(err, f.pos)
	}
	fd.AddParam(funcArgs...).AddResult(rets...)
	fs := &scope{parent: s, source: f, idx: len(s.scopes)}
	fs.descriptor = &runtime.ScopeDescriptor{Kind: runtime.SKFunction, ScopeSpecific: runtime.FuncScopeDescriptor{Func: fd}}
	s.scopes = append(s.scopes, fs)
	idx, err := p.addVar(s, f.name, fs)
	if err != nil {
		return nil, WrapError(err, f.pos)
	}
	s.descriptor.AddVarIdx(*runtime.NewVarDescriptor(runtime.VKFunc, "", f.name).SetInitValue(fs.descriptor), idx)
	for _, arg := range f.args {
		err = p.processVarDef1(fs, arg)
		if err != nil {
			return nil, WrapError(err, f.pos)
		}
		err = p.processVarDef2(fs, arg)
		if err != nil {
			return nil, WrapError(err, f.pos)
		}
	}
	for _, arg := range f.ret {
		err = p.processVarDef1(fs, arg)
		if err != nil {
			return nil, WrapError(err, f.pos)
		}
		err = p.processVarDef2(fs, arg)
		if err != nil {
			return nil, WrapError(err, f.pos)
		}
	}
	return fs, nil
}

func (p *Compiler) processStatement1(s *scope, stmt *statement) error {
	switch stmt.kind {
	case stmtKindInit, stmtKindVar:
		return p.processVarDef1(s, stmt.stmt.(*varDef))
	}
	return nil
}

func (p *Compiler) processVarDef1(s *scope, vd *varDef) error {
	if vd.lvalues != nil {
		var err error
		vd.names, err = p.lvaluesToIdents(vd.lvalues)
		if err != nil {
			return err
		}
		vd.lvalues = nil
	}
	newVarExists := false
	for _, name := range vd.names {
		if idx, _ := s.getVar(name); idx == -1 {
			newVarExists = true
			_, err := p.addVar(s, name, vd.tip)
			if err != nil {
				return err
			}
		}
	}
	if !newVarExists {
		return NewError("no new vars on the left part of var initialization", vd.pos)
	}
	return nil
}

func (p *Compiler) processFunction2(fs *scope) error {
	err := p.processBlock(fs, fs.source.(*function).body.stmt.([]*statement))
	if err != nil {
		return WrapError(err, fs.source.(*function).pos)
	}
	return nil
}

func (p *Compiler) processBlock(s *scope, stmts []*statement) error {
	for _, stmt := range stmts {
		err := p.processStatement1(s, stmt)
		if err != nil {
			return err
		}
	}
	for _, stmt := range stmts {
		err := p.processStatement2(s, stmt)
		if err != nil {
			return err
		}
	}
	s.descriptor.Scopes = make([]runtime.ScopeDescriptor, len(s.scopes))
	for i, sc := range s.scopes {
		s.descriptor.Scopes[i] = *sc.descriptor
	}
	return nil
}

func (p *Compiler) processStatement2(s *scope, stmt *statement) error {
	switch stmt.kind {
	case stmtKindAssign:
		return p.processAssignment(s, stmt.stmt.(*assignment))
	case stmtKindInit, stmtKindVar:
		return p.processVarDef2(s, stmt.stmt.(*varDef))
	case stmtKindFunc:
		return p.processFuncCall(s, stmt.stmt.(*funcCall), true)
	case stmtKindBlock:
		bs := &scope{parent: s, source: stmt, idx: len(s.scopes)}
		bs.descriptor = &runtime.ScopeDescriptor{Kind: runtime.SKBlock}
		s.scopes = append(s.scopes, bs)
		s.descriptor.Program.Add(runtime.CKExec, runtime.ExecEnterScope, bs.idx).SetDebugInfo(p.getDebugInfo(stmt.pos))
		return p.processBlock(bs, stmt.stmt.([]*statement))
	case stmtKindIf:
		return p.processIf(s, stmt.stmt.(*ifStatement))
	case stmtKindFor:
		return p.processFor(s, stmt.stmt.(*forStatement))
	case stmtKindSwitch:
	case stmtKindExpression:
		return p.processExpression(s, stmt.stmt.(*expression), true)
	case stmtKindInc:
		return p.processIncDec(s, stmt.stmt.(*lvalue), 1)
	case stmtKindDec:
		return p.processIncDec(s, stmt.stmt.(*lvalue), -1)
	case stmtKindRet:
		return p.processReturn(s, stmt.stmt.([]*expression), stmt.pos)
	}
	return nil
}

func (p *Compiler) processVarDef2(s *scope, vd *varDef) error {
	var types []*typeRef
	var err error
	if len(vd.init) > 0 {
		types, err = p.typeRefsFromExpressions(s, vd.init)
		if err != nil {
			return WrapError(err, vd.pos)
		}
		if len(vd.names) > len(types) {
			return WrapError(fmt.Errorf("too few init values"), vd.pos)
		}
		if len(vd.names) < len(types) {
			return WrapError(fmt.Errorf("too many init values"), vd.pos)
		}
		for i, varName := range vd.names {
			_, cv := s.getVar(varName)
			if cv == nil {
				return WrapError(fmt.Errorf("undefined var: %s", varName), vd.pos)
			}
			storedDef := cv.descriptor.(*typeRef)
			if storedDef != nil && !storedDef.equalTo(types[i]) {
				return WrapError(fmt.Errorf("cannot initialize %s with value of type %s", varName, types[i].String()), vd.pos)
			}
			cv.descriptor = types[i]
		}
	} else if vd.tip == nil {
		return NewError("no type defined", vd.pos)
	}
	for i, name := range vd.names {
		tip := vd.tip
		if len(types) > i {
			tip = types[i]
		}
		varDesc, err := p.getTypeKind(tip)
		if err != nil {
			return WrapError(err, vd.pos)
		}
		if varDesc.Name() == "" {
			varDesc.SetName(name)
		}
		idx, cv := s.getVar(name)
		if tr, ok := cv.descriptor.(*typeRef); ok && tip.refType != nil {
			tr.refType = tip.refType
		}
		s.descriptor.AddVarIdx(*varDesc, idx)
	}

	if vd.init != nil {
		left := make([]*lvalue, len(vd.names))
		for i, name := range vd.names {
			left[i] = &lvalue{
				kind: lvalueKindIdent,
				stmt: name,
			}
		}
		return p.processAssignment(
			s,
			&assignment{left: left, right: vd.init, pos: vd.pos},
		)
	}
	return nil
}

func (p *Compiler) processFuncCall(s *scope, fc *funcCall, skipRets bool) error {
	types, err := p.typeRefsFromExpressions(s, fc.params)
	if err != nil {
		return err
	}

	if fc.isTypeConversion() {
		if len(types) != 1 {
			return NewError("invalid type conversion argument", fc.pos)
		}
		for _, param := range fc.params {
			err = p.processExpression(s, param, false)
			if err != nil {
				return err
			}
		}
		s.descriptor.Program.Add(runtime.CKALU, runtime.AlucConvert, int(fc.convertTo)).SetDebugInfo(p.getDebugInfo(fc.pos))
		return nil
	}

	if fc.pckg == "" {
		sid, vid, v := s.findVar(fc.name)
		if v == nil {
			return fmt.Errorf("function '%s' not found", fc.name)
		}
		fs, ok := v.descriptor.(*scope)
		if !ok {
			return fmt.Errorf("'%s' cannot be called", fc.name)
		}
		fun, ok := fs.source.(*function)
		if !ok {
			return fmt.Errorf("'%s' cannot be called", fc.name)
		}
		fd := fs.descriptor.ScopeSpecific.(runtime.FuncScopeDescriptor).Func
		if len(types) > len(fd.Params()) {
			return fmt.Errorf("'%s': too many params", fc.name)
		}
		if len(types) < len(fd.Params()) {
			return fmt.Errorf("'%s': too few params", fc.name)
		}
		argTypes := make([]*typeRef, len(fd.Params()))
		i := 0
		for _, arg := range fun.args {
			for range arg.names {
				argTypes[i] = arg.tip
				i++
			}
		}
		for i, tip := range types {
			if !tip.equalTo(argTypes[i]) {
				return fmt.Errorf("'%s' can be used as %s param", tip.String(), fd.Params()[i].Name)
			}
		}
		for _, param := range fc.params {
			err = p.processExpression(s, param, false)
			if err != nil {
				return err
			}
		}
		varName := fmt.Sprintf("%%funcArgsCount%d", len(s.vars))
		idx, err := p.addVar(s, varName, fd)
		if err != nil {
			return err
		}
		s.descriptor.AddVarIdx(*runtime.NewVarDescriptor(runtime.VKInt, "", "").SetIntInitValue(len(types)), idx)
		s.descriptor.Program.Add(runtime.CKScope, runtime.SCGetVar, 0, idx).SetDebugInfo(p.getDebugInfo(fc.pos))

		s.descriptor.Program.Add(runtime.CKExec, runtime.ExeccCall, sid, vid)
		if skipRets && len(fd.Result()) > 0 {
			s.descriptor.Program.Add(runtime.CKScope, runtime.SCPopN, len(fd.Result()))
		}
	} else {
		fd, err := p.getExtVar(fc.pckg, fc.name)
		if err != nil {
			return err
		}
		if !fd.IsFunction() {
			return fmt.Errorf("%s.%s is not a function", fc.pckg, fc.name)
		}
		var resultsCount int
		if fd.Kind() == runtime.VKFunc {
			//TODO check params
			funcDesc := fd.GetFunction().(*runtime.ScopeDescriptor).ScopeSpecific.(runtime.FuncScopeDescriptor).Func
			resultsCount = len(funcDesc.Result())
		} else {
			fun := fd.GetFunction().(reflect.Value)
			funType := fun.Type()
			argsCount := funType.NumIn()
			isVariadic := funType.IsVariadic()
			if len(types) < argsCount && !isVariadic ||
				len(types) < argsCount-1 && isVariadic {
				return errors.New("too few values for function call")
			}
			if len(types) > argsCount && !isVariadic {
				return errors.New("too many values for function call")
			}
			var argType reflect.Type
			for i, t := range types {
				if !isVariadic || i < argsCount {
					argType = funType.In(i)
				}

				vd, err := p.getTypeKind(t)
				if err != nil {
					return err
				}
				if !vd.IsAssignable(argType, p.registry) {
					if isVariadic && argType.Kind() == reflect.Slice {
						argType = argType.Elem()
						if vd.IsAssignable(argType, p.registry) {
							continue
						}
					}
					return fmt.Errorf("expression cannot be used as %d parameter", i+1)
				}
			}
			resultsCount = funType.NumOut()
		}
		for _, param := range fc.params {
			err = p.processExpression(s, param, false)
			if err != nil {
				return err
			}
		}
		varName := fmt.Sprintf("%%funcArgsCount%d", len(s.vars))
		idx, err := p.addVar(s, varName, fd)
		if err != nil {
			return err
		}
		s.descriptor.AddVarIdx(*runtime.NewVarDescriptor(runtime.VKInt, "", "").SetIntInitValue(len(types)), idx)
		s.descriptor.Program.Add(runtime.CKScope, runtime.SCGetVar, 0, idx).SetDebugInfo(p.getDebugInfo(fc.pos))

		varName = fmt.Sprintf("%%funcRef%d", len(s.vars))
		idx, err = p.addVar(s, varName, fd)
		if err != nil {
			return err
		}

		s.descriptor.AddVarIdx(*runtime.NewRefVarDescriptor(fc.pckg, fc.name), idx)
		s.descriptor.Program.Add(runtime.CKScope, runtime.SCGetVar, 0, idx)

		s.descriptor.Program.Add(runtime.CKExec, runtime.ExeccCall)
		if skipRets {
			s.descriptor.Program.Add(runtime.CKScope, runtime.SCPopN, resultsCount)
		}

	}
	return nil
}

func (p *Compiler) processMethodCall(s *scope, mc *methodCall, skipReturns bool) error {
	tr, err := p.typeRefFromLvalue(s, mc.lvalue)
	if err != nil {
		return WrapError(err, mc.pos)
	}
	if mc.lvalue.kind == lvalueKindPackage {
		fc := &funcCall{
			name:   mc.name,
			pckg:   mc.lvalue.stmt.(string),
			params: mc.params,
			pos:    0,
		}
		return p.processFuncCall(s, fc, skipReturns)
	}

	if tr == nil || tr.refType == nil {
		return NewInternalError("compiler error: type is undefined on method call", mc.lvalue.pos)
	}
	if mc.lvalue.tip == nil {
		_, err := p.typeRefFromLvalue(s, mc.lvalue)
		if err != nil {
			return err
		}
	}
	types, err := p.typeRefsFromExpressions(s, mc.params)
	m, _ := mc.lvalue.tip.refType.MethodByName(mc.name)
	fun := m.Func
	funType := fun.Type()
	argsCount := funType.NumIn() - 1
	isVariadic := funType.IsVariadic()
	if len(types) < argsCount && !isVariadic ||
		len(types) < argsCount-1 && isVariadic {
		return WrapError(errors.New("too few values for method call"), mc.pos)
	}
	if len(types) > argsCount && !isVariadic {
		return WrapError(errors.New("too many values for function call"), mc.pos)
	}
	var argType reflect.Type
	for i, t := range types {
		if !isVariadic || i < argsCount {
			argType = funType.In(i + 1)
		}

		vd, err := p.getTypeKind(t)
		if err != nil {
			return err
		}
		if !vd.IsAssignable(argType, p.registry) {
			if isVariadic && argType.Kind() == reflect.Slice {
				argType = argType.Elem()
				if vd.IsAssignable(argType, p.registry) {
					continue
				}
			}
			return fmt.Errorf("expression cannot be used as %d parameter", i+1)
		}
	}
	resultsCount := funType.NumOut()
	for _, param := range mc.params {
		err = p.processExpression(s, param, false)
		if err != nil {
			return err
		}
	}
	varName := fmt.Sprintf("%%funcArgsCount%d", len(s.vars))
	idx, err := p.addVar(s, varName, mc)
	if err != nil {
		return err
	}
	s.descriptor.AddVarIdx(*runtime.NewVarDescriptor(runtime.VKInt, "", "").SetIntInitValue(len(types)), idx)
	s.descriptor.Program.Add(runtime.CKScope, runtime.SCGetVar, 0, idx).SetDebugInfo(p.getDebugInfo(mc.pos))

	err = p.processLValueExpression(s, mc.lvalue)
	if err != nil {
		return WrapError(err, mc.lvalue.pos)
	}

	s.descriptor.Program.Add(runtime.CKExec, runtime.ExecCallMethod, 0, 0, mc.name)
	if skipReturns {
		s.descriptor.Program.Add(runtime.CKScope, runtime.SCPopN, resultsCount)
	}
	return nil
}

func (p *Compiler) processAssignment(s *scope, as *assignment) error {
	types, err := p.typeRefsFromExpressions(s, as.right)
	if err != nil {
		return WrapError(err, as.pos)
	}
	if len(types) > len(as.left) {
		return NewError("too many values", as.pos)
	}
	if len(types) < len(as.left) {
		return NewError("too few values", as.pos)
	}
	for i, lv := range as.left {
		tr, err := p.typeRefFromLvalue(s, lv)
		if err != nil {
			return WrapError(err, as.pos)
		}
		if !tr.equalTo(types[i]) {
			return WrapError(fmt.Errorf("cannot assign '%s' to '%s'", types[i].String(), lv.String()), as.pos)
		}
	}
	for _, e := range as.right {
		err = p.processExpression(s, e, false)
		if err != nil {
			return WrapError(err, as.pos)
		}
	}
	for i := len(as.left) - 1; i >= 0; i-- {
		err = p.processLValueExpression(s, as.left[i])
		if err != nil {
			return WrapError(err, as.pos)
		}
		if as.left[i].forAssignment {
			s.descriptor.Program.Add(runtime.CKScope, runtime.SCSetMap).SetDebugInfo(p.getDebugInfo(as.pos))
		} else {
			s.descriptor.Program.Add(runtime.CKScope, runtime.SCAssign).SetDebugInfo(p.getDebugInfo(as.pos))
		}
	}
	return nil
}

func (p *Compiler) processIf(s *scope, is *ifStatement) error {
	err := p.processExpression(s, is.condition, false)
	if err != nil {
		return err
	}
	thenScope := &scope{
		parent: s,
		descriptor: &runtime.ScopeDescriptor{
			Kind: runtime.SKBlock,
		},
		source: is.thn,
		idx:    len(s.scopes),
	}
	s.scopes = append(s.scopes, thenScope)
	err = p.processBlock(thenScope, is.thn.stmt.([]*statement))
	if err != nil {
		return err
	}
	if is.els != nil {
		elseScope := &scope{
			parent: s,
			descriptor: &runtime.ScopeDescriptor{
				Kind: runtime.SKBlock,
			},
			source: is.els,
			idx:    len(s.scopes),
		}
		s.scopes = append(s.scopes, elseScope)
		err := p.processBlock(elseScope, is.els.stmt.([]*statement))
		if err != nil {
			return err
		}
		s.descriptor.Program.Add(runtime.CKExec, runtime.ExeccIfThenElse, thenScope.idx, elseScope.idx).SetDebugInfo(p.getDebugInfo(is.pos))
	} else {
		s.descriptor.Program.Add(runtime.CKExec, runtime.ExeccIfThen, thenScope.idx).SetDebugInfo(p.getDebugInfo(is.pos))
	}
	return nil
}

func (p *Compiler) processFor(s *scope, fs *forStatement) error {
	return nil
}

func (p *Compiler) processExpression(s *scope, expr *expression, isStatement bool) error {
	switch expr.kind {
	case expressionKindParens:
		return p.processExpression(s, expr.left.(*expression), isStatement)
	case expressionKindConst:
		return p.processConstExpression(s, expr.left.(*constant))
	case expressionKindLValue:
		return p.processLValueExpression(s, expr.left.(*lvalue))
	case expressionKindFunc:
		return p.processFuncCall(s, expr.left.(*funcCall), isStatement)
	case expressionKindMethod:
		return p.processMethodCall(s, expr.left.(*methodCall), isStatement)
	case expressionKindSum, expressionKindSub, expressionKindMul, expressionKindDiv:
		return p.processArithmeticExpression(s, expr.left.(*expression), expr.right, expr.kind)
	case expressionKindNot:
		return p.processLogicExpression(s, expr.left.(*expression), nil, expr.kind)
	case expressionKindAnd, expressionKindOr:
		return p.processLogicExpression(s, expr.left.(*expression), expr.right, expr.kind)
	case expressionKindGreaterThan, expressionKindLessThan, expressionKindEqualTo,
		expressionKindNotEqualTo, expressionKindGreaterThanOrEqualTo, expressionKindLessThanOrEqualTo:
		return p.processCompareExpression(s, expr.left.(*expression), expr.right, expr.kind)
	}
	return fmt.Errorf("undefined expression kind: %d", expr.kind)
}

func (p *Compiler) processConstExpression(s *scope, c *constant) error {
	switch c.kind {
	case constKindInt:
		val, _ := strconv.ParseInt(c.value, 0, 64)
		s.descriptor.Program.Add(runtime.CKScope, runtime.SCLoadConst, int(runtime.VKInt), int(val)).SetDebugInfo(p.getDebugInfo(c.pos))
	case constKindFloat:
		val, _ := strconv.ParseFloat(c.value, 64)
		s.descriptor.Program.Add(runtime.CKScope, runtime.SCLoadConst, int(runtime.VKFloat), 0, "", val).SetDebugInfo(p.getDebugInfo(c.pos))
	case constKindChar:
		return errors.New("char const not implemented yet")
	case constKindString:
		s.descriptor.Program.Add(runtime.CKScope, runtime.SCLoadConst, int(runtime.VKString), 0, c.value[1:len(c.value)-1]).SetDebugInfo(p.getDebugInfo(c.pos))
	case constKindBool:
		val := 0
		if c.value == "true" {
			val = 1
		}
		s.descriptor.Program.Add(runtime.CKScope, runtime.SCLoadConst, int(runtime.VKBool), val).SetDebugInfo(p.getDebugInfo(c.pos))
	}
	//varName := fmt.Sprintf("%%const%d", len(s.vars))
	//idx, err := p.addVar(s, varName, c)
	//if err != nil {
	//	return err
	//}
	//s.descriptor.AddVarIdx(vd, idx)
	//s.descriptor.Program.Add(runtime.CKScope, runtime.SCGetVar, 0, idx)
	return nil
}

func (p *Compiler) processLValueExpression(s *scope, lv *lvalue) error {
	switch lv.kind {
	case lvalueKindIdent, lvalueKindPackage, lvalueKindVar:
		tr, err := p.typeRefFromLvalue(s, lv)
		if err != nil {
			return WrapError(err, lv.pos)
		}
		if lv.kind == lvalueKindPackage {
			return nil
		}
		if tr.name == "nil" {
			s.descriptor.Program.Add(runtime.CKScope, runtime.SCLoadConst, int(runtime.VKUndef)).SetDebugInfo(p.getDebugInfo(lv.pos))
			break
		}
		name := lv.stmt.(string)
		si, vi, _ := s.findVar(name)

		lv.kind = lvalueKindVar
		//lv.stmt = cv
		lv.tip = tr
		lv.forAssignment = false
		s.descriptor.Program.Add(runtime.CKScope, runtime.SCGetVar, si, vi).SetDebugInfo(p.getDebugInfo(lv.pos))
	case lvalueKindField:
		fa := lv.stmt.(*fieldAccess)
		_, err := p.typeRefFromLvalue(s, fa.lvalue)
		if err != nil {
			return WrapError(err, lv.pos)
		}
		if fa.lvalue.kind == lvalueKindPackage {
			//ev, err := p.getExtVar(fa.lvalue.stmt.(string), fa.field)
			//if err != nil {
			//	return WrapError(err, lv.pos)
			//}
			//lv.tip = &typeRef{pckg: fa.lvalue.stmt.(string), name: fa.field}
			//if ev.Kind() == runtime.VKExternal {
			//	lv.tip.refType = ev.Reflect().Type()
			//}
			vv := runtime.NewVarDescriptor(runtime.VKRef, fa.lvalue.stmt.(string), fa.field)
			varName := fmt.Sprintf("%%pckg%d", len(s.vars))
			idx, err := p.addVar(s, varName, fa.field)
			if err != nil {
				return err
			}
			s.descriptor.AddVarIdx(*vv, idx)
			s.descriptor.Program.Add(runtime.CKScope, runtime.SCGetVar, 0, idx).SetDebugInfo(p.getDebugInfo(fa.pos))
		} else {
			err = p.processLValueExpression(s, fa.lvalue)
			if err != nil {
				return WrapError(err, fa.lvalue.pos)
			}
			//check that field exists
			lv.tip, err = getAttrType(fa.lvalue.tip, fa.field)
			if err != nil {
				return WrapError(err, fa.lvalue.pos)
			}
			s.descriptor.Program.Add(runtime.CKScope, runtime.SCLoadConst, int(runtime.VKString), 0, fa.field).SetDebugInfo(p.getDebugInfo(fa.pos))
			s.descriptor.Program.Add(runtime.CKScope, runtime.SCDot)
		}
		lv.forAssignment = false
	case lvalueKindIndex:
		aa := lv.stmt.(*arrayAccess)
		err := p.processLValueExpression(s, aa.lvalue)
		if err != nil {
			return err
		}
		if aa.lvalue.tip == nil {
			return NewInternalError("compiler error: lvalue type of index operator is undefined", aa.pos)
		}
		err = p.processExpression(s, aa.expression, false)
		if err != nil {
			return err
		}
		var command runtime.Command = runtime.SCIndex
		isMap := true
		// duplicating code...
		if aa.lvalue.tip.elem != nil {
			if aa.lvalue.tip.mapKey == "" {
				isMap = false
			}
			aa.tip = aa.lvalue.tip.elem
		} else if aa.lvalue.tip.pckg != "" || aa.lvalue.tip.refType != nil {
			if aa.lvalue.tip.refType == nil {
				ev, err := p.getExtVar(aa.lvalue.tip.pckg, aa.lvalue.tip.name)
				if err != nil {
					return WrapError(err, aa.lvalue.pos)
				}
				if ev.Kind() == runtime.VKExternal {
					aa.lvalue.tip.refType = ev.Reflect().Type()
				}
			}
			if aa.lvalue.tip.refType != nil {
				kind := aa.lvalue.tip.refType.Kind()
				if kind == reflect.Map {
					isMap = true
				} else if kind == reflect.Slice || kind == reflect.Array {
					isMap = false
				} else {
					return WrapError(errors.New("impossible use of '[]'"), aa.pos)
				}
				aa.tip.refType = aa.lvalue.tip.refType.Elem()
			}
		} else {
			return NewInternalError("compiler error: lvalue type is undefined or not map/array", aa.pos)
		}

		if isMap {
			if aa.lvalue.forAssignment {
				// assignment operation should do this
				return nil
			} else {
				command = runtime.SCGetMap
			}
		}
		aa.lvalue.forAssignment = false
		s.descriptor.Program.Add(runtime.CKScope, command).SetDebugInfo(p.getDebugInfo(lv.pos))
	}
	return nil
}

func (p *Compiler) processArithmeticExpression(s *scope, left *expression, right *expression, oper int) error {
	rt, err := p.typeRefFromExpression(s, right)
	if err != nil {
		return err
	}
	lt, err := p.typeRefFromExpression(s, left)
	if err != nil {
		return err
	}
	err = p.processExpression(s, right, false)
	if err != nil {
		return err
	}

	err = p.processExpression(s, left, false)
	if err != nil {
		return err
	}
	leftName := lt.toEmbeddedTypeName()
	rightName := rt.toEmbeddedTypeName()
	if leftName != rightName {
		showError := false
		if rightName == "float" && leftName == "int" {
			if p.Options.AllowImplicitNumbersConversion {
				s.descriptor.Program.Add(runtime.CKALU, runtime.AlucConvert, int(runtime.VKFloat)).SetDebugInfo(p.getDebugInfo(left.pos))
				leftName = "float"
			} else {
				showError = true
			}
		} else if leftName == "string" && oper == expressionKindSum {
			if !p.Options.AllowImplicitConversionToString {
				showError = true
			}
		} else if leftName != "float" && rightName != "int" {
			showError = true
		}
		if showError {
			return WrapError(fmt.Errorf("invalid operation: mismatched types %s and %s", leftName, rightName), left.pos)
		}
	}
	if leftName != rightName {
		showError := false
		if leftName == "float" && rightName == "int" {
			if p.Options.AllowImplicitNumbersConversion {
				s.descriptor.Program.Add(runtime.CKALU, runtime.AlucConvert, int(runtime.VKFloat)).SetDebugInfo(p.getDebugInfo(right.pos))
			} else {
				showError = true
			}
		} else if leftName == "string" && oper == expressionKindSum {
			s.descriptor.Program.Add(runtime.CKALU, runtime.AlucConvert, int(runtime.VKString)).SetDebugInfo(p.getDebugInfo(right.pos))
		} else {
			showError = true
		}
		if showError {
			return WrapError(fmt.Errorf("invalid operation: mismatched types %s and %s", leftName, rightName), left.pos)
		}
	} else if lt.name != rt.name {
		if lt.refType != nil {
			//TODO for other types
			if leftName == "string" {
				s.descriptor.Program.Add(runtime.CKALU, runtime.AlucConvert, int(runtime.VKString), 0).SetDebugInfo(p.getDebugInfo(right.pos))
			}
		}
		if lt.refType != nil {
			if leftName == "string" {
				s.descriptor.Program.Add(runtime.CKALU, runtime.AlucConvert, int(runtime.VKString), 1).SetDebugInfo(p.getDebugInfo(right.pos))
			}
		}
	}
	var command runtime.Command
	switch oper {
	case expressionKindSum:
		command = runtime.AlucAdd
	case expressionKindSub:
		command = runtime.AlucSub
	case expressionKindMul:
		command = runtime.AlucMul
	case expressionKindDiv:
		command = runtime.AlucDiv
	default:
		return errors.New("compile error: undefined arithmetic command")
	}
	s.descriptor.Program.Add(runtime.CKALU, command).SetDebugInfo(p.getDebugInfo(left.pos))
	return nil
}

func (p *Compiler) processLogicExpression(s *scope, left *expression, right *expression, oper int) error {
	err := p.processExpression(s, left, false)
	if err != nil {
		return err
	}
	if right != nil {
		err = p.processExpression(s, right, false)
		if err != nil {
			return err
		}
	}
	var command runtime.Command
	switch oper {
	case expressionKindNot:
		command = runtime.AlucNot
	case expressionKindAnd:
		command = runtime.AlucAnd
	case expressionKindOr:
		command = runtime.AlucOr
	default:
		return errors.New("compile error: undefined logic command")
	}
	s.descriptor.Program.Add(runtime.CKALU, command).SetDebugInfo(p.getDebugInfo(left.pos))
	return nil
}

func (p *Compiler) processCompareExpression(s *scope, left *expression, right *expression, oper int) error {
	err := p.processExpression(s, left, false)
	if err != nil {
		return err
	}
	err = p.processExpression(s, right, false)
	if err != nil {
		return err
	}
	var command runtime.Command
	switch oper {
	case expressionKindGreaterThan:
		command = runtime.AlucGreaterThan
	case expressionKindGreaterThanOrEqualTo:
		command = runtime.AlucGreaterOrEqual
	case expressionKindLessThan:
		command = runtime.AlucLessThan
	case expressionKindLessThanOrEqualTo:
		command = runtime.AlucLessOrEqual
	case expressionKindEqualTo:
		command = runtime.AlucEqualTo
	case expressionKindNotEqualTo:
		command = runtime.AlucNotEqualTo
	default:
		return errors.New("compile error: undefined compare command")
	}
	s.descriptor.Program.Add(runtime.CKALU, command).SetDebugInfo(p.getDebugInfo(left.pos))
	return nil
}

func (p *Compiler) processIncDec(s *scope, lv *lvalue, step int) error {
	err := p.processLValueExpression(s, lv)
	if err != nil {
		return err
	}
	command := runtime.AlucInc
	if step < 0 {
		command = runtime.AlucDec
	}
	s.descriptor.Program.Add(runtime.CKALU, command).SetDebugInfo(p.getDebugInfo(lv.pos))
	return nil
}

func (p *Compiler) processReturn(s *scope, exprs []*expression, pos token.Pos) error {
	fs := s
	for fs != nil && fs.descriptor.Kind != runtime.SKFunction {
		fs = fs.parent
	}
	if fs == nil {
		return errors.New("return out of function")
	}
	fd := fs.descriptor.ScopeSpecific.(runtime.FuncScopeDescriptor).Func
	fun := fs.source.(*function)
	types, err := p.typeRefsFromExpressions(s, exprs)
	if err != nil {
		return WrapError(err, pos)
	}
	if len(types) > 0 {
		if len(types) > len(fd.Result()) {
			return errors.New("too many values to return")
		}
		if len(types) < len(fd.Result()) {
			return errors.New("too few values to return")
		}
		//TODO check types and function call
		conversionRequired := false
		for i, tip := range types {
			awaiting := fun.ret[i].tip
			if !awaiting.equalTo(tip) {
				if err := p.isConvertible(tip, awaiting); err != nil {
					return WrapError(err, pos)
				}
				conversionRequired = true
			}
		}
		if conversionRequired && len(exprs) != len(types) {
			return NewError("type mismatch", pos)
		}
		for i, expr := range exprs {
			p.processExpression(s, expr, false)
			awaiting := fun.ret[i].tip
			tip := types[i]
			if !awaiting.equalTo(tip) {
				s.descriptor.Program.Add(runtime.CKALU, runtime.AlucConvert, int(getBuiltInTypeKind(awaiting.name))).
					SetDebugInfo(p.getDebugInfo(pos))
			}
		}
	} else if len(fd.Result()) != 0 {
		startIdx := len(fd.Params())
		for range fd.Result() {
			s.descriptor.Program.Add(runtime.CKScope, runtime.SCGetVar, 0, startIdx).SetDebugInfo(p.getDebugInfo(pos))
			startIdx++
		}
	}
	s.descriptor.Program.Add(runtime.CKExec, runtime.ExeccReturn).SetDebugInfo(p.getDebugInfo(pos))
	return nil
}

func (s *scope) findVar(name string) (scopIdx int, idx int, cv *cv) {
	return s.lookForVar(0, name)
}

func (p *Compiler) addVar(s *scope, name string, descriptor any) (idx int, err error) {
	if idx, _ := s.getVar(name); idx != -1 {
		return -1, fmt.Errorf("duplicate var name: %s", name)
	}
	idx = len(s.vars)
	s.vars = append(s.vars, cv{name, descriptor})

	return
}

func (s *scope) getVar(name string) (idx int, cv *cv) {
	for idx, v := range s.vars {
		if v.name == name {
			return idx, &s.vars[idx]
		}
	}
	return -1, nil
}

func (s *scope) lookForVar(sIdx int, name string) (scopIdx int, idx int, cv *cv) {
	if idx, v := s.getVar(name); idx != -1 {
		return sIdx, idx, v
	}
	if s.parent != nil {
		return s.parent.lookForVar(sIdx+1, name)
	}
	return -1, -1, nil
}

func (p *Compiler) checkType(tip *typeRef) error {
	_, err := p.getTypeKind(tip)
	return err
}

func (p *Compiler) getTypeKind(tip *typeRef) (*runtime.VarDescriptor, error) {
	if tip.elem != nil {
		et, err := p.getTypeKind(tip.elem)
		if err != nil {
			return nil, err
		}
		if tip.mapKey != "" {
			//TODO check mapKey
			return runtime.NewVarDescriptor(runtime.VKMap, "", tip.name, et,
				runtime.NewVarDescriptor(runtime.VKString, "", "")), nil
		} else {
			return runtime.NewVarDescriptor(runtime.VKSlice, "", tip.name, et), nil
		}
	}

	if tip.pckg != "" {
		if tip.refType == nil {
			pckg, err := p.registry.GetPackage(tip.pckg)
			if err != nil {
				return nil, err
			}
			rt, err := pckg.GetReflectType(tip.name)
			if err != nil {
				return nil, err
			}
			tip.refType = rt
		}
		// TODO check script defined types when appear
		return runtime.NewVarDescriptor(runtime.VKExternal, tip.pckg, tip.name).SetInitValue(tip.refType), nil
	}
	if tip.refType != nil && tip.refType.Kind() != reflect.Invalid {
		return runtime.NewVarDescriptor(runtime.VKExternal, "", "").SetInitValue(reflect.Zero(tip.refType)), nil
	}
	// built-in types
	return varDescriptorForBuiltInType(tip.name)
}

func (p *Compiler) varDefsToFuncParams(s *scope, varDefs []*varDef) ([]runtime.FuncParam, error) {
	var vars []runtime.FuncParam
	var types []*typeRef
	var err error
	for _, v := range varDefs {
		if v.tip != nil {
			err := p.checkType(v.tip)
			if err != nil {
				return nil, err
			}
			types = make([]*typeRef, len(v.names))
			for i := range v.names {
				types[i] = v.tip
			}
		} else if len(v.init) > 0 {
			types, err = p.typeRefsFromExpressions(s, v.init)
			if err != nil {
				return nil, err
			}
		}
		if len(v.names) > len(types) {
			return nil, errors.New("too few values")
		}
		if len(v.names) < len(types) {
			return nil, errors.New("too many values")
		}
		for i, name := range v.names {
			descr, err := p.getTypeKind(types[i])
			//descr.SetName(name)
			if err != nil {
				return nil, err
			}
			vars = append(vars, runtime.FuncParam{Name: name, Descriptor: descr})
		}
	}
	return vars, nil
}

func (p *Compiler) typeRefsFromExpressions(s *scope, expressions []*expression) ([]*typeRef, error) {
	var ret []*typeRef
	for _, expr := range expressions {
		switch expr.kind {
		case expressionKindMethod:
			mc := expr.left.(*methodCall)
			_, err := p.typeRefFromLvalue(s, mc.lvalue)
			if err != nil {
				return nil, err
			}
			if mc.lvalue.kind != lvalueKindPackage {
				return p.typeRefsFromMethodCall(s, mc)
			}
			expr.kind = expressionKindFunc
			expr.left = &funcCall{name: mc.name, pckg: mc.lvalue.stmt.(string), params: mc.params, pos: mc.pos}
			fallthrough
		case expressionKindFunc:
			tr, err := p.typeRefsFromFuncCall(s, expr.left.(*funcCall))
			if err != nil {
				return nil, err
			}
			if len(tr) > 1 && len(expressions) > 1 {
				return nil, errors.New("func call in expressions list")
			}
			ret = append(ret, tr...)

		default:
			tr, err := p.typeRefFromExpression(s, expr)
			if err != nil {
				return nil, err
			}
			ret = append(ret, tr)
		}
	}
	return ret, nil
}

func (p *Compiler) typeRefsFromFuncCall(s *scope, fc *funcCall) ([]*typeRef, error) {
	if fc.returnTypes != nil {
		return fc.returnTypes, nil
	}
	if fc.pckg == "" {
		if fc.isTypeConversion() {
			return fc.returnTypes, nil
		}
		name := fc.name

		_, _, v := s.findVar(name)
		if v == nil {
			return nil, fmt.Errorf("%s: not found", name)
		}
		fs, ok := v.descriptor.(*scope)
		if !ok {
			return nil, fmt.Errorf("'%s' cannot be called", name)
		}
		fd := fs.descriptor.ScopeSpecific.(runtime.FuncScopeDescriptor).Func
		if !ok {
			return nil, fmt.Errorf("'%s': not a function", name)
		}
		ret := make([]*typeRef, len(fd.Result()))
		for i, res := range fd.Result() {
			ret[i] = varDescriptorToTypeRef(res.Descriptor)
		}
		fc.returnTypes = ret
		return ret, nil
	} else {
		pack, err := p.registry.GetPackage(fc.pckg)
		if err != nil {
			return nil, err
		}
		fd := pack.GetVarByName(fc.name)
		if fd == nil {
			return nil, fmt.Errorf("%s: not found in package %s", fc.name, fc.pckg)
		}
		if !fd.IsFunction() {
			return nil, fmt.Errorf("%s.%s is not a function", fc.pckg, fc.name)
		}
		if fd.Kind() == runtime.VKFunc {
			funcDesc := fd.GetFunction().(*runtime.ScopeDescriptor).ScopeSpecific.(runtime.FuncScopeDescriptor).Func
			ret := make([]*typeRef, len(funcDesc.Result()))
			for i, res := range funcDesc.Result() {
				ret[i] = varDescriptorToTypeRef(res.Descriptor)
			}
			fc.returnTypes = ret
			return ret, nil
		} else {
			return p.typeRefsFromReflectFuncCall(s, fd.GetFunction().(reflect.Value).Type())
		}
	}
}

func (p *Compiler) typeRefsFromMethodCall(s *scope, mc *methodCall) ([]*typeRef, error) {
	// currently it is for reflect only
	if mc.lvalue.tip == nil {
		_, err := p.typeRefFromLvalue(s, mc.lvalue)
		if err != nil {
			return nil, err
		}
	}
	if mc.lvalue.tip.pckg != "" || mc.lvalue.tip.refType != nil {
		if mc.lvalue.tip.refType == nil {
			ev, err := p.getExtVar(mc.lvalue.tip.pckg, mc.lvalue.tip.name)
			if err != nil {
				return nil, WrapError(err, mc.pos)
			}
			if ev.Kind() == runtime.VKExternal {
				mc.lvalue.tip.refType = ev.Reflect().Type()
			}
		}
		if mc.lvalue.tip.refType != nil {
			tip := mc.lvalue.tip.refType
			if tip.Kind() == reflect.Pointer {
				tip = tip.Elem()
			}
			m, ok := mc.lvalue.tip.refType.MethodByName(mc.name)
			if !ok {
				return nil, NewError("no such method", mc.pos)
			}
			return p.typeRefsFromReflectFuncCall(s, m.Type)
		} else {
			return nil, NewInternalError("lvalue type cannot be defined for method call", mc.pos)
		}
	}
	return nil, WrapError(fmt.Errorf("%s cannot be called", mc.name), mc.pos)
}

func (p *Compiler) typeRefsFromReflectFuncCall(s *scope, funType reflect.Type) ([]*typeRef, error) {
	retsCount := funType.NumOut()
	ret := make([]*typeRef, retsCount)
	for i := 0; i < retsCount; i++ {
		refType := funType.Out(i)
		retDesc, err := runtime.NewDescriptorFromReflect(refType)
		if err != nil {
			return nil, err
		}
		ret[i] = varDescriptorToTypeRef(retDesc)
		ret[i].refType = refType
	}
	return ret, nil
}

func (p *Compiler) typeRefFromExpression(s *scope, expr *expression) (*typeRef, error) {
	if expr.typeRef != nil {
		return expr.typeRef, nil
	}
	var err error
	switch expr.kind {
	case expressionKindParens:
		return p.typeRefFromExpression(s, expr.left.(*expression))
	case expressionKindConst:
		expr.typeRef, err = expr.left.(*constant).toTypeRef()
		return expr.typeRef, err
	case expressionKindLValue:
		expr.typeRef, err = p.typeRefFromLvalue(s, expr.left.(*lvalue))
		return expr.typeRef, err
	case expressionKindFunc:
		fc := expr.left.(*funcCall)
		tr, err := p.typeRefsFromFuncCall(s, fc)
		if err != nil {
			return nil, err
		}
		if len(tr) < 1 {
			return nil, fmt.Errorf("%s: too little return values", fc.name)
		}
		if len(tr) > 1 {
			return nil, fmt.Errorf("%s: too many return values", fc.name)
		}
		expr.typeRef = tr[0]
		return tr[0], nil
	case expressionKindMethod:
		fc := expr.left.(*methodCall)
		tr, err := p.typeRefsFromMethodCall(s, fc)
		if err != nil {
			return nil, err
		}
		if len(tr) < 1 {
			return nil, fmt.Errorf("%s: too little return values", fc.name)
		}
		if len(tr) > 1 {
			return nil, fmt.Errorf("%s: too many return values", fc.name)
		}
		expr.typeRef = tr[0]
		return tr[0], nil
	case expressionKindSum, expressionKindSub, expressionKindMul, expressionKindDiv:
		lp, err := p.typeRefFromExpression(s, expr.left.(*expression))
		if err != nil {
			return nil, err
		}
		rp, err := p.typeRefFromExpression(s, expr.right)
		if err != nil {
			return nil, err
		}
		if err := p.isConvertible(rp, lp); err != nil {
			return nil, err
		}
		if rp.refType != nil && lp.refType != nil {
			return rp, nil
		}
		leftType := lp.toEmbeddedTypeName()
		rightType := rp.toEmbeddedTypeName()
		switch leftType {
		case "float":
			if rightType == "float" || rightType == "int" {
				return lp, nil
			}
		case "int":
			if rightType == "float" || rightType == "int" {
				return rp, nil
			}
		case "string":
			if rightType == "string" {
				return lp, nil
			}
		}
		return nil, fmt.Errorf("impossible operation between %s and %s", lp.name, rp.name)
	case expressionKindNot:
		lp, err := p.typeRefFromExpression(s, expr.left.(*expression))
		if err != nil {
			return nil, err
		}
		if lp.name == "bool" {
			return lp, nil
		}
		return nil, fmt.Errorf("bool operand required but found %s", lp.name)
	case expressionKindAnd, expressionKindOr:
		lp, err := p.typeRefFromExpression(s, expr.left.(*expression))
		if err != nil {
			return nil, err
		}
		rp, err := p.typeRefFromExpression(s, expr.right)
		if err != nil {
			return nil, err
		}
		if lp.name == "bool" && rp.name == "bool" {
			return lp, nil
		}
		return nil, fmt.Errorf("bool operands required but found %s and %s", lp.name, rp.name)
	case expressionKindGreaterThan, expressionKindLessThan, expressionKindEqualTo,
		expressionKindNotEqualTo, expressionKindGreaterThanOrEqualTo, expressionKindLessThanOrEqualTo:
		lp, err := p.typeRefFromExpression(s, expr.left.(*expression))
		if err != nil {
			return nil, err
		}
		rp, err := p.typeRefFromExpression(s, expr.right)
		if err != nil {
			return nil, err
		}
		if areComparable(lp.name, rp.name) {
			return &typeRef{name: "bool"}, nil
		}
		return nil, fmt.Errorf("not comparable types found %s and %s", lp.name, rp.name)
	}
	return nil, fmt.Errorf("undefined expression kind: %d", expr.kind)
}

func (c *constant) toTypeRef() (*typeRef, error) {
	return &typeRef{name: consts[c.kind]}, nil
}

func (p *Compiler) typeRefFromLvalue(s *scope, lv *lvalue) (*typeRef, error) {
	switch lv.kind {
	case lvalueKindIdent, lvalueKindPackage, lvalueKindVar:
		if lv.tip != nil {
			return lv.tip, nil
		}
		name := lv.stmt.(string)
		if name == "nil" {
			return &typeRef{name: "nil"}, nil
		}
		_, _, cv := s.findVar(name)
		if cv == nil {
			if _, ok := p.cpackages[name]; ok {
				lv.kind = lvalueKindPackage
				return nil, nil
			}
			if pc, _ := p.registry.GetPackage(name); pc != nil {
				lv.kind = lvalueKindPackage
				return nil, nil
			}
			return nil, WrapError(fmt.Errorf("'%s': not found", lv.stmt.(string)), lv.pos)
		}
		lv.tip, _ = cv.descriptor.(*typeRef)

		return lv.tip, nil
	case lvalueKindIndex:
		aa := lv.stmt.(*arrayAccess)
		arrayType, err := p.typeRefFromLvalue(s, aa.lvalue)
		if err != nil {
			return nil, err
		}
		return arrayType.Elem()
	case lvalueKindField:
		fa := lv.stmt.(*fieldAccess)
		if lv.tip != nil {
			return lv.tip, nil
		}
		//if fa.tip != nil {
		//	return fa.tip, nil
		//}
		_, err := p.typeRefFromLvalue(s, fa.lvalue)
		if err != nil {
			return nil, err
		}
		if fa.lvalue.kind == lvalueKindPackage {
			ev, err := p.getExtVar(fa.lvalue.stmt.(string), fa.field)
			if err != nil {
				return nil, WrapError(err, lv.pos)
			}
			lv.tip = &typeRef{pckg: fa.lvalue.stmt.(string), name: fa.field}
			if ev.Kind() == runtime.VKExternal {
				lv.tip.refType = ev.Reflect().Type()
			}
			return lv.tip, nil
		} else {
			if fa.lvalue.tip == nil {
				return nil, NewInternalError("compiler error: lvalue type is undefined", fa.pos)
			}
			if fa.lvalue.tip.pckg != "" || fa.lvalue.tip.refType != nil {
				if fa.lvalue.tip.refType == nil {
					ev, err := p.getExtVar(fa.lvalue.tip.pckg, fa.lvalue.tip.name)
					if err != nil {
						return nil, WrapError(err, fa.lvalue.pos)
					}
					if ev.Kind() == runtime.VKExternal {
						fa.lvalue.tip.refType = ev.Reflect().Type()
					}
				}
				tip, err := getAttrType(fa.lvalue.tip, fa.field)
				if err != nil {
					return nil, WrapError(err, fa.pos)
				}
				lv.tip = tip
				return tip, nil
				//if fa.lvalue.tip.refType != nil {
				//  tip := fa.lvalue.tip.refType
				//  if tip.Kind() == reflect.Pointer {
				//    tip = tip.Elem()
				//  }
				//  sf, ok := tip.FieldByName(fa.field)
				//  if !ok {
				//    typeName := tip.Name()
				//    if tip.Kind() == reflect.Pointer {
				//      typeName = tip.Elem().Name()
				//    }
				//    return nil,
				//      WrapError(
				//        fmt.Errorf("field '%s' not found in type '%s'", fa.field, typeName),
				//        fa.pos,
				//      )
				//  }

				//} else {
				//  return nil, NewInternalError("compiler error: lvalue type can not be determined", fa.pos)
				//}
			} else {
				return nil, NewInternalError("compiler error: lvalue type is undefined for field access", fa.pos)
			}
		}
		return nil, NewInternalError("compiler error: lvalue type is undefined or not complex", fa.pos)
	}
	return nil, fmt.Errorf("undefined lvalue kind: %d", lv.kind)
}

func getAttrType(ref *typeRef, fieldName string) (*typeRef, error) {
	if ref.refType != nil {
		tip := ref.refType
		if tip.Kind() == reflect.Pointer {
			tip = tip.Elem()
		}
		if tip.Kind() != reflect.Struct {
			return nil, fmt.Errorf("type '%s' is not a struct", tip.Name())
		}
		sf, ok := tip.FieldByName(fieldName)
		if !ok {
			typeName := tip.Name()
			if tip.Kind() == reflect.Pointer {
				typeName = tip.Elem().Name()
			}
			return nil, fmt.Errorf("field '%s' not found in type '%s'", fieldName, typeName)
		}
		return &typeRef{name: sf.Type.Name(), pckg: sf.Type.PkgPath(), refType: sf.Type}, nil
	}
	return nil, errors.New("compiler error: lvalue type can not be determined")
}

func varDescriptorToTypeRef(vd *runtime.VarDescriptor) *typeRef {
	tr := &typeRef{
		name: vd.Name(),
		pckg: vd.Package(),
	}
	if vd.Key() != nil {
		tr.mapKey = vd.Key().Name()
	}
	if vd.Elem() != nil {
		tr.elem = varDescriptorToTypeRef(vd.Elem())
	}
	return tr
}

func areComparable(t1, t2 string) bool {
	return isNumber(t1) && isNumber(t2) ||
		t1 == "string" && t2 == "string" ||
		t1 == "bool" && t2 == "bool"
}

func isNumber(t string) bool {
	return t == "int" || t == "float"
}

func (e Error) Error() string {
	return e.error.Error()
}

func (e Error) Pos() token.Pos {
	return e.pos
}

func (e Error) Unwrap() error {
	return e.error
}

func (e InternalError) Error() string {
	return e.problem
}

func WrapError(e error, pos token.Pos) Error {
	if err, ok := e.(Error); ok {
		return err
	}
	return Error{error: e, pos: pos}
}

func NewError(msg string, pos token.Pos) Error {
	return Error{error: errors.New(msg), pos: pos}
}

func NewInternalError(msg string, pos token.Pos) Error {
	return Error{error: InternalError{problem: msg}, pos: pos}
}

func (p *Compiler) getExtVar(pckg, name string) (v *runtime.Value, err error) {
	pack, err := p.registry.GetPackage(pckg)
	if err != nil {
		return nil, err
	}
	v = pack.GetVarByName(name)
	if v == nil {
		err = fmt.Errorf("%s: not found in package %s", name, pckg)
	}
	return
}

func (p *Compiler) typeRefToReflectType(tr *typeRef) (reflect.Type, error) {
	if tr.refType != nil {
		return tr.refType, nil
	}
	pack, err := p.registry.GetPackage(tr.pckg)
	if err != nil {
		return nil, err
	}
	t, err := pack.GetReflectType(tr.name)
	if err == nil {
		tr.refType = t
	}
	return t, err
}

func (fc *funcCall) isTypeConversion() bool {
	if fc.returnTypes == nil && fc.pckg == "" {
		switch fc.name {
		case "int":
			fc.convertTo = runtime.VKInt
		case "float":
			fc.convertTo = runtime.VKFloat
		case "string":
			fc.convertTo = runtime.VKString
		case "bool":
			fc.convertTo = runtime.VKBool
		}
		fc.returnTypes = []*typeRef{{name: fc.name}}
	}
	return fc.convertTo != runtime.VKUndef && fc.convertTo != runtime.VarKind(-1)
}

func (fc *funcCall) isEmbedded() bool {
	if fc.returnTypes == nil && fc.pckg == "" {
		switch fc.name {
		case "len", "cap", "append":
			fc.convertTo = runtime.VarKind(-1)
		}
	}
	return fc.convertTo == runtime.VarKind(-1)
}

func (p *Compiler) getDebugInfo(pos token.Pos) any {
	if p.Options.AddDebugInfo && p.FileSet != nil {
		position := p.FileSet.Position(pos)
		return DebugInfo{Position: position, Text: position.String()}
	}
	return nil
}

func (di *DebugInfo) String() string { return di.Text }

func (p *Compiler) isConvertible(from *typeRef, to *typeRef) error {
	if from.equalTo(to) {
		return nil
	}
	if to.name == "bool" {
		if p.Options.AllowImplicitConversionToBool {
			return nil
		}
	}
	if from.pckg != "" || to.pckg != "" {
		return errors.New("complex types cannot be converted")
	}
	if isNumber(from.name) && isNumber(to.name) {
		if p.Options.AllowImplicitNumbersConversion {
			return nil
		}
	}
	if to.name == "string" && isNumber(from.name) || from.name == "bool" {
		if p.Options.AllowImplicitConversionToString {
			return nil
		}
	}

	return fmt.Errorf("invalid operation: mismatched types %s and %s", from.name, to.name)
}

func getBuiltInTypeKind(tn string) runtime.VarKind {
	for idx, t := range builtInTypes {
		if tn == t {
			return builtInTypesMapping[idx]
		}
	}
	panic("undefined type")
}

func varDescriptorForBuiltInType(tn string) (*runtime.VarDescriptor, error) {
	for idx, t := range builtInTypes {
		if tn == t {
			return runtime.NewVarDescriptor(builtInTypesMapping[idx], "", tn), nil
		}
	}
	return nil, fmt.Errorf("undefined type %s", tn)
}
