package compiler

import (
	"errors"
	"fmt"
	"github.com/vc2402/go-script/runtime"
	"go/token"
	"reflect"
	"strings"
)

type Project struct {
	packages map[string]Package
	FileSet  *token.FileSet
}

type Package struct {
	name  string
	files []File
}

type File struct {
	//functions  []*function
	//statements []*statement
	pckg  string
	parts []any
	pos   token.Pos
}

type typeRef struct {
	name    string
	pckg    string
	mapKey  string
	elem    *typeRef
	refType reflect.Type
	pos     token.Pos
}

type function struct {
	name string
	args []*varDef
	ret  []*varDef
	body *statement
	pos  token.Pos
}

type statement struct {
	kind statementKind
	stmt any
	pos  token.Pos
}

type lvalue struct {
	kind          int
	stmt          any
	forAssignment bool
	tip           *typeRef
	pos           token.Pos
}

type constant struct {
	kind  int
	value string
	pos   token.Pos
}
type expression struct {
	kind    int
	left    any
	right   *expression
	pos     token.Pos
	typeRef *typeRef
}
type arrayAccess struct {
	*lvalue
	*expression
	pos token.Pos
}
type assignment struct {
	left  []*lvalue
	right []*expression
	pos   token.Pos
}
type fieldAccess struct {
	*lvalue
	field string
	pos   token.Pos
}
type funcCall struct {
	name        string
	pckg        string
	params      []*expression
	pos         token.Pos
	returnTypes []*typeRef
	convertTo   runtime.VarKind
}
type methodCall struct {
	*lvalue
	name   string
	params []*expression
	pos    token.Pos
}
type varDef struct {
	names   []string
	lvalues []*lvalue
	tip     *typeRef
	init    []*expression
	pos     token.Pos
}

type ifStatement struct {
	condition *expression
	thn       *statement
	els       *statement
	pos       token.Pos
}

type forStatement struct {
	condition *expression
	initStmt  *statement
	postStmt  *statement
	rangeStmt *statement
	block     *statement
	pos       token.Pos
}

type statementKind int

const (
	stmtKindEmpty statementKind = iota
	stmtKindVar
	stmtKindInit
	stmtKindFunc
	stmtKindAssign
	stmtKindBlock
	stmtKindIf
	stmtKindFor
	stmtKindSwitch
	stmtKindExpression
	stmtKindInc
	stmtKindDec
	stmtKindRet
)

var statements = [...]string{
	"empty",
	"var",
	"init",
	"func-call",
	"assign",
	"block",
	"if",
	"for",
	"switch",
}

const (
	lvalueKindIdent = iota
	lvalueKindIndex
	lvalueKindField
	lvalueKindVar
	lvalueKindPackage
)

var lvalues = [...]string{
	"identifier",
	"index",
	"field-access",
}

const (
	expressionKindParens = iota
	expressionKindConst
	expressionKindLValue
	expressionKindFunc
	expressionKindMethod
	expressionKindSum
	expressionKindSub
	expressionKindMul
	expressionKindDiv
	expressionKindNot
	expressionKindAnd
	expressionKindOr
	expressionKindGreaterThan
	expressionKindLessThan
	expressionKindEqualTo
	expressionKindNotEqualTo
	expressionKindGreaterThanOrEqualTo
	expressionKindLessThanOrEqualTo
)

var expressions = [...]string{
	"parens",
	"+",
	"-",
	"*",
	"/",
	"!",
	"&&",
	"||",
	">",
	"<",
	"==",
	"!=",
	">=",
	"<=",
}

const (
	constKindInt = iota
	constKindFloat
	constKindChar
	constKindString
	constKindBool
)

var consts = [...]string{
	"int",
	"float",
	"char",
	"string",
	"bool",
}

func (f File) String() string {
	ret := strings.Builder{}
	for _, part := range f.parts {
		switch v := part.(type) {
		case *function:
			ret.WriteString(v.String())
		case *statement:
			ret.WriteString(v.String())
		default:
			return fmt.Sprintf("udefined type in file.part: %T", part)
		}
	}
	return ret.String()
}

func (f *function) String() string {
	ret := strings.Builder{}
	ret.WriteString(fmt.Sprintf("function: %s; \n\targs:\n", f.name))
	for _, arg := range f.args {
		ret.WriteString(fmt.Sprintf("\t\t%s: %v\n", arg.names, arg.tip))
	}
	ret.WriteString(fmt.Sprintf("\tbody: \n"))
	for _, s := range f.body.stmt.([]*statement) {
		ret.WriteString(fmt.Sprintf("\t\t%v\n", s))
	}
	return ret.String()
}

func (s *statement) String() string {
	ret := strings.Builder{}
	ret.WriteString(fmt.Sprintf("statement: %s\n", statements[s.kind]))
	ret.WriteString(fmt.Sprintf("\t%v\n", s.stmt))
	return ret.String()
}

func (lv *lvalue) String() string {
	ret := strings.Builder{}
	ret.WriteString(fmt.Sprintf("lvalue: %s\n", lvalues[lv.kind]))
	ret.WriteString(fmt.Sprintf("\t%v\n", lv.stmt))
	return ret.String()
}

func (c *constant) String() string {
	ret := strings.Builder{}
	ret.WriteString(fmt.Sprintf("const: %s = %s\n", consts[c.kind], c.value))
	return ret.String()
}

func (ex *expression) String() string {
	ret := strings.Builder{}
	ret.WriteString(fmt.Sprintf("expression: %s\n", expressions[ex.kind]))
	ret.WriteString(fmt.Sprintf("\tleft: %v\n", ex.left))
	ret.WriteString(fmt.Sprintf("\tright: %v\n", ex.right))
	return ret.String()
}

func (aa *arrayAccess) String() string {
	ret := strings.Builder{}
	ret.WriteString(fmt.Sprintf("index: %v[%v]\n", aa.lvalue, aa.expression))
	return ret.String()
}

func (as *assignment) String() string {
	ret := strings.Builder{}
	ret.WriteString(fmt.Sprintf("assignment: %v = %v\n", as.left, as.right))
	return ret.String()
}

func (fa *fieldAccess) String() string {
	ret := strings.Builder{}
	ret.WriteString(fmt.Sprintf("fieldAccess: %v . %v\n", fa.lvalue, fa.field))
	return ret.String()
}

func (fc *funcCall) String() string {
	ret := strings.Builder{}
	ret.WriteString(fmt.Sprintf("func-call: %s (\n", fc.name))
	for _, param := range fc.params {
		ret.WriteString(fmt.Sprintf("\t%v\n", param))
	}
	ret.WriteString(")")
	return ret.String()
}

func (vd *varDef) String() string {
	ret := strings.Builder{}
	ret.WriteString(fmt.Sprintf("var: %s %s", vd.names, vd.tip))
	if vd.init != nil {
		ret.WriteString(fmt.Sprintf(" = %v\n", vd.init))
	} else {
		ret.WriteString("\n")
	}
	return ret.String()
}

func (f File) Cmd() string {
	ret := strings.Builder{}
	for _, part := range f.parts {
		switch v := part.(type) {
		case *function:
			ret.WriteString(v.Cmd())
		case *statement:
			ret.WriteString(v.Cmd())
		default:
			return fmt.Sprintf("udefined type in file.part: %T", part)
		}
	}
	return ret.String()
}

func (f *function) Cmd() string {
	ret := strings.Builder{}
	ret.WriteString(fmt.Sprintf("DEF FUNC: (%s); \n", f.name))
	for _, arg := range f.args {
		ret.WriteString(fmt.Sprintf("\tDEF ARG%v: %v\n", arg.names, arg.tip))
	}
	ret.WriteString(fmt.Sprintf("\tCOMMANDS: \n"))
	for _, s := range f.body.stmt.([]*statement) {
		ret.WriteString(fmt.Sprintf("\t\t%s\n", s.Cmd()))
	}
	ret.WriteString(fmt.Sprintf("END DEF FUNC: (%s); \n", f.name))
	return ret.String()
}

func (s *statement) Cmd() string {
	ret := strings.Builder{}
	switch s.kind {
	case stmtKindAssign:
		ret.WriteString(fmt.Sprintf("%s", s.stmt.(*assignment).Cmd()))
	case stmtKindFunc:
		ret.WriteString(fmt.Sprintf("%s", s.stmt.(*funcCall).Cmd()))
	case stmtKindInit, stmtKindVar:
		ret.WriteString(fmt.Sprintf("%s", s.stmt.(*varDef).Cmd()))
	}
	return ret.String()
}

func (lv *lvalue) Cmd() string {
	ret := strings.Builder{}
	switch lv.kind {
	case lvalueKindIdent:
		ret.WriteString(fmt.Sprintf("GET VAR '%s'\n", lv.stmt.(string)))
	case lvalueKindIndex:
		ret.WriteString(fmt.Sprintf("%s", lv.stmt.(*arrayAccess).Cmd()))
		ret.WriteString("INDEX\n")
	case lvalueKindField:
		ret.WriteString(fmt.Sprintf("%s", lv.stmt.(*fieldAccess).Cmd()))
	}
	return ret.String()
}

func (c *constant) Cmd() string {
	ret := strings.Builder{}
	ret.WriteString(fmt.Sprintf("%s '%s'\n", consts[c.kind], c.value))
	return ret.String()
}

func (ex *expression) Cmd() string {
	ret := strings.Builder{}
	switch ex.kind {
	case expressionKindSum:
		ret.WriteString(ex.right.Cmd())
		//ret.WriteString(ex.left.Cmd())
		ret.WriteString("ADD\n")
	case expressionKindSub:
		ret.WriteString(ex.right.Cmd())
		//ret.WriteString(ex.left.Cmd())
		ret.WriteString("SUB\n")
	case expressionKindMul:
		ret.WriteString(ex.right.Cmd())
		//ret.WriteString(ex.left.Cmd())
		ret.WriteString("MUL\n")
	case expressionKindDiv:
		ret.WriteString(ex.right.Cmd())
		//ret.WriteString(ex.left.Cmd())
		ret.WriteString("DIV\n")
	}
	return ret.String()
}

func (aa *arrayAccess) Cmd() string {
	ret := strings.Builder{}
	ret.WriteString(aa.lvalue.Cmd())
	ret.WriteString(aa.expression.Cmd())
	return ret.String()
}

func (as *assignment) Cmd() string {
	ret := strings.Builder{}
	//ret.WriteString(as.left.Cmd())
	//ret.WriteString(as.expression.Cmd())
	ret.WriteString("ASSIGN")
	return ret.String()
}

func (fa *fieldAccess) Cmd() string {
	ret := strings.Builder{}
	ret.WriteString(fa.lvalue.Cmd())
	ret.WriteString(fmt.Sprintf("PUSH '%s'\n", fa.field))
	ret.WriteString("FIELD_ACCESS\n")
	return ret.String()
}

func (fc *funcCall) Cmd() string {
	ret := strings.Builder{}
	for _, param := range fc.params {
		ret.WriteString(param.Cmd())
	}
	ret.WriteString(fmt.Sprintf("CALL '%s'\n", fc.name))
	return ret.String()
}

func (vd *varDef) Cmd() string {
	ret := strings.Builder{}
	ret.WriteString(fmt.Sprintf("CREATE VAR %v: %s\n", vd.names, vd.tip))
	if vd.init != nil {
		//init := assignment{
		//	lvalue:     &lvalue{kind: lvalueKindIdent, stmt: vd.name},
		//	expression: vd.init,
		//}
		//ret.WriteString(init.Cmd())
	}
	return ret.String()
}

func (p *Project) AddFile(f File) {
	if p.packages == nil {
		p.packages = map[string]Package{}
	}
	if pckg, ok := p.packages[f.pckg]; ok {
		pckg.files = append(pckg.files, f)
		p.packages[f.pckg] = pckg
	} else {
		p.packages[f.pckg] = Package{name: f.pckg, files: []File{f}}
	}
}

func (tr *typeRef) equalTo(other *typeRef) bool {
	if tr.refType != nil && other.refType != nil {
		return tr.refType == other.refType
	}
	if tr.refType == nil && other.refType != nil {
		return tr.isAssignable(other.refType)
	}
	if tr.refType != nil && other.refType == nil {
		return other.isAssignable(tr.refType)
	}
	return tr.pckg == other.pckg && tr.name == other.name
}

func (tr *typeRef) isAssignable(other reflect.Type) bool {
	if other == nil || other.Kind() == reflect.Invalid {
		return false
	}
	kind := other.Kind()
	switch tr.name {
	case "int":
		return kind == reflect.Int || kind == reflect.Int8 || kind == reflect.Int16 || kind == reflect.Int32 || kind == reflect.Int64 ||
			kind == reflect.Uint || kind == reflect.Uint8 || kind == reflect.Uint16 || kind == reflect.Uint32 || kind == reflect.Uint64
	case "float":
		return kind == reflect.Float32 || kind == reflect.Float64
	case "bool":
		return kind == reflect.Bool
	case "char":
		return kind == reflect.Int32
	case "string":
		return kind == reflect.String
	case "any":
		return kind == reflect.Interface
	case "error":
		return kind == reflect.Interface
	case "slice":
		return kind == reflect.Slice || kind == reflect.Array
	case "map":
		return kind == reflect.Map
	}
	return false
}
func (tr *typeRef) String() string {
	if tr.pckg != "" {
		return fmt.Sprintf("%s.%s", tr.pckg, tr.name)
	} else {
		return tr.name
	}
}

func (tr *typeRef) isMap() bool {
	ok := tr.mapKey != ""

	if !ok && tr.refType != nil && tr.refType.Kind() == reflect.Map {
		ok = true
	}
	return ok
}

func (tr *typeRef) isArray() bool {
	ok := tr.mapKey == "" && tr.elem != nil

	if !ok && tr.refType != nil && tr.refType.Kind() == reflect.Slice {
		ok = true
	}
	return ok
}

func (tr *typeRef) Elem() (*typeRef, error) {
	if tr.elem != nil {
		return tr.elem, nil
	}
	if tr.isMap() || tr.isArray() {
		return &typeRef{refType: tr.refType.Elem(), name: tr.refType.Name(), pos: tr.pos}, nil
	}
	return nil, errors.New("not an array or map")
}

func (p *Compiler) lvaluesToIdents(ll []*lvalue) ([]string, error) {
	ret := make([]string, len(ll))
	for i, lv := range ll {
		if lv.kind != lvalueKindIdent {
			return nil, NewError("only identifier may be used in var definition", lv.pos)
		}
		ret[i] = lv.stmt.(string)
	}
	return ret, nil
}
