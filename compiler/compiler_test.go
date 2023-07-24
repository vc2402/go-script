package compiler

import (
	"fmt"
	"github.com/vc2402/go-script/runtime"
	"go/scanner"
	"go/token"
	"reflect"
	"testing"
)

type funCall struct {
	funcToCall string
	params     []*runtime.Value
	results    []*runtime.Value
}
type compilerFields struct {
	registry *runtime.Registry
	Options  Options
	files    map[string]string
	calls    []funCall
}

func compileAndEngine(fields compilerFields) (*runtime.Engine, error) {
	fileSet := token.NewFileSet()
	var s scanner.Scanner
	var p Project
	for name, file := range fields.files {
		f := fileSet.AddFile(name, fileSet.Base(), len([]byte(file)))
		s.Init(f, []byte(file), nil, 0)
		lex := &GosLex{S: s, Fset: fileSet}
		pr := GosParse(lex)
		if pr != 0 {
			return nil, fmt.Errorf("parse error = %v", lex.Err())
		} else {
			p.AddFile(lex.File)
		}
	}
	c := p.Compiler(fields.registry)
	c.Options = fields.Options
	c.FileSet = fileSet
	err := c.Compile()
	if err != nil {
		return nil, fmt.Errorf("Compile() error = %v", err)
	}

	if err := c.Commit(); err != nil {
		return nil, fmt.Errorf("Commit() error = %v", err)
	}
	eng, err := fields.registry.NewEngine("")
	if err != nil {
		return nil, fmt.Errorf("NewEngine() error = %v", err)
	}
	return eng, nil
}
func TestCompiler(t *testing.T) {

	tests := []struct {
		name    string
		fields  compilerFields
		wantErr bool
	}{
		{
			name: "function call, int to float with implicit conversion",
			fields: compilerFields{
				registry: &runtime.Registry{},
				Options:  Options{AllowImplicitNumbersConversion: true},
				files: map[string]string{
					"test": `
func test(a int) (r1 int, r2 float) {
  return a*10, float(a)/2
}
`,
				},
				calls: []funCall{
					{
						funcToCall: "test",
						params: []*runtime.Value{
							runtime.NewValue(12),
						},
						results: []*runtime.Value{
							runtime.NewValue(120),
							runtime.NewValue(6.0),
						}},
				},
			},
			wantErr: false,
		},
		{
			name: "function call, int to float (implicit conversion off)",
			fields: compilerFields{
				registry: &runtime.Registry{},
				Options:  Options{},
				files: map[string]string{
					"test": `
func test(a int) (r1 int, r2 float) {
  return a*10, float(a)/2
}
`,
				},
				calls: []funCall{
					{
						funcToCall: "test",
						params: []*runtime.Value{
							runtime.NewValue(12),
						},
						results: []*runtime.Value{
							runtime.NewValue(120),
							runtime.NewValue(6.0),
						}},
				},
			},
			wantErr: true,
		},
		{
			name: "function call, if block",
			fields: compilerFields{
				registry: &runtime.Registry{},
				Options:  Options{},
				files: map[string]string{
					"test": `
func test(a int) bool {
  if a > 10 {
    return true
  }
  return false
}
`,
				},
				calls: []funCall{
					{
						funcToCall: "test",
						params: []*runtime.Value{
							runtime.NewValue(12),
						},
						results: []*runtime.Value{
							runtime.NewValue(true),
						},
					},
					{
						funcToCall: "test",
						params: []*runtime.Value{
							runtime.NewValue(10),
						},
						results: []*runtime.Value{
							runtime.NewValue(false),
						},
					},
				},
			},
			wantErr: false,
		},
		{
			name: "arithmetic oper precedence",
			fields: compilerFields{
				registry: &runtime.Registry{},
				Options:  Options{},
				files: map[string]string{
					"test": `
func test(a int) (r1 int, r2 int) {
  return 25+a*10, (25+a)*10
}
`,
				},
				calls: []funCall{
					{
						funcToCall: "test",
						params: []*runtime.Value{
							runtime.NewValue(12),
						},
						results: []*runtime.Value{
							runtime.NewValue(145),
							runtime.NewValue(370),
						}},
				},
			},
			wantErr: false,
		},
		{
			name: "recursive factorial",
			fields: compilerFields{
				registry: &runtime.Registry{},
				Options:  Options{},
				files: map[string]string{
					"test": `
func factorial(val int) int {
  if val > 1 {
    return val * factorial(val-1)
  }
  return 1
}
`,
				},
				calls: []funCall{
					{
						funcToCall: "factorial",
						params: []*runtime.Value{
							runtime.NewValue(14),
						},
						results: []*runtime.Value{
							runtime.NewValue(87178291200),
						}},
				},
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			eng, err := compileAndEngine(tt.fields)
			if err != nil {
				if !tt.wantErr {
					t.Errorf("at '%s': %v, wantErr %v", tt.name, err, tt.wantErr)
				}
				return
			}

			for i, ftc := range tt.fields.calls {
				results, err := eng.Run(ftc.funcToCall, ftc.params)
				if err != nil {
					if !tt.wantErr {
						t.Errorf("call #%d: Run() error = %v, wantErr %v", i, err, tt.wantErr)
					}
					return
				}
				if len(results) != len(ftc.results) {
					t.Errorf("call #%d: got %d results, but expected %d", i, len(results), len(ftc.results))
					return
				}
				for resIdx, res := range results {
					if res.Kind() != ftc.results[resIdx].Kind() {
						t.Errorf("call #%d: result #%d: kinds differ: got %d, but expected %d", i, resIdx, res.Kind(), ftc.results[resIdx].Kind())
						return
					}
					if res.Value() != ftc.results[resIdx].Value() {
						t.Errorf("call #%d: result #%d: values differ: got %v, but expected %v", i, resIdx, res.Value(), ftc.results[resIdx].Value())
						return
					}
				}
			}
		})
	}
}

func BenchmarkFactorial(b *testing.B) {
	test := compilerFields{
		registry: &runtime.Registry{},
		Options:  Options{},
		files: map[string]string{
			"test": `
func factorial(val int) int {
  if val > 1 {
    return val * factorial(val-1)
  }
  return 1
}
`,
		},
		calls: []funCall{
			{
				funcToCall: "factorial",
				params: []*runtime.Value{
					runtime.NewValue(14),
				},
				results: []*runtime.Value{
					runtime.NewValue(87178291200),
				}},
		},
	}
	eng, err := compileAndEngine(test)
	if err != nil {
		b.Errorf("%v", err)
		return
	}
	for i := 0; i < b.N; i++ {
		_, err := eng.Run(test.calls[0].funcToCall, test.calls[0].params)
		if err != nil {
			b.Errorf("call #%d: Run() error = %v", i, err)
			return
		}
	}

}

func TestCompiler_Compile(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	tests := []struct {
		name    string
		fields  fields
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if err := p.Compile(); (err != nil) != tt.wantErr {
				t.Errorf("Compile() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestCompiler_addVar(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s          *scope
		name       string
		descriptor any
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantIdx int
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			gotIdx, err := p.addVar(tt.args.s, tt.args.name, tt.args.descriptor)
			if (err != nil) != tt.wantErr {
				t.Errorf("addVar() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if gotIdx != tt.wantIdx {
				t.Errorf("addVar() gotIdx = %v, want %v", gotIdx, tt.wantIdx)
			}
		})
	}
}

func TestCompiler_checkType(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		tip *typeRef
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if err := p.checkType(tt.args.tip); (err != nil) != tt.wantErr {
				t.Errorf("checkType() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestCompiler_getDebugInfo(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		pos token.Pos
	}
	tests := []struct {
		name   string
		fields fields
		args   args
		want   any
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if got := p.getDebugInfo(tt.args.pos); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("getDebugInfo() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestCompiler_getExtVar(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		pckg string
		name string
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantV   *runtime.Value
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			gotV, err := p.getExtVar(tt.args.pckg, tt.args.name)
			if (err != nil) != tt.wantErr {
				t.Errorf("getExtVar() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(gotV, tt.wantV) {
				t.Errorf("getExtVar() gotV = %v, want %v", gotV, tt.wantV)
			}
		})
	}
}

func TestCompiler_getTypeKind(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		tip *typeRef
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    *runtime.VarDescriptor
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			got, err := p.getTypeKind(tt.args.tip)
			if (err != nil) != tt.wantErr {
				t.Errorf("getTypeKind() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("getTypeKind() got = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestCompiler_isConvertible(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		from *typeRef
		to   *typeRef
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if err := p.isConvertible(tt.args.from, tt.args.to); (err != nil) != tt.wantErr {
				t.Errorf("isConvertible() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestCompiler_pass1(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	tests := []struct {
		name    string
		fields  fields
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if err := p.pass1(); (err != nil) != tt.wantErr {
				t.Errorf("pass1() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestCompiler_pass2(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	tests := []struct {
		name    string
		fields  fields
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if err := p.pass2(); (err != nil) != tt.wantErr {
				t.Errorf("pass2() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestCompiler_processArithmeticExpression(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s     *scope
		left  *expression
		right *expression
		oper  int
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if err := p.processArithmeticExpression(tt.args.s, tt.args.left, tt.args.right, tt.args.oper); (err != nil) != tt.wantErr {
				t.Errorf("processArithmeticExpression() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestCompiler_processAssignment(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s  *scope
		as *assignment
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if err := p.processAssignment(tt.args.s, tt.args.as); (err != nil) != tt.wantErr {
				t.Errorf("processAssignment() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestCompiler_processBlock(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s     *scope
		stmts []*statement
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if err := p.processBlock(tt.args.s, tt.args.stmts); (err != nil) != tt.wantErr {
				t.Errorf("processBlock() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestCompiler_processCompareExpression(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s     *scope
		left  *expression
		right *expression
		oper  int
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if err := p.processCompareExpression(tt.args.s, tt.args.left, tt.args.right, tt.args.oper); (err != nil) != tt.wantErr {
				t.Errorf("processCompareExpression() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestCompiler_processConstExpression(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s *scope
		c *constant
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if err := p.processConstExpression(tt.args.s, tt.args.c); (err != nil) != tt.wantErr {
				t.Errorf("processConstExpression() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestCompiler_processExpression(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s           *scope
		expr        *expression
		isStatement bool
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if err := p.processExpression(tt.args.s, tt.args.expr, tt.args.isStatement); (err != nil) != tt.wantErr {
				t.Errorf("processExpression() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestCompiler_processFor(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s  *scope
		fs *forStatement
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if err := p.processFor(tt.args.s, tt.args.fs); (err != nil) != tt.wantErr {
				t.Errorf("processFor() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestCompiler_processFuncCall(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s        *scope
		fc       *funcCall
		skipRets bool
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if err := p.processFuncCall(tt.args.s, tt.args.fc, tt.args.skipRets); (err != nil) != tt.wantErr {
				t.Errorf("processFuncCall() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestCompiler_processFunction1(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s *scope
		f *function
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    *scope
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			got, err := p.processFunction1(tt.args.s, tt.args.f)
			if (err != nil) != tt.wantErr {
				t.Errorf("processFunction1() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("processFunction1() got = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestCompiler_processFunction2(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		fs *scope
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if err := p.processFunction2(tt.args.fs); (err != nil) != tt.wantErr {
				t.Errorf("processFunction2() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestCompiler_processIf(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s  *scope
		is *ifStatement
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if err := p.processIf(tt.args.s, tt.args.is); (err != nil) != tt.wantErr {
				t.Errorf("processIf() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestCompiler_processIncDec(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s    *scope
		lv   *lvalue
		step int
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if err := p.processIncDec(tt.args.s, tt.args.lv, tt.args.step); (err != nil) != tt.wantErr {
				t.Errorf("processIncDec() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestCompiler_processLValueExpression(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s  *scope
		lv *lvalue
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if err := p.processLValueExpression(tt.args.s, tt.args.lv); (err != nil) != tt.wantErr {
				t.Errorf("processLValueExpression() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestCompiler_processLogicExpression(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s     *scope
		left  *expression
		right *expression
		oper  int
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if err := p.processLogicExpression(tt.args.s, tt.args.left, tt.args.right, tt.args.oper); (err != nil) != tt.wantErr {
				t.Errorf("processLogicExpression() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestCompiler_processMethodCall(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s           *scope
		mc          *methodCall
		skipReturns bool
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if err := p.processMethodCall(tt.args.s, tt.args.mc, tt.args.skipReturns); (err != nil) != tt.wantErr {
				t.Errorf("processMethodCall() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestCompiler_processReturn(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s     *scope
		exprs []*expression
		pos   token.Pos
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if err := p.processReturn(tt.args.s, tt.args.exprs, tt.args.pos); (err != nil) != tt.wantErr {
				t.Errorf("processReturn() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestCompiler_processStatement1(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s    *scope
		stmt *statement
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if err := p.processStatement1(tt.args.s, tt.args.stmt); (err != nil) != tt.wantErr {
				t.Errorf("processStatement1() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestCompiler_processStatement2(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s    *scope
		stmt *statement
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if err := p.processStatement2(tt.args.s, tt.args.stmt); (err != nil) != tt.wantErr {
				t.Errorf("processStatement2() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestCompiler_processVarDef1(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s  *scope
		vd *varDef
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if err := p.processVarDef1(tt.args.s, tt.args.vd); (err != nil) != tt.wantErr {
				t.Errorf("processVarDef1() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestCompiler_processVarDef2(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s  *scope
		vd *varDef
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			if err := p.processVarDef2(tt.args.s, tt.args.vd); (err != nil) != tt.wantErr {
				t.Errorf("processVarDef2() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestCompiler_typeRefFromExpression(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s    *scope
		expr *expression
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    *typeRef
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			got, err := p.typeRefFromExpression(tt.args.s, tt.args.expr)
			if (err != nil) != tt.wantErr {
				t.Errorf("typeRefFromExpression() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("typeRefFromExpression() got = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestCompiler_typeRefFromLvalue(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s  *scope
		lv *lvalue
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    *typeRef
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			got, err := p.typeRefFromLvalue(tt.args.s, tt.args.lv)
			if (err != nil) != tt.wantErr {
				t.Errorf("typeRefFromLvalue() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("typeRefFromLvalue() got = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestCompiler_typeRefToReflectType(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		tr *typeRef
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    reflect.Type
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			got, err := p.typeRefToReflectType(tt.args.tr)
			if (err != nil) != tt.wantErr {
				t.Errorf("typeRefToReflectType() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("typeRefToReflectType() got = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestCompiler_typeRefsFromExpressions(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s           *scope
		expressions []*expression
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    []*typeRef
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			got, err := p.typeRefsFromExpressions(tt.args.s, tt.args.expressions)
			if (err != nil) != tt.wantErr {
				t.Errorf("typeRefsFromExpressions() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("typeRefsFromExpressions() got = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestCompiler_typeRefsFromFuncCall(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s  *scope
		fc *funcCall
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    []*typeRef
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			got, err := p.typeRefsFromFuncCall(tt.args.s, tt.args.fc)
			if (err != nil) != tt.wantErr {
				t.Errorf("typeRefsFromFuncCall() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("typeRefsFromFuncCall() got = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestCompiler_typeRefsFromMethodCall(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s  *scope
		mc *methodCall
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    []*typeRef
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			got, err := p.typeRefsFromMethodCall(tt.args.s, tt.args.mc)
			if (err != nil) != tt.wantErr {
				t.Errorf("typeRefsFromMethodCall() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("typeRefsFromMethodCall() got = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestCompiler_typeRefsFromReflectFuncCall(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s       *scope
		funType reflect.Type
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    []*typeRef
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			got, err := p.typeRefsFromReflectFuncCall(tt.args.s, tt.args.funType)
			if (err != nil) != tt.wantErr {
				t.Errorf("typeRefsFromReflectFuncCall() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("typeRefsFromReflectFuncCall() got = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestCompiler_varDefsToFuncParams(t *testing.T) {
	type fields struct {
		Project   *Project
		registry  *runtime.Registry
		cpackages map[string]cpckg
		Options   Options
	}
	type args struct {
		s       *scope
		varDefs []*varDef
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		want    []runtime.FuncParam
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Compiler{
				Project:   tt.fields.Project,
				registry:  tt.fields.registry,
				cpackages: tt.fields.cpackages,
				Options:   tt.fields.Options,
			}
			got, err := p.varDefsToFuncParams(tt.args.s, tt.args.varDefs)
			if (err != nil) != tt.wantErr {
				t.Errorf("varDefsToFuncParams() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("varDefsToFuncParams() got = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestDebugInfo_String(t *testing.T) {
	type fields struct {
		Position token.Position
		Text     string
	}
	tests := []struct {
		name   string
		fields fields
		want   string
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			di := &DebugInfo{
				Position: tt.fields.Position,
				Text:     tt.fields.Text,
			}
			if got := di.String(); got != tt.want {
				t.Errorf("String() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestError_Error(t *testing.T) {
	type fields struct {
		error error
		pos   token.Pos
	}
	tests := []struct {
		name   string
		fields fields
		want   string
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			e := Error{
				error: tt.fields.error,
				pos:   tt.fields.pos,
			}
			if got := e.Error(); got != tt.want {
				t.Errorf("Error() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestError_Pos(t *testing.T) {
	type fields struct {
		error error
		pos   token.Pos
	}
	tests := []struct {
		name   string
		fields fields
		want   token.Pos
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			e := Error{
				error: tt.fields.error,
				pos:   tt.fields.pos,
			}
			if got := e.Pos(); got != tt.want {
				t.Errorf("Pos() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestError_Unwrap(t *testing.T) {
	type fields struct {
		error error
		pos   token.Pos
	}
	tests := []struct {
		name    string
		fields  fields
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			e := Error{
				error: tt.fields.error,
				pos:   tt.fields.pos,
			}
			if err := e.Unwrap(); (err != nil) != tt.wantErr {
				t.Errorf("Unwrap() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestInternalError_Error(t *testing.T) {
	type fields struct {
		problem string
	}
	tests := []struct {
		name   string
		fields fields
		want   string
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			e := InternalError{
				problem: tt.fields.problem,
			}
			if got := e.Error(); got != tt.want {
				t.Errorf("Error() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestNewError(t *testing.T) {
	type args struct {
		msg string
		pos token.Pos
	}
	tests := []struct {
		name string
		args args
		want Error
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := NewError(tt.args.msg, tt.args.pos); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("NewError() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestNewInternalError(t *testing.T) {
	type args struct {
		msg string
		pos token.Pos
	}
	tests := []struct {
		name string
		args args
		want Error
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := NewInternalError(tt.args.msg, tt.args.pos); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("NewInternalError() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestProject_Compiler(t *testing.T) {
	type fields struct {
		packages map[string]Package
		FileSet  *token.FileSet
	}
	type args struct {
		r *runtime.Registry
	}
	tests := []struct {
		name   string
		fields fields
		args   args
		want   Compiler
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			p := &Project{
				packages: tt.fields.packages,
				FileSet:  tt.fields.FileSet,
			}
			if got := p.Compiler(tt.args.r); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("Compiler() = %v, want %v", got, tt.want)
			}
		})
	}
}

func TestWrapError(t *testing.T) {
	type args struct {
		e   error
		pos token.Pos
	}
	tests := []struct {
		name string
		args args
		want Error
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := WrapError(tt.args.e, tt.args.pos); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("WrapError() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_areComparable(t *testing.T) {
	type args struct {
		t1 string
		t2 string
	}
	tests := []struct {
		name string
		args args
		want bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := areComparable(tt.args.t1, tt.args.t2); got != tt.want {
				t.Errorf("areComparable() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_constant_toTypeRef(t *testing.T) {
	type fields struct {
		kind  int
		value string
		pos   token.Pos
	}
	tests := []struct {
		name    string
		fields  fields
		want    *typeRef
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			c := &constant{
				kind:  tt.fields.kind,
				value: tt.fields.value,
				pos:   tt.fields.pos,
			}
			got, err := c.toTypeRef()
			if (err != nil) != tt.wantErr {
				t.Errorf("toTypeRef() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("toTypeRef() got = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_funcCall_isEmbedded(t *testing.T) {
	type fields struct {
		name        string
		pckg        string
		params      []*expression
		pos         token.Pos
		returnTypes []*typeRef
		convertTo   runtime.VarKind
	}
	tests := []struct {
		name   string
		fields fields
		want   bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			fc := &funcCall{
				name:        tt.fields.name,
				pckg:        tt.fields.pckg,
				params:      tt.fields.params,
				pos:         tt.fields.pos,
				returnTypes: tt.fields.returnTypes,
				convertTo:   tt.fields.convertTo,
			}
			if got := fc.isEmbedded(); got != tt.want {
				t.Errorf("isEmbedded() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_funcCall_isTypeConversion(t *testing.T) {
	type fields struct {
		name        string
		pckg        string
		params      []*expression
		pos         token.Pos
		returnTypes []*typeRef
		convertTo   runtime.VarKind
	}
	tests := []struct {
		name   string
		fields fields
		want   bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			fc := &funcCall{
				name:        tt.fields.name,
				pckg:        tt.fields.pckg,
				params:      tt.fields.params,
				pos:         tt.fields.pos,
				returnTypes: tt.fields.returnTypes,
				convertTo:   tt.fields.convertTo,
			}
			if got := fc.isTypeConversion(); got != tt.want {
				t.Errorf("isTypeConversion() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_getBuiltInTypeKind(t *testing.T) {
	type args struct {
		tn string
	}
	tests := []struct {
		name string
		args args
		want runtime.VarKind
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := getBuiltInTypeKind(tt.args.tn); got != tt.want {
				t.Errorf("getBuiltInTypeKind() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_isNumber(t *testing.T) {
	type args struct {
		t string
	}
	tests := []struct {
		name string
		args args
		want bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := isNumber(tt.args.t); got != tt.want {
				t.Errorf("isNumber() = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_scope_findVar(t *testing.T) {
	type fields struct {
		vars       []cv
		scopes     []*scope
		parent     *scope
		descriptor *runtime.ScopeDescriptor
		source     any
		idx        int
	}
	type args struct {
		name string
	}
	tests := []struct {
		name        string
		fields      fields
		args        args
		wantScopIdx int
		wantIdx     int
		wantCv      *cv
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			s := &scope{
				vars:       tt.fields.vars,
				scopes:     tt.fields.scopes,
				parent:     tt.fields.parent,
				descriptor: tt.fields.descriptor,
				source:     tt.fields.source,
				idx:        tt.fields.idx,
			}
			gotScopIdx, gotIdx, gotCv := s.findVar(tt.args.name)
			if gotScopIdx != tt.wantScopIdx {
				t.Errorf("findVar() gotScopIdx = %v, want %v", gotScopIdx, tt.wantScopIdx)
			}
			if gotIdx != tt.wantIdx {
				t.Errorf("findVar() gotIdx = %v, want %v", gotIdx, tt.wantIdx)
			}
			if !reflect.DeepEqual(gotCv, tt.wantCv) {
				t.Errorf("findVar() gotCv = %v, want %v", gotCv, tt.wantCv)
			}
		})
	}
}

func Test_scope_getVar(t *testing.T) {
	type fields struct {
		vars       []cv
		scopes     []*scope
		parent     *scope
		descriptor *runtime.ScopeDescriptor
		source     any
		idx        int
	}
	type args struct {
		name string
	}
	tests := []struct {
		name    string
		fields  fields
		args    args
		wantIdx int
		wantCv  *cv
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			s := &scope{
				vars:       tt.fields.vars,
				scopes:     tt.fields.scopes,
				parent:     tt.fields.parent,
				descriptor: tt.fields.descriptor,
				source:     tt.fields.source,
				idx:        tt.fields.idx,
			}
			gotIdx, gotCv := s.getVar(tt.args.name)
			if gotIdx != tt.wantIdx {
				t.Errorf("getVar() gotIdx = %v, want %v", gotIdx, tt.wantIdx)
			}
			if !reflect.DeepEqual(gotCv, tt.wantCv) {
				t.Errorf("getVar() gotCv = %v, want %v", gotCv, tt.wantCv)
			}
		})
	}
}

func Test_scope_lookForVar(t *testing.T) {
	type fields struct {
		vars       []cv
		scopes     []*scope
		parent     *scope
		descriptor *runtime.ScopeDescriptor
		source     any
		idx        int
	}
	type args struct {
		sIdx int
		name string
	}
	tests := []struct {
		name        string
		fields      fields
		args        args
		wantScopIdx int
		wantIdx     int
		wantCv      *cv
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			s := &scope{
				vars:       tt.fields.vars,
				scopes:     tt.fields.scopes,
				parent:     tt.fields.parent,
				descriptor: tt.fields.descriptor,
				source:     tt.fields.source,
				idx:        tt.fields.idx,
			}
			gotScopIdx, gotIdx, gotCv := s.lookForVar(tt.args.sIdx, tt.args.name)
			if gotScopIdx != tt.wantScopIdx {
				t.Errorf("lookForVar() gotScopIdx = %v, want %v", gotScopIdx, tt.wantScopIdx)
			}
			if gotIdx != tt.wantIdx {
				t.Errorf("lookForVar() gotIdx = %v, want %v", gotIdx, tt.wantIdx)
			}
			if !reflect.DeepEqual(gotCv, tt.wantCv) {
				t.Errorf("lookForVar() gotCv = %v, want %v", gotCv, tt.wantCv)
			}
		})
	}
}

func Test_varDescriptorForBuiltInType(t *testing.T) {
	type args struct {
		tn string
	}
	tests := []struct {
		name    string
		args    args
		want    *runtime.VarDescriptor
		wantErr bool
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := varDescriptorForBuiltInType(tt.args.tn)
			if (err != nil) != tt.wantErr {
				t.Errorf("varDescriptorForBuiltInType() error = %v, wantErr %v", err, tt.wantErr)
				return
			}
			if !reflect.DeepEqual(got, tt.want) {
				t.Errorf("varDescriptorForBuiltInType() got = %v, want %v", got, tt.want)
			}
		})
	}
}

func Test_varDescriptorToTypeRef(t *testing.T) {
	type args struct {
		vd *runtime.VarDescriptor
	}
	tests := []struct {
		name string
		args args
		want *typeRef
	}{
		// TODO: Add test cases.
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := varDescriptorToTypeRef(tt.args.vd); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("varDescriptorToTypeRef() = %v, want %v", got, tt.want)
			}
		})
	}
}

func BenchmarkGoFactorial(b *testing.B) {
	for i := 0; i < b.N; i++ {
		factorial(14)
	}
}

func factorial(n int) int {
	if n <= 1 {
		return 1
	}
	return n * factorial(n-1)
}
