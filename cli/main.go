package main

import (
	"bufio"
	"errors"
	"fmt"
	"github.com/vc2402/go-script/compiler"
	"github.com/vc2402/go-script/runtime"
	"go/scanner"
	"go/token"
	"io"
	"io/fs"
	"os"
	"path"
	"reflect"
	"strings"
	"time"
)

var fileSet *token.FileSet
var s scanner.Scanner
var reg runtime.Registry
var p compiler.Project
var prevFiles []string
var reader *bufio.Reader

func main() {
	reader = bufio.NewReader(os.Stdin)
	//goscript.GosDebug = 10
	compiler.GosErrorVerbose = true

	initRegistry()

	for {
		if _, err := os.Stdout.WriteString("> "); err != nil {
			fmt.Printf("WriteString: %s", err)
			return
		}
		line, err := reader.ReadBytes('\n')
		if err == io.EOF {
			return
		}
		if err != nil {
			fmt.Printf("ReadBytes: %s", err)
			return
		}
		fields := strings.Fields(string(line))
		if len(fields) > 0 {
			switch fields[0] {
			case "help", "h":
				command := ""
				if len(fields) > 1 {
					command = fields[1]
				}
				help(command)
			case "compile", "c":
				if len(fields) < 2 && len(prevFiles) == 0 {
					help("compile")
					continue
				}
				err = compile(fields[1:]...)
				if err != nil {
					fmt.Println(err.Error())
				}
			case "run", "r":
				run(fields[1:]...)
			case "print", "p":
				printVar(fields[1:]...)
			default:
				fmt.Println("unknown command; use help for list available commands")
			}
		}

	}
}

func help(command string) {
	switch command {
	case "compile":
		fmt.Println("use: compile file1 [file2...]")
	case "run":
		fmt.Println("use: run [package.]function")
	case "print":
		fmt.Println("use: print [package.]varName")
	default:
		fmt.Println("commands: \ncompile\nrun\nprint")
	}
}

func initProject() {
	fileSet = token.NewFileSet()
	p = compiler.Project{}
}

func compile(files ...string) error {
	initProject()
	if len(files) == 0 {
		files = prevFiles
	} else {
		prevFiles = files
	}
	for _, name := range files {

		file, err := os.ReadFile(name)
		if err != nil {
			if errors.Is(err, fs.ErrNotExist) && path.Ext(name) == "" {
				name = name + ".gos"
				file, err = os.ReadFile(name)
			}
			if err != nil {
				return err
			}
		}
		f := fileSet.AddFile(name, fileSet.Base(), len(file))
		s.Init(f, file, nil /* no error handler */, 0 /*scanner.ScanComments*/)
		lex := &compiler.GosLex{S: s, Fset: fileSet}
		pr := compiler.GosParse(lex)
		if pr != 0 {
			initProject()
			fmt.Println(lex.Err())
			return nil
		} else {
			p.AddFile(lex.File)
		}
	}
	c := p.Compiler(&reg)
	c.Options.AllowImplicitNumbersConversion = true
	c.Options.AddDebugInfo = true
	c.FileSet = fileSet
	err := c.Compile()
	if err != nil {
		if e, ok := err.(compiler.Error); ok {
			position := fileSet.Position(e.Pos())
			fmt.Printf("%s: %s\n", position.String(), e.Error())
		} else {
			fmt.Println(err)
		}
		initProject()
		return nil
	}
	err = c.Commit()
	if err != nil {
		fmt.Println(err)
		initProject()
		return nil
	}
	fmt.Println("success")
	return nil
}

var function string

func run(fName ...string) {

	if len(fName) >= 1 {
		function = strings.Trim(fName[0], " \t")
	} else if function == "" {
		help("run")
		return
	}
	names := strings.Split(function, ".")
	pack := ""
	if len(names) > 1 {
		pack = names[0]
		function = names[1]
	}
	eng, err := reg.NewEngine(pack)
	if err != nil {
		fmt.Println(err.Error())
		return
	}
	params, rets, err := eng.DescribeFunction(function)
	if err != nil {
		fmt.Println(err.Error())
		return
	}
	var args []*runtime.Value
	if len(params) > 0 {
		fmt.Println("enter params: ")
		for i, param := range params {
			for {
				fmt.Printf("enter param #%d: (%s:%s): ", i+1, param.Name, param.Descriptor.String())
				pv, err := reader.ReadBytes('\n')
				if err == io.EOF {
					return
				}
				val, err := param.Descriptor.Parse(string(pv[:len(pv)-1]))
				if err != nil {
					fmt.Println(err.Error())
				} else {
					args = append(args, &val)
					break
				}
			}
		}
	}
	results, err := eng.Run(function, args)
	if err != nil {
		fmt.Println(err.Error())
		return
	}
	if len(results) != len(rets) {
		fmt.Printf("\nwarning: got %d results but expecting %d\n", len(results), len(rets))
	}
	if len(results) > 0 {
		fmt.Println("\nresults: ")
		for i, ret := range rets {
			fmt.Printf("%d: %s (%+v)\n", i+1, ret.Name, results[i].String())
		}
	}
}

var prevVars []string

func printVar(vars ...string) {
	if len(vars) == 0 {
		vars = prevVars
	} else {
		prevVars = vars
	}

	for _, vn := range vars {
		names := strings.Split(vn, ".")
		pack := ""
		name := vn
		if len(names) > 1 {
			pack = names[0]
			name = names[1]
		}
		eng, err := reg.NewEngine(pack)
		if err != nil {
			fmt.Println(err.Error())
			return
		}
		v, err := eng.Var(name)
		if err != nil {
			fmt.Printf("%s: %s\n", name, err.Error())
		} else {
			fmt.Printf("%s: %s\n", name, v.String())
		}
	}
	fmt.Println()
}

func initRegistry() {
	reg.RegisterFunction(fmt.Println)
	reg.RegisterFunction(fmt.Sprintf)
	reg.RegisterReflectType(reflect.TypeOf(time.Time{}))
	reg.RegisterFunction(time.Now)
	reg.RegisterFunction(time.Parse)

	reg.RegisterReflectValue(
		"test",
		"TestObject",
		reflect.ValueOf(&TestType{StringValue: "Hi from test type", MapValue: map[string]string{"a": "aaa"}}))
	reg.RegisterReflectType(reflect.TypeOf(Subtype{}))
	//tt := reflect.TypeOf(time.Time{})
	//for i := 0; i < tt.NumMethod(); i++ {
	//	fmt.Println(tt.Method(i))
	//}
}

type TestType struct {
	StringValue string
	MapValue    map[string]string
}

func (tt *TestType) Print() {
	fmt.Println("TestType: ", tt.StringValue)
}

func (tt *TestType) New() *Subtype {
	return &Subtype{parent: tt, Time: time.Now()}
}

type Subtype struct {
	parent *TestType
	Value  int
	Time   time.Time
}

func (st *Subtype) Parent() (*TestType, error) {
	if st.parent != nil {
		return st.parent, nil
	}
	return nil, errors.New("parent is not defined")
}

func (st *Subtype) Unbind() {
	st.parent = nil
}
