package runtime

import (
	"errors"
	"reflect"
	"runtime"
	"strings"
)

var ErrPackageNotFound = errors.New("package not found")
var ErrDuplicateAlias = errors.New("package alias already in use")

type Registry struct {
	packages map[string]*Package
}

func (r *Registry) NewEngine(alias string) (*Engine, error) {
	p, err := r.GetPackage(alias)
	if err != nil {
		return nil, err
	}
	eng := &Engine{
		scope:    ExecutionScope{&p.Scope},
		stack:    nil,
		Registry: r,
		options:  EngineOptions{},
	}
	return eng, nil
}

func (r *Registry) GetPackage(alias string) (*Package, error) {
	if p, ok := r.packages[alias]; ok {
		return p, nil
	}
	return nil, ErrPackageNotFound
}

func (r *Registry) GetType(pckg, name string) (reflect.Type, error) {
	p, err := r.GetPackage(pckg)
	if err != nil {
		return reflect.TypeOf(err), err
	}
	return p.GetReflectType(name)
}

// Package returns reference to Package; creates it if necessary
func (r *Registry) Package(path string, alias string) (*Package, error) {
	if alias == "" {
		idx := strings.LastIndex(path, "/")
		if idx != -1 {
			alias = path[idx+1:]
		} else {
			alias = path
		}
	}
	if p, ok := r.packages[alias]; ok {
		if p.path != path {
			return nil, ErrDuplicateAlias
		}
		return p, nil
	}
	p := &Package{
		Scope: Scope{
			ScopeDescriptor: &ScopeDescriptor{
				Kind: SKTop,
			},
			vars: nil,
		},
		types: map[string]reflect.Type{},
		path:  path,
		alias: alias,
	}
	if r.packages == nil {
		r.packages = map[string]*Package{}
	}
	r.packages[alias] = p
	return p, nil
}

// SetTopScope finds or creates package and creates top scope
func (r *Registry) SetTopScope(path string, alias string, sd *ScopeDescriptor) (*Package, error) {
	p, err := r.Package(path, alias)
	if err != nil {
		return nil, err
	}
	eng := Engine{Registry: r}
	scope := createNewScope(&eng, sd)
	eng.scope = ExecutionScope{scope}
	eng.exec()
	p.Scope = *scope

	return p, nil
}
func (r *Registry) RegisterReflectType(tip reflect.Type) error {
	packageName := tip.PkgPath()
	p, err := r.Package(packageName, "")
	if err != nil {
		return err
	}
	return p.RegisterReflectType(tip)
}

func (r *Registry) RegisterReflectValue(packageName, name string, val reflect.Value) error {
	p, err := r.Package(packageName, "")
	if err != nil {
		return err
	}
	return p.RegisterReflectValue(name, val)
}

func (r *Registry) RegisterFunction(fun any) error {
	val := reflect.ValueOf(fun)
	rtf := runtime.FuncForPC(val.Pointer())
	fullName := rtf.Name()
	nameIdx := strings.LastIndex(fullName, ".")
	if nameIdx == -1 {
		return errors.New("cannot get function name; use RegisterReflectValue instead")
	}
	packageName := fullName[:nameIdx]
	p, err := r.Package(packageName, "")
	if err != nil {
		return err
	}
	return p.RegisterReflectValue(fullName[nameIdx+1:], val)
}
