package runtime

import (
	"errors"
	"fmt"
	"reflect"
	"strconv"
	"strings"
)

type VarKind int

const (
	VKUndef VarKind = iota
	VKInt
	VKFloat
	VKBool
	VKChar
	VKString
	VKAny
	VKError
	VKSlice
	VKMap
	VKFunc
	VKRef
	VKAlias
	VKExternal
)

type Int int

type Float float64

var varKinds = [...]string{
	"undef",
	"int",
	"float",
	"bool",
	"char",
	"string",
	"any",
	"error",
	"slice",
	"map",
	"func",
	"ref",
	"alias",
	"reflect",
}

type VarRef struct {
	pckg     string
	name     string
	scopeIdx int
	idx      int
	ref      *Value
	scope    *Scope
}

//TODO create typeDescriptor for script-defined types
// TODO create types registry?

type VarDescriptor struct {
	name string
	kind VarKind
	pckg string
	// for script-defined maps and slices/arrays
	elemType *VarDescriptor
	keyType  *VarDescriptor
	init     any
}

type Value struct {
	*VarDescriptor
	val any
}

func (eng *Engine) newValue(d VarDescriptor) Value {
	v := Value{VarDescriptor: &d}
	if d.init != nil {
		if d.kind == VKExternal {
			switch init := d.init.(type) {
			case reflect.Type:
				v.val = reflect.Zero(init)
			case reflect.Value:
				v.val = init
			default:
				panic("undefined init value")
			}
		} else {
			v.val = d.init
		}
		return v
	}
	switch d.kind {
	case VKInt:
		v.val = Int(0)
	case VKFloat:
		v.val = Float(0)
	case VKBool:
		v.val = false
	case VKString:
		v.val = ""
	case VKChar:
		v.val = rune(0)
	case VKMap:
		v.val = map[string]*Value(nil)
	case VKSlice:
		v.val = []*Value(nil)
	case VKExternal:
		t, err := eng.Registry.GetType(d.pckg, d.name)
		if err != nil {
			panic(err.Error())
		}
		v.val = reflect.Zero(t)
	case VKRef:
		if d.pckg != "" && d.name != "" {
			v.val = &VarRef{pckg: d.pckg, name: d.name}
		}
	}
	return v
}

func (eng *Engine) assign(v *Value, newVal *Value) {
	v = v.deref(eng)
	newVal = newVal.deref(eng)
	if v.kind == newVal.kind {
		if v.kind != VKExternal {
			v.val = newVal.val
			return
		}
		vv, vok := v.val.(reflect.Value)
		nv, nok := newVal.val.(reflect.Value)
		if !vok || !nok {
			panic("assign: can't get reflect value")
		}
		if vv.CanSet() {
			vv.Set(nv)
		} else {
			v.val = newVal.val
		}
		return
	}
	if v.kind == VKExternal {
		v.assignReflect(newVal)
		return
	} else if newVal.kind == VKExternal {
		if rv, ok := newVal.val.(reflect.Value); ok {
			v.assignFromReflect(rv)
			return
		} else {
		}
		panic("assign: not reflect.Value for external type")
	}
	panic(fmt.Sprintf("assign: attempt to assign %s to %s", varKinds[newVal.kind], varKinds[v.kind]))
}

func (v *Value) deref(eng *Engine) *Value {
	if v.kind != VKRef {
		return v
	}
	if vv, ok := v.val.(*Value); ok {
		return vv.deref(eng)
	}
	if vr, ok := v.val.(*VarRef); ok {
		if vr.ref != nil {
			return vr.ref
		}
		var varr *Value
		if vr.pckg != "" && vr.name != "" {
			p, e := eng.Registry.GetPackage(vr.pckg)
			if e != nil {
				panic(e.Error())
			}
			varr = p.GetVarByName(vr.name)
			vr.scope = &p.Scope
		} else {
			vr.scope = eng.scopeAt(vr.scopeIdx)
			varr = vr.scope.getVar(vr.idx)
		}
		vr.ref = varr
		return varr.deref(eng)
	}
	return v
}

func (v *Value) getEffectiveScope() *Scope {
	if v.kind == VKRef {
		if vr, ok := v.val.(*VarRef); ok {
			return vr.scope
		}
	}
	return nil
}

func (v *Value) assignReflect(newVal *Value) {
	v.val = newVal.toReflect()
}

func (v *Value) assignFromReflect(newVal reflect.Value) {
	if newVal.IsNil() {
		v.val = nil
		return
	}
	switch v.kind {
	case VKInt:
		if newVal.CanInt() {
			v.val = Int(newVal.Int())
		} else if newVal.CanUint() {
			v.val = Int(newVal.Uint())
		} else if newVal.CanFloat() {
			v.val = Int(newVal.Float())
		} else {
			break
		}
		return
	case VKFloat:
		if newVal.CanInt() {
			v.val = Float(newVal.Int())
		} else if newVal.CanUint() {
			v.val = Float(newVal.Uint())
		} else if newVal.CanFloat() {
			v.val = Float(newVal.Float())
		} else {
			break
		}
		return
	case VKBool:
		// maybe cast to bool from int and string?
		if newVal.Type().Kind() == reflect.Bool {
			v.val = newVal.Bool()
			return
		}
	case VKString:
		if newVal.Type().Kind() == reflect.String {
			v.val = newVal.String()
			return
		}
	case VKError:
		if newVal.Type().Kind() == reflect.Interface {
			if err, ok := newVal.Interface().(error); ok {
				v.val = err
				return
			}
		}
	}
	panic(fmt.Sprintf("assignFromReflect: cannot assign from %s to %s", newVal.String(), varKinds[v.kind]))
}

func (eng *Engine) isNil(v *Value) bool {
	v = v.deref(eng)
	if v.kind != VKExternal {
		return v.val == nil
	}
	val := v.val.(reflect.Value)
	return val.IsZero() || val.IsNil()
}

func (eng *Engine) sum(v *Value, o *Value) (ret *Value) {
	v = v.deref(eng)
	o = o.deref(eng)
	if o.kind == v.kind {
		ret = &Value{VarDescriptor: o.VarDescriptor}
		switch v.kind {
		case VKInt:
			ret.val = v.val.(Int) + o.val.(Int)
		case VKFloat:
			ret.val = v.val.(Float) + o.val.(Float)
		case VKString:
			ret.val = v.val.(string) + o.val.(string)
		}
		return
	}
	panic(fmt.Sprintf("sum is called for %s and %s", varKinds[v.kind], varKinds[o.kind]))
}

func (eng *Engine) sub(v *Value, o *Value) (ret *Value) {
	v = v.deref(eng)
	o = o.deref(eng)
	if o.kind == v.kind {
		ret = &Value{VarDescriptor: o.VarDescriptor}
		switch v.kind {
		case VKInt:
			ret.val = v.val.(Int) - o.val.(Int)
		case VKFloat:
			ret.val = v.val.(Float) - o.val.(Float)
		}
		return
	}
	panic(fmt.Sprintf("sub is called for %s and %s", varKinds[v.kind], varKinds[o.kind]))
}

func (eng *Engine) mul(v *Value, o *Value) (ret *Value) {
	v = v.deref(eng)
	o = o.deref(eng)
	if o.kind == v.kind {
		ret = &Value{VarDescriptor: o.VarDescriptor}
		switch v.kind {
		case VKInt:
			ret.val = v.val.(Int) * o.val.(Int)
		case VKFloat:
			ret.val = v.val.(Float) * o.val.(Float)
		}
		return
	}
	panic(fmt.Sprintf("sub is called for %s and %s", varKinds[v.kind], varKinds[o.kind]))
}

func (eng *Engine) div(v *Value, o *Value) (ret *Value) {
	v = v.deref(eng)
	o = o.deref(eng)
	if o.kind == v.kind {
		ret = &Value{VarDescriptor: o.VarDescriptor}
		switch v.kind {
		case VKInt:
			ret.val = v.val.(Int) / o.val.(Int)
		case VKFloat:
			ret.val = v.val.(Float) / o.val.(Float)
		}
		return
	}
	panic(fmt.Sprintf("sub is called for %s and %s", varKinds[v.kind], varKinds[o.kind]))
}

func (eng *Engine) inc(v *Value) (ret *Value) {
	v = v.deref(eng)
	ret = &Value{VarDescriptor: v.VarDescriptor}
	switch v.kind {
	case VKInt:
		ret.val = v.val.(Int) + 1
	case VKFloat:
		ret.val = v.val.(Float) + 1
	default:
		ret.val = v.getInt() + 1
	}
	return
}

func (eng *Engine) dec(v *Value) (ret *Value) {
	v = v.deref(eng)
	ret = &Value{VarDescriptor: v.VarDescriptor}
	switch v.kind {
	case VKInt:
		ret.val = v.val.(Int) - 1
	case VKFloat:
		ret.val = v.val.(Float) - 1
	default:
		ret.val = v.getInt() - 1
	}
	return
}

func (eng *Engine) and(v *Value, o *Value) (ret *Value) {
	v = v.deref(eng)
	o = o.deref(eng)
	ret = &Value{VarDescriptor: &VarDescriptor{kind: VKBool}, val: v.getBool() && o.getBool()}
	return
}

func (eng *Engine) or(v *Value, o *Value) (ret *Value) {
	v = v.deref(eng)
	o = o.deref(eng)
	ret = &Value{VarDescriptor: &VarDescriptor{kind: VKBool}, val: v.getBool() || o.getBool()}
	return
}

func (eng *Engine) eq(v *Value, o *Value) (ret *Value) {
	v = v.deref(eng)
	o = o.deref(eng)
	ret = &Value{VarDescriptor: &VarDescriptor{kind: VKBool}, val: eng.compare(v, o) == 0}
	return
}

func (eng *Engine) ne(v *Value, o *Value) (ret *Value) {
	v = v.deref(eng)
	o = o.deref(eng)
	ret = &Value{VarDescriptor: &VarDescriptor{kind: VKBool}, val: eng.compare(v, o) != 0}
	return
}

func (eng *Engine) gt(v *Value, o *Value) (ret *Value) {
	v = v.deref(eng)
	o = o.deref(eng)
	ret = &Value{VarDescriptor: &VarDescriptor{kind: VKBool}, val: eng.compare(v, o) > 0}
	return
}

func (eng *Engine) lt(v *Value, o *Value) (ret *Value) {
	v = v.deref(eng)
	o = o.deref(eng)
	ret = &Value{VarDescriptor: &VarDescriptor{kind: VKBool}, val: eng.compare(v, o) < 0}
	return
}

func (eng *Engine) gte(v *Value, o *Value) (ret *Value) {
	v = v.deref(eng)
	o = o.deref(eng)
	ret = &Value{VarDescriptor: &VarDescriptor{kind: VKBool}, val: eng.compare(v, o) >= 0}
	return
}

func (eng *Engine) lte(v *Value, o *Value) (ret *Value) {
	v = v.deref(eng)
	o = o.deref(eng)
	ret = &Value{VarDescriptor: &VarDescriptor{kind: VKBool}, val: eng.compare(v, o) <= 0}
	return
}

func (eng *Engine) not(v *Value) (ret *Value) {
	v = v.deref(eng)
	ret = &Value{VarDescriptor: &VarDescriptor{kind: VKBool}, val: !v.getBool()}
	return
}

func (eng *Engine) index(v *Value, index *Value) (ret *Value) {
	v = v.deref(eng)
	ok := v.isSlice()
	if !ok {
		panic("index for non slice")
	}
	idx := index.getInt()
	if v.kind == VKSlice {
		sl := v.val.([]*Value)
		if len(sl) <= int(idx) {
			panic("index out of range")
		}
		return sl[idx]
	}
	val := reflect.Indirect(v.val.(reflect.Value))
	ret = &Value{VarDescriptor: &VarDescriptor{kind: VKExternal}, val: val.Index(int(idx))}
	return
}

func (eng *Engine) setMapValue(m *Value, key *Value, val *Value) {
	m = m.deref(eng)
	ok := m.isMap()
	if !ok {
		panic("setMapValue for non map")
	}
	if m.kind == VKMap {
		k := key.getString()
		m.val.(map[string]*Value)[k] = val
		return
	}
	rm := reflect.Indirect(m.val.(reflect.Value))
	key = key.deref(eng)
	val = val.deref(eng)
	rm.SetMapIndex(key.toReflect(), val.toReflect())
}

func (eng *Engine) mapValue(m *Value, key *Value) (val *Value) {
	m = m.deref(eng)
	ok := m.isMap()
	if !ok {
		panic("mapValue for non map")
	}
	if m.kind == VKMap {
		k := key.getString()
		val = m.val.(map[string]*Value)[k]
		return
	}
	rm := reflect.Indirect(m.val.(reflect.Value))
	key = key.deref(eng)
	val = &Value{VarDescriptor: &VarDescriptor{kind: VKExternal}, val: rm.MapIndex(key.toReflect())}
	return
}

func (eng *Engine) dot(v *Value, field *Value) (ret *Value) {
	v = v.deref(eng)
	//TODO add support of script-defined structs
	if v.kind != VKExternal || reflect.Indirect(v.val.(reflect.Value)).Type().Kind() != reflect.Struct {
		panic("dot for non struct")
	}
	val := reflect.Indirect(v.val.(reflect.Value))
	ret = &Value{VarDescriptor: &VarDescriptor{kind: VKExternal}, val: val.FieldByName(field.getString())}
	return
}

func (eng *Engine) convert(v *Value, kind VarKind) (ret *Value) {
	v = v.deref(eng)
	if v.kind == kind {
		return v
	}
	ret = &Value{VarDescriptor: &VarDescriptor{kind: kind}}
	switch kind {
	case VKInt:
		ret.val = v.getInt()
	case VKFloat:
		ret.val = v.getFloat()
	case VKBool:
		ret.val = v.getBool()
	case VKChar:
		panic("convert to char not implemented")
	case VKString:
		ret.val = v.getString()
	default:
		panic(fmt.Sprintf("impossible conversion from %s to %s", varKinds[v.kind], varKinds[kind]))
	}
	return
}

func (eng *Engine) compare(v *Value, to *Value) int {
	v = v.deref(eng)
	to = to.deref(eng)
	ln := eng.isNil(v)
	rn := eng.isNil(to)
	if ln && rn {
		return 0
	}
	if ln || rn {
		return 1
	}
	var comp int
	switch v.kind {
	case VKInt:
		comp = int(v.getInt() - to.getInt())
	case VKFloat:
		comp = int(v.getFloat() - to.getFloat())
	case VKBool:
		l := v.getBool()
		r := to.getBool()
		if l == r {
			comp = 0
		} else if !l {
			comp = -1
		} else {
			comp = 1
		}
	case VKChar:
		panic("compare for char not implemented")
	case VKString:
		comp = strings.Compare(v.getString(), to.getString())
	default:
		panic(fmt.Sprintf("impossible conversion from %s to %s", varKinds[v.kind], varKinds[to.kind]))
	}
	return comp
}

func (v *Value) getInt() Int {
	switch v.kind {
	case VKInt:
		return v.val.(Int)
	case VKFloat:
		return Int(v.val.(Float))
	case VKExternal:
		if val, ok := v.val.(reflect.Value); ok {
			if val.CanInt() {
				return Int(val.Int())
			} else if val.CanUint() {
				return Int(val.Uint())
			} else if val.CanFloat() {
				return Int(val.Float())
			}
			panic(fmt.Sprintf("getInt for reflect.Value %T (%v)", val.Interface(), val.Interface()))
		}
	}
	panic(fmt.Sprintf("getInt for %+v", *v))
}

func (v *Value) getFloat() Float {
	switch v.kind {
	case VKInt:
		return Float(v.val.(Int))
	case VKFloat:
		return v.val.(Float)
	case VKExternal:
		if val, ok := v.val.(reflect.Value); ok {
			if val.CanInt() {
				return Float(val.Int())
			} else if val.CanUint() {
				return Float(val.Uint())
			} else if val.CanFloat() {
				return Float(val.Float())
			}
			panic(fmt.Sprintf("getFloat for reflect.Value %T (%v)", val.Interface(), val.Interface()))
		}
	}
	panic(fmt.Sprintf("getFloat for %+v", *v))
}

func (v *Value) getString() string {
	switch v.kind {
	case VKString:
		return v.val.(string)
	case VKInt:
		return strconv.Itoa(int(v.val.(Int)))
	case VKFloat:
		return strconv.FormatFloat(float64(v.val.(Float)), 'f', 2, 64)
	case VKExternal:
		if val, ok := v.val.(reflect.Value); ok {
			if val.CanInt() {
				return strconv.FormatInt(val.Int(), 10)
			} else if val.CanUint() {
				return strconv.FormatInt(int64(val.Uint()), 10)
			} else if val.CanFloat() {
				return strconv.FormatFloat(val.Float(), 'f', 2, 64)
			} else if val.Type().Kind() == reflect.String {
				return val.String()
			}
			panic(fmt.Sprintf("getString for reflect.Value %T (%v)", val.Interface(), val.Interface()))
		}
	}
	panic(fmt.Sprintf("getString for %+v", *v))
}

func (v *Value) String() string {
	switch v.kind {
	case VKString:
		return v.val.(string)
	case VKInt:
		return strconv.Itoa(int(v.val.(Int)))
	case VKFloat:
		return strconv.FormatFloat(float64(v.val.(Float)), 'f', 2, 64)
	case VKBool:
		return strconv.FormatBool(v.val.(bool))
	default:
		return fmt.Sprintf("%+v", v.val)
	}
}

func (v *Value) getBool() bool {
	switch v.kind {
	case VKBool:
		return v.val.(bool)
	// let's convert others to bool
	//TODO use option for this?
	case VKInt:
		return v.val.(Int) != 0
	case VKFloat:
		return v.val.(Float) != 0
	case VKString:
		return v.val.(string) != ""
	case VKExternal:
		if val, ok := v.val.(reflect.Value); ok {
			if val.Type().Kind() == reflect.Bool {
				return val.Bool()
			} else if val.CanInt() {
				return val.Int() != 0
			} else if val.CanUint() {
				return val.Uint() != 0
			} else if val.CanFloat() {
				return val.Float() != 0
			} else if val.Type().Kind() == reflect.String {
				return val.String() != ""
			}
			panic(fmt.Sprintf("getBool for reflect.Value %T (%v)", val.Interface(), val.Interface()))
		}
	}
	panic(fmt.Sprintf("getBool for %+v", *v))
}

func (v *Value) isSlice() bool {
	ok := v.kind == VKSlice

	if !ok && v.kind == VKExternal {
		val := reflect.Indirect(v.val.(reflect.Value))
		k := val.Type().Kind()
		ok = k == reflect.Array || k == reflect.Slice
	}
	return ok
}

func (v *Value) isMap() bool {
	ok := v.kind == VKMap

	if !ok && v.kind == VKExternal {
		val := reflect.Indirect(v.val.(reflect.Value))
		ok = val.Type().Kind() == reflect.Map
	}
	return ok
}

func (v *Value) toReflect() reflect.Value {
	switch v.kind {
	case VKExternal:
		return v.val.(reflect.Value)
	case VKInt:
		return reflect.ValueOf(int(v.val.(Int)))
	case VKFloat:
		return reflect.ValueOf(float64(v.val.(Float)))
	case VKString:
		return reflect.ValueOf(v.val.(string))
	case VKBool:
		return reflect.ValueOf(v.val.(bool))
	case VKError:
		return reflect.ValueOf(v.val.(error))
	}
	panic(fmt.Sprintf("cannot convert to reflect.Value: %+v", *v))
}

func (v *Value) Reflect() reflect.Value {
	if v.kind == VKExternal {
		return v.val.(reflect.Value)
	}
	panic("not a reflect value")
}

func (v *Value) GetFunction() any {
	if v.kind == VKFunc {
		return v.val
	}
	if v.kind == VKExternal && reflect.Indirect(v.val.(reflect.Value)).Type().Kind() == reflect.Func {
		return reflect.Indirect(v.val.(reflect.Value))
	}
	panic("GetFunction: var is not a function")
}

func (v *Value) IsFunction() bool {
	return v.kind == VKFunc ||
		v.kind == VKExternal && reflect.Indirect(v.val.(reflect.Value)).Type().Kind() == reflect.Func
}

func NewVarDescriptor(
	kind VarKind,
	pckg string,
	name string,
	elemAndKey ...*VarDescriptor,
) *VarDescriptor {
	vd := &VarDescriptor{
		kind: kind,
		pckg: pckg,
		name: name,
	}
	// for script-defined maps and slices/arrays
	if len(elemAndKey) > 0 {
		vd.elemType = elemAndKey[0]
	}
	if len(elemAndKey) > 1 {
		vd.keyType = elemAndKey[1]
	}
	return vd
}

func NewRefVarDescriptor(
	pckg string,
	name string,
) *VarDescriptor {
	return &VarDescriptor{
		kind: VKRef,
		pckg: pckg,
		name: name,
	}
}

func NewDescriptorFromReflect(t reflect.Type) (*VarDescriptor, error) {
	d := &VarDescriptor{}
	switch t.Kind() {
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64,
		reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
		d.kind = VKInt
		d.name = "int"
	case reflect.Float32, reflect.Float64:
		d.kind = VKFloat
		d.name = "float"
	case reflect.Interface:
		if t.Name() == "error" {
			d.kind = VKError
			d.name = "error"

		} else {
			d.kind = VKAny
			d.name = "any"
		}
	case reflect.Bool:
		d.kind = VKBool
		d.name = "bool"
	//TODO add support for maps, slices and arrays
	case reflect.Pointer:
		return NewDescriptorFromReflect(t.Elem())
	default:
		if t.Name() != "" {
			//TODO resolve alias by path
			d.pckg = t.PkgPath()
			d.name = t.Name()
			d.kind = VKExternal
		} else {
			return nil, fmt.Errorf("reflect kind %s is not supported", t.Kind())
		}
	}
	return d, nil
}

func (vd *VarDescriptor) SetName(name string) *VarDescriptor { vd.name = name; return vd }

func (vd *VarDescriptor) SetInitValue(initValue any) *VarDescriptor {
	vd.init = initValue
	return vd
}

func (vd *VarDescriptor) SetIntInitValue(initValue int) *VarDescriptor {
	vd.init = Int(initValue)
	vd.kind = VKInt
	return vd
}

func (vd *VarDescriptor) SetFloatInitValue(initValue float64) *VarDescriptor {
	vd.init = Float(initValue)
	vd.kind = VKFloat
	return vd
}

func (vd *VarDescriptor) SetBoolInitValue(initValue bool) *VarDescriptor {
	vd.init = initValue
	vd.kind = VKBool
	return vd
}

func (vd *VarDescriptor) SetStringInitValue(initValue string) *VarDescriptor {
	vd.init = initValue
	vd.kind = VKString
	return vd
}

func (vd *VarDescriptor) SetReflectInitValue(initValue reflect.Value) *VarDescriptor {
	vd.init = initValue
	vd.kind = VKExternal
	return vd
}

func (vd *VarDescriptor) Name() string { return vd.name }

func (vd *VarDescriptor) Kind() VarKind { return vd.kind }

func (vd *VarDescriptor) Package() string { return vd.pckg }

func (vd *VarDescriptor) Elem() *VarDescriptor { return vd.elemType }

func (vd *VarDescriptor) Key() *VarDescriptor { return vd.keyType }

func (vd *VarDescriptor) String() string {
	if vd.kind <= VKError {
		return varKinds[vd.kind]
	}
	switch vd.kind {
	case VKSlice:
		return fmt.Sprintf("[]%s", vd.elemType.String())
	case VKMap:
		return fmt.Sprintf("map[%s]%s", vd.keyType.String(), vd.elemType.String())
	case VKFunc:
		return "function"
	case VKRef:
		return fmt.Sprintf("ref to %s.%s", vd.pckg, vd.name)
	case VKAlias:
		return "alias"
	case VKExternal:
		return fmt.Sprintf("external: %s.%s", vd.pckg, vd.name)
	}
	return "undef"
}

func (vd *VarDescriptor) Parse(v string) (val Value, err error) {
	val = Value{VarDescriptor: vd}
	switch vd.kind {
	case VKInt:
		var iv int64
		iv, err = strconv.ParseInt(v, 0, 64)
		val.val = Int(iv)
	case VKFloat:
		var fv float64
		fv, err = strconv.ParseFloat(v, 64)
		val.val = Float(fv)
	case VKString:
		val.val = v
	case VKBool:
		switch v {
		case "true":
			val.val = true
		case "false":
			val.val = false
		default:
			err = errors.New("invalid value for boolean")
		}
	default:
		err = errors.New("unsupported type")
	}
	return
}

func (vd *VarDescriptor) IsAssignable(tip reflect.Type, reg *Registry) bool {
	if tip.Kind() == reflect.Interface {
		return true
	}
	switch vd.kind {
	case VKInt:
		return tip.AssignableTo(reflect.TypeOf(0))
	case VKFloat:
		return tip.AssignableTo(reflect.TypeOf(0.0))
	case VKBool:
		return tip.AssignableTo(reflect.TypeOf(true))
	case VKString:
		return tip.AssignableTo(reflect.TypeOf(""))
	case VKExternal:
		extType, err := reg.GetType(vd.pckg, vd.name)
		if err != nil {
			return false
		}
		return tip.AssignableTo(extType)
	}
	//TODO check for other types
	return false
}
