package runtime

import (
	"reflect"
	"testing"
)

func TestEngine_next(t *testing.T) {
	type fields struct {
		scope    ExecutionScope
		stack    []*Value
		Registry *Registry
		options  EngineOptions
	}
	tests := []struct {
		name   string
		fields fields
		want   any
	}{
		{
			name: "two ints sum",
			fields: fields{
				scope: []*Scope{
					{
						ScopeDescriptor: &ScopeDescriptor{
							Kind: SKTop,
							Program: []Operation{
								{class: CKALU, command: AlucAdd},
							},
						},
					},
				},
				stack: []*Value{
					{&VarDescriptor{kind: VKInt}, Int(3)},
					{&VarDescriptor{kind: VKInt}, Int(5)},
				},
			},
			want: Int(8),
		},
		{
			name: "two ints sub",
			fields: fields{
				scope: []*Scope{
					{
						ScopeDescriptor: &ScopeDescriptor{
							Kind: SKTop,
							Program: []Operation{
								{class: CKALU, command: AlucSub},
							},
						},
					},
				},
				stack: []*Value{
					{&VarDescriptor{kind: VKInt}, Int(13)},
					{&VarDescriptor{kind: VKInt}, Int(5)},
				},
			},
			want: Int(8),
		},
		{
			name: "two floats sum",
			fields: fields{
				scope: []*Scope{
					{
						ScopeDescriptor: &ScopeDescriptor{
							Kind: 3,
							Program: []Operation{
								{class: CKALU, command: AlucAdd},
							},
						},
					},
				},
				stack: []*Value{
					{&VarDescriptor{kind: VKFloat}, Float(3.4)},
					{&VarDescriptor{kind: VKFloat}, Float(5.2)},
				},
			},
			want: Float(8.6),
		},
		{
			name: "two ints multiplication",
			fields: fields{
				scope: []*Scope{
					{
						ScopeDescriptor: &ScopeDescriptor{
							Kind: 3,
							Program: []Operation{
								{class: CKALU, command: AlucMul},
							},
						},
					},
				},
				stack: []*Value{
					{&VarDescriptor{kind: VKInt}, Int(3)},
					{&VarDescriptor{kind: VKInt}, Int(2)},
				},
			},
			want: Int(6),
		},
		{
			name: "and between bools",
			fields: fields{
				scope: []*Scope{
					{
						ScopeDescriptor: &ScopeDescriptor{
							Kind: 3,
							Program: []Operation{
								{class: CKALU, command: AlucAnd},
							},
						},
					},
				},
				stack: []*Value{
					{&VarDescriptor{kind: VKBool}, true},
					{&VarDescriptor{kind: VKBool}, true},
				},
			},
			want: true,
		},
		{
			name: "and between bool and int (conversion)",
			fields: fields{
				scope: []*Scope{
					{
						ScopeDescriptor: &ScopeDescriptor{
							Kind: 3,
							Program: []Operation{
								{class: CKALU, command: AlucAnd},
							},
						},
					},
				},
				stack: []*Value{
					{&VarDescriptor{kind: VKBool}, true},
					{&VarDescriptor{kind: VKInt}, Int(0)},
				},
			},
			want: false,
		},
		{
			name: "greater than",
			fields: fields{
				scope: []*Scope{
					{
						ScopeDescriptor: &ScopeDescriptor{
							Kind: 3,
							Program: []Operation{
								{class: CKALU, command: AlucGreaterThan},
							},
						},
					},
				},
				stack: []*Value{
					{&VarDescriptor{kind: VKInt}, Int(10)},
					{&VarDescriptor{kind: VKInt}, Int(0)},
				},
			},
			want: true,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			eng := &Engine{
				scope:    tt.fields.scope,
				stack:    tt.fields.stack,
				Registry: tt.fields.Registry,
				options:  tt.fields.options,
			}
			got := eng.next()
			if got {
				stackTop := eng.stackItem(0)
				if stackTop.val != tt.want {
					t.Errorf("next() got = %v, want %v", stackTop.val, tt.want)
				}
			}
		})
	}
}

func TestEngine_exec(t *testing.T) {
	type fields struct {
		scope    ExecutionScope
		stack    []*Value
		Registry *Registry
		options  EngineOptions
	}
	tests := []struct {
		name    string
		fields  fields
		wantRet []*Value
	}{
		{
			name: "simple loop",
			fields: fields{
				scope: []*Scope{
					{
						ScopeDescriptor: &ScopeDescriptor{
							Kind:    SKTop,
							Program: []Operation{},
						},
						vars: []Value{
							{&VarDescriptor{kind: VKInt}, Int(5)},
							{&VarDescriptor{kind: VKInt}, Int(1)},
						},
					},
					{
						ScopeDescriptor: &ScopeDescriptor{
							Kind: SKFunction,
							Scopes: []ScopeDescriptor{
								{
									Kind: SKLoop,
									Program: []Operation{
										{class: CKScope, command: SCGetVar, ip1: 2, ip2: 0},
										{class: CKScope, command: SCGetVar, ip1: 2, ip2: 1},
										{class: CKALU, command: AlucGreaterThan},
										{class: CKExec, command: ExeccSkipIf},
										{class: CKExec, command: ExeccBreak},
										{class: CKScope, command: SCCreateScope, ip1: 0},
									},
									Scopes: []ScopeDescriptor{
										{
											Kind: SKBlock,
											Program: []Operation{
												{class: CKScope, command: SCGetVar, ip1: 3, ip2: 0},
												{class: CKALU, command: AlucDec},
												{class: CKScope, command: SCAssign, ip1: 3, ip2: 0},
												{class: CKScope, command: SCGetVar, ip1: 3, ip2: 1},
												{class: CKALU, command: AlucInc},
												{class: CKScope, command: SCAssign, ip1: 3, ip2: 1},
											},
										},
									},
									ScopeSpecific: &LoopScopeDescriptor{Cond: 0},
								},
							},
							Program: []Operation{
								{class: CKScope, command: SCCreateScope, ip1: 0},
								{class: CKScope, command: SCGetVar, ip1: 1, ip2: 0},
							},
						},
						scopeSpecific: &funcScope{},
					},
				},
				stack:    nil,
				Registry: nil,
				options:  EngineOptions{},
			},
			wantRet: []*Value{
				{&VarDescriptor{kind: VKInt}, Int(3)},
			},
		},
		{
			name: "func call",
			fields: fields{
				scope: []*Scope{
					{
						ScopeDescriptor: &ScopeDescriptor{
							Kind: SKTop,
							Scopes: []ScopeDescriptor{
								{
									Kind: SKFunction,
									Scopes: []ScopeDescriptor{
										{
											Kind: SKLoop,
											Program: []Operation{
												{class: CKScope, command: SCGetVar, ip1: 3, ip2: 0},
												{class: CKScope, command: SCGetVar, ip1: 3, ip2: 1},
												{class: CKALU, command: AlucGreaterThan},
												{class: CKExec, command: ExeccSkipIf},
												{class: CKExec, command: ExeccBreak},
												{class: CKScope, command: SCCreateScope, ip1: 0},
											},
											Scopes: []ScopeDescriptor{
												{
													Kind: SKBlock,
													Program: []Operation{
														{class: CKScope, command: SCGetVar, ip1: 4, ip2: 0},
														{class: CKALU, command: AlucDec},
														{class: CKScope, command: SCAssign, ip1: 4, ip2: 0},
														{class: CKScope, command: SCGetVar, ip1: 4, ip2: 1},
														{class: CKALU, command: AlucInc},
														{class: CKScope, command: SCAssign, ip1: 4, ip2: 1},
													},
												},
											},
											ScopeSpecific: &LoopScopeDescriptor{Cond: 0},
										},
									},
									Program: []Operation{
										{class: CKScope, command: SCCreateScope, ip1: 0},
										{class: CKScope, command: SCGetVar, ip1: 2, ip2: 0},
									},
									ScopeSpecific: &FuncDescriptor{
										name:    "test",
										params:  nil,
										results: nil,
									},
								},
							},
						},
						vars: []Value{
							{&VarDescriptor{kind: VKInt}, Int(5)},
							{&VarDescriptor{kind: VKInt}, Int(1)},
						},
					},
					{
						ScopeDescriptor: &ScopeDescriptor{
							Kind: SKFunction,
							Program: []Operation{
								{class: CKScope, command: SCGetVar, ip1: 1, ip2: 2},
								{class: CKExec, command: ExeccCall},
							},
						},
						scopeSpecific: &funcScope{},
					},
				},
				stack:    nil,
				Registry: nil,
				options:  EngineOptions{},
			},
			wantRet: []*Value{
				{&VarDescriptor{kind: VKInt}, Int(3)},
			},
		},
	}

	tests[1].fields.scope[0].vars = append(
		tests[1].fields.scope[0].vars,
		Value{&VarDescriptor{kind: VKFunc}, &tests[1].fields.scope[0].Scopes[0]},
	)
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			eng := &Engine{
				scope:    tt.fields.scope,
				stack:    tt.fields.stack,
				Registry: tt.fields.Registry,
				options:  tt.fields.options,
			}
			if gotRet := eng.exec(); !reflect.DeepEqual(gotRet, tt.wantRet) {
				t.Errorf("exec() = %v, want %v", gotRet, tt.wantRet)
			}
		})
	}
}
