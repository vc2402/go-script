package runtime

import "testing"

func TestCommandWithClass_split(t *testing.T) {
	tests := []struct {
		name  string
		cc    CommandWithClass
		want  CommandClass
		want1 Command
	}{
		{
			name:  "alu:add",
			cc:    0,
			want:  CKALU,
			want1: AlucAdd,
		},
		{
			name:  "alu:sub",
			cc:    0x00000001,
			want:  CKALU,
			want1: AlucSub,
		},
		{
			name:  "exec:break",
			cc:    0x01000002,
			want:  CKExec,
			want1: ExeccBreak,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, got1 := tt.cc.split()
			if got != tt.want {
				t.Errorf("split() got = %v, want %v", got, tt.want)
			}
			if got1 != tt.want1 {
				t.Errorf("split() got1 = %v, want %v", got1, tt.want1)
			}
		})
	}
}
