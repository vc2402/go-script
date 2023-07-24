package compiler

import (
	"fmt"
	"go/scanner"
	"go/token"
)

// The parser expects the lexer to return 0 on EOF.  Give it a name
// for clarity.
const eof = 0

// The parser uses the type <prefix>Lex as a lexer. It must provide
// the methods Lex(*<prefix>SymType) int and Error(string).
type GosLex struct {
	S    scanner.Scanner
	Pos  token.Pos
	Fset *token.FileSet
	File
	next  *Token
	error error
}

type Token struct {
	pos token.Pos
	tok token.Token
	lit string
}

// The parser calls this method to get each new token. This
// implementation returns operators and NUM.
func (x *GosLex) Lex(yylval *GosSymType) int {
	for {
		t := x.Scan()
		if t.tok == token.EOF {
			return eof
		}
		x.Pos = t.pos
		yylval.val = t.lit
		switch t.tok {
		case token.INT:
			return INT
		case token.CHAR:
			return CHAR
		case token.STRING:
			return STRING
		case token.FLOAT:
			return FLOAT
		case token.ADD:
			return '+'
		case token.SUB:
			return '-'
		case token.MUL:
			return '*'
		case token.QUO:
			return '/'
		case token.LOR:
			return LOR
		case token.LAND:
			return LAND
		case token.DEFINE:
			return DEFINE
		case token.GTR:
			return GT
		case token.LSS:
			return LT
		case token.INC:
			return INC
		case token.DEC:
			return DEC
		case token.EQL:
			return EQ
		case token.NEQ:
			return NEQ
		case token.LEQ:
			return LTE
		case token.GEQ:
			return GTE
		case token.LBRACE:
			return '{'
		case token.RBRACE:
			return '}'
		case token.ASSIGN:
			return '='
		case token.LPAREN:
			return '('
		case token.RPAREN:
			return ')'
		case token.LBRACK:
			return '['
		case token.RBRACK:
			return ']'
		case token.COMMA:
			return ','
		case token.PERIOD:
			return '.'
		case token.SEMICOLON:
			return ';'
		case token.COLON:
			return ':'
		case token.VAR:
			return VAR
		case token.FUNC:
			return FUNC
		case token.IDENT:
			if t.lit == "true" || t.lit == "false" {
				return BOOL
			}
			t := x.Next()
			if t.tok == token.LPAREN {
				if t.lit == "make" {
					return MAKE
				}
				return FUNCNAME
			}
			return IDENT
		case token.PACKAGE:
			return PACKAGE
		case token.MAP:
			return MAP
		case token.IF:
			return IF
		case token.ELSE:
			return ELSE
		case token.FOR:
			return FOR
		case token.RANGE:
			return RANGE
		case token.SWITCH:
			return SWITCH
		case token.CASE:
			return CASE
		case token.BREAK:
			return BREAK
		case token.CONTINUE:
			return CONTINUE
		case token.RETURN:
			return RETURN
		default:
			return int(t.tok)
		}
	}
}

// The parser calls this method on a parse error.
func (x *GosLex) Error(s string) {
	x.error = fmt.Errorf("parse error: %s: %s", x.Fset.Position(x.Pos), s)
}

func (x *GosLex) Err() error {
	return x.error
}

func (x *GosLex) Scan() Token {
	if x.next != nil {
		t := x.next
		x.next = nil
		return *t
	}
	t := Token{}
	t.pos, t.tok, t.lit = x.S.Scan()
	return t
}

func (x *GosLex) Next() Token {
	if x.next == nil {
		t := x.Scan()
		x.next = &t
	}
	return *x.next
}
