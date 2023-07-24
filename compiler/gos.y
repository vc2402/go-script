
%{

package compiler

import (
	"github.com/vc2402/go-script/runtime"
)

%}

%union {
	val string
	vals []string
	fn *function
//	functions []*function
	args []*varDef
	arg *varDef
	File File
	statement *statement
	statements []*statement
	typeRef *typeRef
	lvalue *lvalue
	lvList []*lvalue
	constant *constant
	expression *expression
	arrayAccess *arrayAccess
	fieldAccess *fieldAccess
	expressions []*expression
	funcCall *funcCall
	varDef *varDef
	assignment *assignment
	fileContents []any
	ifStatement *ifStatement
	forStatement *forStatement
	methodCall *methodCall
}

%type <val> package_def pckg_name pckg_host

%type <vals> idents_list

%type <fn> function

%type <arg> argument

%type <args> arguments_list func_return types_list

%type <File> file

%type <fileContents> file_contents

%type <statement> statement simple_stmt inc_dec_stmt init_stmt post_stmt range_stmt block

%type <statements> statements

%type <lvalue> lvalue

%type <lvList> lvalue_list

%type <constant> const

%type <expression> expression param expr_part condition

%type <expressions> params expr_list ret_stmt

%type <arrayAccess> array_access

%type <assignment> assignment

%type <fieldAccess> field_access

%type <funcCall> function_call make_call

%type <methodCall> method_call

%type <varDef> var_statement var_initialization

%type <typeRef> type_ref

%type <ifStatement> if_stmt

%type <forStatement> for_stmt

%token <val> IDENT INT FLOAT CHAR STRING BOOL FUNCNAME

%left '+' '-' LOR

%left '*' '/' REM LAND

%left GT LT EQ NEQ GTE LTE

%token '!' INC DEC

%right DEFINE

%token '{' '}'

%right '='

%token '[' ']'

%left ','

%left '.'

%right <val> '(' ')'

%token ';' ':' ELLIPSIS

%token VAR FUNC PACKAGE MAP IF ELSE SWITCH CASE FOR BREAK CONTINUE RANGE RETURN MAKE

//%token	<val>

%%
root:
	file {Goslex.(*GosLex).File = $1}
	;

file:
    file_contents {$$=File{pckg: "", parts: $1, pos: Goslex.(*GosLex).Pos}}
|   package_def	file_contents {$$=File{pckg: $1, parts: $2, pos: Goslex.(*GosLex).Pos}}
	;

package_def:
    PACKAGE pckg_name {$$=$2}
    ;

pckg_name:
    IDENT               {$$=$1}
|   pckg_host '/' IDENT {$$=$1+"/"+$3}
|   pckg_name '/' IDENT {$$=$1+"/"+$3}
    ;

pckg_host:
    IDENT '.' IDENT     {$$=$1+"."+$3}
|   pckg_host '.' IDENT {$$=$1+"."+$3}
    ;

file_contents:
                            {$$=[]any{}}
|	file_contents function  {$$=append($1,$2)}
|	file_contents statement {$$=append($1,$2)}
	;

function:
	FUNC FUNCNAME '(' arguments_list ')' func_return block {$$=&function{name:$2, args: $4, ret: $6, body: $7, pos: Goslex.(*GosLex).Pos}}
	;

arguments_list:
			{$$=[]*varDef{}}
|	argument  {$$ = []*varDef{$1}}
|	arguments_list ',' argument {$$ = append($1, $3)}
	;

argument:
	idents_list type_ref {$$ = &varDef{names: $1, tip: $2, pos: Goslex.(*GosLex).Pos}}
	;

func_return:
				{$$=[]*varDef{}}
|	type_ref		{$$=[]*varDef{&varDef{tip: $1, pos: Goslex.(*GosLex).Pos}}}
|	'(' types_list ')'	{$$ = $2}
|	'(' arguments_list ')'	{$$ = $2}
	;

types_list:
	type_ref 			{$$=[]*varDef{&varDef{tip: $1, pos: Goslex.(*GosLex).Pos}}}
|	types_list ',' type_ref		{$$ = append($1, &varDef{tip: $3, pos: Goslex.(*GosLex).Pos})}
	;

idents_list:
	IDENT			{$$=[]string{$1}}
|	idents_list ',' IDENT	{$$=append($1, $3)}
	;
type_ref:
	IDENT        {$$=&typeRef{name: $1, pos: Goslex.(*GosLex).Pos}}
|	IDENT '.' IDENT  {$$=&typeRef{pckg: $1, name: $3, pos: Goslex.(*GosLex).Pos}}
|   MAP '[' IDENT ']' type_ref {$$=&typeRef{mapKey: $3, elem: $5, pos: Goslex.(*GosLex).Pos}}
|   '[' ']' type_ref {$$=&typeRef{elem:$3, pos: Goslex.(*GosLex).Pos}}
	;

make_call:
	MAKE '(' type_ref ')'					{$$=&funcCall{name:"map", returnTypes: []*typeRef{$3}, convertTo: runtime.VarKind(-1), pos: Goslex.(*GosLex).Pos}}
|	MAKE '(' type_ref ',' expression ')'			{$$=&funcCall{name:"map", params:[]*expression{$5}, returnTypes: []*typeRef{$3}, convertTo: runtime.VarKind(-1), pos: Goslex.(*GosLex).Pos}}
|	MAKE '(' type_ref ',' expression ',' expression ')'	{$$=&funcCall{name:"map", params:[]*expression{$5, $7}, returnTypes: []*typeRef{$3}, convertTo: runtime.VarKind(-1), pos: Goslex.(*GosLex).Pos}}
	;
block:
	'{' statements '}' {$$=&statement{kind:stmtKindBlock, stmt:$2, pos: Goslex.(*GosLex).Pos}}
	;

statements:
//		{$$=[]*statement{}}
	statement {$$=[]*statement{$1}}
|	statements statement {$$=append($1,$2)}
	;

statement:
	simple_stmt ';' {$$=$1}
|	var_statement ';' {$$=&statement{kind:stmtKindVar, stmt:$1, pos: Goslex.(*GosLex).Pos}}
//|	function_call ';'  {$$=&statement{kind:stmtKindFunc, stmt:$1, pos: Goslex.(*GosLex).Pos}}
|	block {$$=$1}
|	if_stmt {$$=&statement{kind:stmtKindIf, stmt:$1, pos: Goslex.(*GosLex).Pos}}
|	for_stmt {$$=&statement{kind:stmtKindFor, stmt:$1, pos: Goslex.(*GosLex).Pos}}
|	ret_stmt {$$=&statement{kind:stmtKindRet, stmt:$1, pos: Goslex.(*GosLex).Pos}}
	;

var_statement:
	VAR idents_list type_ref {$$=&varDef{names:$2, tip: $3, pos: Goslex.(*GosLex).Pos}}
|	VAR idents_list type_ref '=' expr_list  {$$=&varDef{names:$2, tip: $3, init:$5, pos: Goslex.(*GosLex).Pos}}
|	VAR idents_list '=' expr_list  {$$=&varDef{names:$2, init:$4, pos: Goslex.(*GosLex).Pos}}
	;

assignment:
	lvalue_list '=' expr_list {
	$$=&assignment{left:$1, right:$3, pos: Goslex.(*GosLex).Pos}
	for _, lv := range $1 {lv.forAssignment = true}
	}
	;

var_initialization:
	lvalue_list DEFINE expr_list  {$$=&varDef{lvalues:$1, init: $3, pos: Goslex.(*GosLex).Pos}}
	;

function_call:
	FUNCNAME '(' params ')'  {$$=&funcCall{name:$1, params:$3, pos: Goslex.(*GosLex).Pos}}
//|	IDENT '.' FUNCNAME '(' params ')'  {$$=&funcCall{pckg: $1, name:$3, params:$5, pos: Goslex.(*GosLex).Pos}}
	;

simple_stmt:
 				{$$=&statement{kind:stmtKindEmpty, pos: Goslex.(*GosLex).Pos}}
| 	expression		{$$=&statement{kind:stmtKindExpression, stmt: $1, pos: Goslex.(*GosLex).Pos}}
| 	inc_dec_stmt		{$$=$1}
|	assignment		{$$=&statement{kind:stmtKindAssign, stmt:$1, pos: Goslex.(*GosLex).Pos}}
| 	var_initialization	{$$=&statement{kind:stmtKindInit, stmt:$1, pos: Goslex.(*GosLex).Pos}}
	;

inc_dec_stmt:
	lvalue INC	{$$=&statement{kind:stmtKindInc, stmt: $1, pos: Goslex.(*GosLex).Pos}}
|	lvalue DEC	{$$=&statement{kind:stmtKindDec, stmt: $1, pos: Goslex.(*GosLex).Pos}}
	;

if_stmt:
	IF expression block			{$$=&ifStatement{condition: $2, thn: $3, pos: Goslex.(*GosLex).Pos}}
|	IF expression block ELSE block		{$$=&ifStatement{condition: $2, thn: $3, els: $5, pos: Goslex.(*GosLex).Pos}}
|	IF expression block ELSE if_stmt	{$$=&ifStatement{condition: $2, thn: $3, els: &statement{kind:stmtKindIf, stmt:$5, pos: Goslex.(*GosLex).Pos}}}
	;

for_stmt:
	FOR block			{$$=&forStatement{block: $2, pos: Goslex.(*GosLex).Pos}}
|	FOR expression block		{$$=&forStatement{condition: $2, block: $3, pos: Goslex.(*GosLex).Pos}}
|	FOR init_stmt ';' condition ';' post_stmt block	{$$=&forStatement{condition: $4, initStmt: $2, postStmt: $6, block: $7, pos: Goslex.(*GosLex).Pos}}
|	FOR range_stmt block		{$$=&forStatement{rangeStmt: $2, block: $3, pos: Goslex.(*GosLex).Pos}}
	;

ret_stmt:
	RETURN	';'		{$$=[]*expression(nil)}
|	RETURN expr_list ';'	{$$=$2}
	;

init_stmt:
	simple_stmt		{$$=$1}
	;

post_stmt:
	simple_stmt		{$$=$1}
	;

condition:
	expression		{$$=$1}
	;

range_stmt:
	assignment		{$$=&statement{kind:stmtKindAssign, stmt:$1, pos: Goslex.(*GosLex).Pos}}
	var_initialization	{$$=&statement{kind:stmtKindInit, stmt:$1, pos: Goslex.(*GosLex).Pos}}
	;

lvalue:
	IDENT	{$$ = &lvalue{kind:lvalueKindIdent, stmt: $1, pos: Goslex.(*GosLex).Pos}}
|	array_access {$$ = &lvalue{kind:lvalueKindIndex, stmt: $1, pos: Goslex.(*GosLex).Pos}}
|	field_access {$$ = &lvalue{kind:lvalueKindField, stmt: $1, pos: Goslex.(*GosLex).Pos}}
	;

lvalue_list:
	lvalue			{$$=[]*lvalue{$1}}
|	lvalue_list ',' lvalue	{$$=append($1, $3)}
	;

array_access:
	lvalue '[' expression ']' {$$=&arrayAccess{lvalue:$1, expression: $3, pos: Goslex.(*GosLex).Pos}}
	;

field_access:
	lvalue '.' IDENT {$$=&fieldAccess{lvalue:$1, field: $3, pos: Goslex.(*GosLex).Pos}}
	;

method_call:
	lvalue '.' FUNCNAME '(' params ')' {$$=&methodCall{lvalue:$1, name: $3, params: $5, pos: Goslex.(*GosLex).Pos}}
	;

expression:
	expr_part {$$=$1}
|	'(' expression ')' {$$=&expression{kind:expressionKindParens, left:$2, pos: Goslex.(*GosLex).Pos}}
|	expression '+' expression {$$=&expression{kind:expressionKindSum, left:$1, right:$3, pos: Goslex.(*GosLex).Pos}}
|	expression '-' expression {$$=&expression{kind:expressionKindSub, left:$1, right:$3, pos: Goslex.(*GosLex).Pos}}
|	expression '*' expression {$$=&expression{kind:expressionKindMul, left:$1, right:$3, pos: Goslex.(*GosLex).Pos}}
|	expression '/' expression {$$=&expression{kind:expressionKindDiv, left:$1, right:$3, pos: Goslex.(*GosLex).Pos}}
|	expression LAND expression {$$=&expression{kind:expressionKindAnd, left:$1, right:$3, pos: Goslex.(*GosLex).Pos}}
|	expression LOR expression {$$=&expression{kind:expressionKindOr, left:$1, right:$3, pos: Goslex.(*GosLex).Pos}}
|	'!' expr_part {$$=&expression{kind:expressionKindNot, left:$2, pos: Goslex.(*GosLex).Pos}}
|	expression EQ expression {$$=&expression{kind:expressionKindEqualTo, left:$1, right:$3, pos: Goslex.(*GosLex).Pos}}
|	expression NEQ expression {$$=&expression{kind:expressionKindNotEqualTo, left:$1, right:$3, pos: Goslex.(*GosLex).Pos}}
|	expression GT expression {$$=&expression{kind:expressionKindGreaterThan, left:$1, right:$3, pos: Goslex.(*GosLex).Pos}}
|	expression LT expression {$$=&expression{kind:expressionKindLessThan, left:$1, right:$3, pos: Goslex.(*GosLex).Pos}}
|	expression GTE expression {$$=&expression{kind:expressionKindGreaterThanOrEqualTo, left:$1, right:$3, pos: Goslex.(*GosLex).Pos}}
|	expression LTE expression {$$=&expression{kind:expressionKindLessThanOrEqualTo, left:$1, right:$3, pos: Goslex.(*GosLex).Pos}}
	;

expr_list:
	expression			{$$=[]*expression{$1}}
|	expr_list ',' expression	{$$=append($1, $3)}
	;

expr_part:
	const 		{$$=&expression{kind:expressionKindConst, left: $1, pos: Goslex.(*GosLex).Pos}}
|	method_call 	{{$$=&expression{kind:expressionKindMethod, left: $1, pos: Goslex.(*GosLex).Pos}}}
|	lvalue 		{$$=&expression{kind:expressionKindLValue, left: $1, pos: Goslex.(*GosLex).Pos}}
|	function_call	{{$$=&expression{kind:expressionKindFunc, left: $1, pos: Goslex.(*GosLex).Pos}}}
|	make_call	{{$$=&expression{kind:expressionKindFunc, left: $1, pos: Goslex.(*GosLex).Pos}}}
	;

const:
	INT  {$$=&constant{kind:constKindInt, value:$1, pos: Goslex.(*GosLex).Pos}}
|	FLOAT {$$=&constant{kind:constKindFloat, value:$1, pos: Goslex.(*GosLex).Pos}}
|	CHAR {$$=&constant{kind:constKindChar, value:$1, pos: Goslex.(*GosLex).Pos}}
|	STRING {$$=&constant{kind:constKindString, value:$1, pos: Goslex.(*GosLex).Pos}}
|	BOOL {$$=&constant{kind:constKindBool, value:$1, pos: Goslex.(*GosLex).Pos}}

params:
	{$$=[]*expression{}}
|	param {$$=[]*expression{$1}}
|	params ',' param {$$=append($1,$3)}
	;

param:
	expression
	;


%%
