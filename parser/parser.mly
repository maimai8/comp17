%{
(* 補助的な変数、関数、型などの定義 *)
%}

/* 以降、どういうわけかコメントが C 式になることに注意 */
/* トークンの定義 */
%token LPAREN
%token RPAREN
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token MOD
%token PLUSDOT
%token MINUSDOT
%token TIMESDOT
%token DIVIDEDOT
%token LET
%token REC
%token IN
%token IF
%token THEN
%token  ELSE
%token EQUAL
%token NOTEQUAL
%token ORMORE
%token ORLESS
%token MORE
%token LESS
%token <int> NUMBER /* 整数NUMBERには int 型の値が伴うことを示す */
%token <float> REAL /*  実数REALにはfloat型が伴う*/
%token <string> VARIABLE /*  変数VARIABLEにはstring型が伴う*/
%token APPLICATION
%token EOF
/* End of File: 入力の終わりを示す */

/* 非終端記号の型をここで宣言する */
/*  main.mlでParserの引数？的なものになっている。*/
%type <Syntax.t> expr

/* 開始記号の定義 */
%start expr

/* 演算子の優先順位を指定する */
/* 下に行くほど強く結合する */
/*  if文、let文 */ /* ELSE以外、つまりIFとかLETとかは必要ないのか？ */
%nonassoc ELSE
/* 加減算 */
%left PLUS PLUSDOT MINUS MINUSDOT
/* 乗除算 */
%left TIMES TIMESDOT DIVIDE DIVIDEDOT MOD
/*   関数呼び出し*/
%nonassoc REC

%nonassoc UNARY
/* nonassoc は結合なし（毎回、かっこを書かなくてはならない）、
   left は左結合、right は右結合 */

/* 以下の %% は省略不可。それ以降に文法規則を書く */
%%

simple_expr:
| NUMBER
	{ Syntax.Number ($1) }
| REAL
	{ Syntax.Real ($1) }
| VARIABLE
	{ Syntax.Variable ($1) }
| LPAREN expr RPAREN
	{ $2 }

expr:
| simple_expr
	{ $1 }
| expr PLUS expr
	{ Syntax.Op ($1, Operator.Plus, $3) }
| expr PLUSDOT expr
	{ Syntax.Op ($1, Operator.PlusDot, $3) }
| PLUS expr %prec UNARY
	{ Syntax.Op ( Syntax.Number (0), Operator.Plus, $2) }
| expr MINUS expr
	{ Syntax.Op ($1, Operator.Minus, $3) }
| expr MINUSDOT expr
       	{ Syntax.Op ($1, Operator.MinusDot, $3) }
| MINUS expr %prec UNARY
	{ Syntax.Op ( Syntax.Number (0), Operator.Minus, $2) }
| expr TIMES expr
	{ Syntax.Op ($1, Operator.Times, $3) }
| expr TIMESDOT expr
       	{Syntax.Op ($1, Operator.TimesDot, $3) }
| expr DIVIDE expr
	{ Syntax.Op ($1, Operator.Divide, $3) }
| expr DIVIDEDOT expr
	{ Syntax.Op ($1, Operator.DivideDot, $3) }
| expr MOD expr
	{ Syntax.Op ($1, Operator.Mod, $3) }
| IF expr EQUAL expr THEN expr ELSE expr
	{ Syntax.IfEqual ($2, $4, $6, $8) }
| IF expr NOTEQUAL expr THEN expr ELSE expr
	{ Syntax.IfEqual ($2, $4, $8, $6) }
| IF expr ORLESS expr THEN expr ELSE expr
	{ Syntax.IfLess ($4, $2, $8, $6)}
| IF expr ORMORE expr THEN expr ELSE expr
	{ Syntax.IfLess ($2, $4, $8, $6) }
|IF expr LESS expr THEN expr ELSE expr
	{ Syntax.IfLess ($2, $4, $6, $8) }
| IF expr MORE expr THEN expr ELSE expr
	{ Syntax.IfLess ($4, $2, $6, $8) }
| LET VARIABLE EQUAL expr IN expr
	{ Syntax.Let (($2, Type.gen_type()), $4, $6) }
| LET REC VARIABLE expr_var EQUAL expr IN expr
	{ Syntax.LetRec (($3, Type.gen_type()), $4, $6, $8 ) }
| simple_expr expr_app
	{ Syntax.Application ($1, $2) }

/* 変数列＝変数｜変数 変数列 */
expr_var:
| VARIABLE
	{ ($1, Type.gen_type()) :: [] }
| VARIABLE expr_var
	{ ($1, Type.gen_type()) :: $2 }

	/* 単純式の列＝単純式｜単純式 単純式の列 */
	expr_app:
| simple_expr
	{ $1 :: [] }
| simple_expr expr_app
	{ $1 :: $2 }
