Nonterminals Program Block Statement Statements Declaration Expression Variable DimStatement
	VParamVar DecList AryVar LogicalExpression Call FuncCall VParamList WhileStatement 
	BreakStatement ProcStatement ThreadStatement IfStatement ParamAryVar FuncStatement 
	ReturnStatement ForStatement Var_or_Ary AssignStatement SwitchStatement ElseifStatement
	CaseList CasePat DefaultPat
%	StackStatement IntStatement RatStatement ExpressionList
	TypeName DeclareStatement  ParamList VParamAryVar DimList PDimList TraceStatement WaitStatement
	 list elements element uminus.
Terminals atom integer string 'if' 'else' 'while' 'and' 'or' 'not' 'proc' 'thread' 'program'
	'for'  'func' 'return' 'break' 'dim' '++' '--' 'new' 'trace' 'wait' 'switch' 'default' 'elseif' 'case' ':'
% 'int' 'stack' 'rat' 	
	'(' ')' ',' '{' '}' '[' ']' '=' '==' '=/=' '>' '<' '>=' '=<' '+' '-' '*' '/' '|' ';' 'dot' 'rem' 'true' 'false'.
Rootsymbol Program.

Nonassoc 40 '='.
Nonassoc 50 'new'.
Left 200 'and' 'or'.
Unary 300 'not'.
Nonassoc 500 '==' '=/='  '>' '<' '>=' '=<'.

Left 1000 '+' '-'.
Left 2000 '*' '/' 'rem'.
Unary 3000 uminus.
Unary 4000 '++' '--'.

Program -> Statement 'dot': '$1'.
Program -> 'program' Statement 'dot': '$2'.


TypeName -> atom : {'type',value_of('$1')}.

Statement -> Block : '$1'.
Statement -> WhileStatement : '$1'.
Statement -> DimStatement : '$1'.
Statement -> DeclareStatement : '$1'.
Statement -> ForStatement : '$1'.
Statement -> ProcStatement : '$1'.
Statement -> FuncStatement : '$1'.
Statement -> BreakStatement : '$1'.
Statement -> ReturnStatement : '$1'.
Statement -> ThreadStatement : '$1'.
Statement -> WaitStatement : '$1'.
Statement -> IfStatement : '$1'.
Statement -> SwitchStatement : '$1'.
Statement -> TraceStatement : '$1'.
Statement -> Call : '$1'.
Statement -> Variable '=' Expression : {def,'$1','$3'}.
Statement -> AryVar '=' Expression : {def,'$1','$3'}.
% Statement -> Expression :  '$1'.
Statement -> '$empty' : [].

DimStatement -> 'dim' AryVar : {dim,'$2'}.
DeclareStatement -> TypeName DecList : {dec,'$1','$2'}.
WhileStatement -> 'while' '(' Expression ')' Statement : {while,'$3','$5'}.
ForStatement -> 'for' '(' Statement ';' Expression ';' Statement ')' Statement : {for,'$3','$5','$7','$9'}.
ProcStatement -> 'proc' atom '('  ')' Statement : {procDec,value_of('$2'),[],'$5'}.
ProcStatement -> 'proc' atom '(' VParamList ')' Statement : {procDec,value_of('$2'),'$4','$6'}.
FuncStatement -> 'func' TypeName atom '('  ')' Statement : {funcDec,'$2',value_of('$3'),[],'$6'}.
FuncStatement -> 'func' TypeName atom '(' VParamList ')' Statement : {funcDec,'$2',value_of('$3'),'$5','$7'}.
ReturnStatement -> 'return'  : {return}.
ReturnStatement -> 'return' Expression  : {return,'$2'}.
BreakStatement -> 'break'  : {break}.
ThreadStatement -> 'thread' Statement : {thread,'$2'}.
WaitStatement -> 'wait'  : {wait}.
IfStatement -> 'if' '(' Expression ')' Statement : {iff,'$3','$5'}.
IfStatement -> 'if' '(' Expression ')' Statement 'else' Statement : {ifelse,'$3','$5','$7'}.
IfStatement -> 'if' '(' Expression ')' Statement 'elseif'  ElseifStatement : {ifelse,'$3','$5','$7'}.
SwitchStatement -> 'switch'  '(' Expression ')' '{' CaseList '}' : {switch, '$3', '$6'}.
AssignStatement -> Variable '=' Expression : {def,'$1','$3'}.
AssignStatement -> AryVar '=' Expression : {def,'$1','$3'}.
TraceStatement -> 'trace' '(' element ')' : {trace,'$3'}.
TraceStatement -> 'trace' '(' list ')' : {trace,'$3'}.

Call -> atom '('  ')' : {'procCall',value_of('$1'),[]}.
Call -> atom '(' ParamList ')' : {'procCall',value_of('$1'),'$3'}.

Block -> '{'   '}' : [].
% Block -> '{'  Statements '}' : '$2'.
Block -> '{'  Statements '}' : {blk,[],'$2'}.
Block -> '{'  DecList '|' Statements '}' : {blk,'$2','$4'}.

Statements -> Statement : ['$1'].
Statements -> Statement ';' Statements : ['$1'|'$3'].

ElseifStatement -> '(' Expression ')' Statement : {iff,'$2','$4'}.
ElseifStatement -> '(' Expression ')' Statement 'else' Statement : {ifelse,'$2','$4','$6'}.
ElseifStatement -> '(' Expression ')' Statement 'elseif' ElseifStatement: {ifelse,'$2','$4','$6'}.


CasePat -> 'case' integer ':' Statement : {casee,{int,value_of('$2')},'$4'}.
CasePat -> Statement : {st,'$1'}.
DefaultPat -> 'default' ':' Statement : {default,'$3'}.

CaseList -> CasePat : ['$1'].
CaseList -> DefaultPat : ['$1'].
CaseList -> CasePat ';' CaseList : ['$1'|'$3'].

DecList -> Declaration : ['$1'].
DecList -> Declaration ',' DecList: ['$1'|'$3'].

Declaration -> Var_or_Ary : '$1'.
Declaration -> AssignStatement : {init, '$1'}.
Declaration -> Variable '=' 'new' TypeName : {new, '$4', '$1'}.
Declaration -> VParamAryVar '=' 'new' TypeName DimList  : {new, '$4', '$1', '$5'}.

DimList -> '[' Expression ']' : ['$2'].
DimList -> '[' Expression ']' DimList : ['$2'|'$4'].

VParamList -> VParamVar : ['$1'].
VParamList -> VParamVar ',' VParamList: ['$1'|'$3'].

VParamVar -> TypeName Variable : {par,'$1','$2'}.
VParamVar -> TypeName VParamAryVar : {par,'$1','$2'}.
% VParamVar -> Variable : {par,'$1'}.
% VParamVar -> VParamAryVar : {par,'$1'}.

Var_or_Ary -> Variable : '$1'.
Var_or_Ary -> AryVar : '$1'.

Variable -> atom : {var,value_of('$1')}.

AryVar -> atom DimList : {ary,value_of('$1'),'$2'}.

VParamAryVar -> atom '['  ']' : {pary,value_of('$1')}.

ParamList -> Expression : ['$1'].
ParamList -> ParamAryVar : ['$1'].
ParamList -> Expression ',' ParamList: ['$1'|'$3'].
ParamList -> ParamAryVar ',' ParamList: ['$1'|'$3'].

ParamAryVar -> atom PDimList : {pary,value_of('$1'),'$2'}.

PDimList -> '[' ']' : 1.
PDimList -> '[' ']' PDimList : 1+'$3'.

% ExpressionList -> Expression : [ '$1' ].
% ExpressionList -> Expression ',' ExpressionList : [ '$1' | '$3' ].

Expression -> '(' Expression ')'  : '$2'.
Expression -> LogicalExpression : '$1'.
Expression -> Expression '+' Expression : {add,'$1','$3'}.
Expression -> Expression '-' Expression : {sub,'$1','$3'}.
Expression -> Expression '*' Expression : {mul,'$1','$3'}.
Expression -> Expression '/' Expression : {divd,'$1','$3'}.
Expression -> Expression 'rem' Expression : {remm,'$1','$3'}.
Expression -> '++' Variable : {inca,'$1'}.
Expression -> '--' Variable : {deca,'$1'}.
Expression -> '++' AryVar : {inca,'$1'}.
Expression -> '--' AryVar : {deca,'$1'}.
Expression -> Variable '++' : {incz,'$1'}.
Expression -> Variable '--' : {decz,'$1'}.
Expression -> AryVar '++' : {incz,'$1'}.
Expression -> AryVar '--' : {decz,'$1'}.
Expression -> uminus : '$1'.
Expression -> integer : {int,value_of('$1')}.
Expression -> Variable : '$1'.
Expression -> AryVar : '$1'.
Expression -> FuncCall : '$1'.
Expression -> string : {string,value_of('$1')}.

uminus -> '-' integer : {int,-1*value_of('$2')}.
uminus -> '-' Expression : {mul,-1,'$2'}.

FuncCall -> atom '('  ')' : {funcCall,value_of('$1'),[]}.
FuncCall -> atom '(' ParamList ')' : {funcCall,value_of('$1'),'$3'}.

LogicalExpression -> 'true'  : true.
LogicalExpression -> 'false'  : false.
LogicalExpression -> '(' LogicalExpression ')'  : '$2'.
LogicalExpression -> LogicalExpression 'and' LogicalExpression : {andd,'$1','$3'}.
LogicalExpression -> LogicalExpression 'or' LogicalExpression : {orr,'$1','$3'}.
LogicalExpression -> 'not' LogicalExpression : {nott,'$2'}.
LogicalExpression -> Expression '==' Expression : {eq,'$1','$3'}.
LogicalExpression -> Expression '=/=' Expression : {neq,'$1','$3'}.
LogicalExpression -> Expression '>' Expression : {gt,'$1','$3'}.
LogicalExpression -> Expression '<' Expression : {lt,'$1','$3'}.
LogicalExpression -> Expression '>=' Expression : {gte,'$1','$3'}.
LogicalExpression -> Expression '=<' Expression : {lte,'$1','$3'}.

list -> '[' ']' : [].
list -> '[' elements ']' : '$2'.
elements -> element : ['$1'].
elements -> element ',' elements : ['$1'|'$3'].
element -> atom : value_of('$1').
element -> list : '$1'. 

Erlang code.
value_of(Token) -> element(3, Token).
line_of(Token) -> element(2, Token).
not_member(Atom,List) -> not(lists:member(Atom,List)).
