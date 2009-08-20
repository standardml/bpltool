/* SICparser.mly */
/* Reverse polish notation calculator. */
%{
open Printf
open Lexing

let var_table = Hashtbl.create 16
let type_table = Hashtbl.create 16
let yy_flex_debug = 1
%}

%token <int> NUM
%token <string> VARIABLE
%token <string> DATATYPE
%token <string> TYPE
%token <string> NAME
%token <string> STRING
%token NEWLINE TYPETOKEN PLUS MINUS MULTIPLY DIVIDE CARET UMINUS COLON 
%token RANGLE LANGLE DOT INACT PARCOMP COMMA EQUAL SEMICOLON NEWTOKEN OPENTOKEN
%token VARIABLE NAMETOKEN PROCESSTOKEN CONTEXTTOKEN TEXTBRACER
%token TEXTBRACEL PROCDECLARATION PARENL PARENR DOUBLEQUOTE INSTTOKEN EOF


%left PLUS MINUS
%nonassoc PARCOMP


%start input
%type <unit> input

%% /* Grammar rules and actions follow */
input:	/* empty */	{ }
| input variableDeclaration typeDeclaration processDeclaration
    processInstantiation { }
| EOF {}
;
  variableDeclaration: NAMETOKEN namelist SEMICOLON { Printf.printf "**********Name Declaration\n"; }
;
  namelist : NAME {}
		  | namelist COMMA NAME {}
;
  typeDeclaration : TYPETOKEN TEXTBRACEL openTypesDec contextTypesDec
    TEXTBRACER {  Printf.printf "*******Type Declaration\n"; }
;
  openTypesDec :  OPENTOKEN TEXTBRACEL typeDeclaration TEXTBRACER {Printf.printf "*******Open Types Declaration\n";}
;
  typeDeclaration : /* empty */ {}
                   |typeDeclaration typelist COLON COLON EQUAL DATATYPE SEMICOLON  {}
;
  typelist : TYPE {}
		  | typelist COMMA TYPE {}
;
  contextTypesDec : CONTEXTTOKEN TEXTBRACEL ctxTypeDec TEXTBRACER {Printf.printf "*******Context Types Declaration\n";}
;
  ctxTypeDec : /* empty */ {}
		   | ctxTypeDec typelist COLON COLON EQUAL ctxType
		       SEMICOLON {}
;
  ctxType : typelist MINUS RANGLE DATATYPE {(*Printf.printf "*******Context type****** \n"; *)}
;
  processDeclaration : PROCDECLARATION TEXTBRACEL processSpec    TEXTBRACER {}
;
  processSpec : /* empty */ {}
		   | processSpec PROCESSTOKEN VARIABLE COLON COLON EQUAL procExpression SEMICOLON {}
;
  procExpression :  TYPE COLON COLON PARENL NAME DOT NAME PARENR DOT procExpression {Printf.printf "*******Service Declaration****** \n";}
		   | typelist COLON COLON LANGLE NAME DOT exp RANGLE DOT procExpression {Printf.printf "*******Service Request****** \n";}
		  | NAME LANGLE exp RANGLE DOT procExpression {Printf.printf "*******Send Reply****** \n";}
		  | NAME PARENL NAME PARENR DOT procExpression {Printf.printf "*******Receive Reply****** \n";}
		  | PARENL NEWTOKEN NAME PARENR procExpression {Printf.printf "*******Restriction****** \n";}
		  | procExpression PARCOMP procExpression {Printf.printf "*******Parallel Composition****** \n";}
		  | INACT {Printf.printf "*******Inaction****** \n";}
		  | VARIABLE {Printf.printf "**Process Variable** \n";}
;
		      
/*  varlist : VARIABLE {}
		  | varlist COMMA VARIABLE {}
;
*/
exp:	NUM	{ }
	| STRING {}
	| exp PLUS exp 	{}
	| exp MINUS exp 	{}
	| exp MULTIPLY exp 	{}
	| exp DIVIDE exp 	{}
;


processInstantiation : INSTTOKEN TEXTBRACEL processList TEXTBRACER {}
;
processList: VARIABLE {}
	    | processList PARCOMP VARIABLE {}
;

%%

