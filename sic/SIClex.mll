(* file: lexer.mll *)
(* Lexical analyzer returns one of the tokens:
   the token NUM of a floating point number,
   operators (PLUS, MINUS, MULTIPLY, DIVIDE),
   or NEWLINE.  It skips all blanks and tabs, unknown characters
   and raises End_of_file on EOF. *)
{
  open SICparser (* Assumes the parser file is "SICparser.mly". *)
}
let digit = ['0'-'9']
let char = ['a'-'z''A'-'Z']
let capschar = ['A'-'Z']

rule token = parse
  | ['n''N']['a''A']['m''M']['e''E'] {Printf.printf "NAMETOKEN\n"; NAMETOKEN}
  | ['t''T'] ['y''Y']['p''P']['e''E']['s''S'] { Printf.printf "TYPETOKEN\n";TYPETOKEN }
  | ['n''N']['e''E']['w''W']   { Printf.printf "NEWTOKEN\n"; NEWTOKEN }
  | ['o''O']['p''P']['e''E']['n''N'] { Printf.printf "OPENTOKEN\n";OPENTOKEN }
  | ['p''P']['r''R']['o''O']['c''C'] { Printf.printf "PROCESSTOKEN\n";PROCESSTOKEN }
  | ['c''C']['o''O']['n''N']['t''T']['e''E']['x''X']['t''T'] {
      Printf.printf "CONTEXTTOKEN\n";CONTEXTTOKEN }
  |
      ['p''P']['r''R']['o''O']['c''C']['e''E']['s''S']['s''S']['e''E']['s''S'] {Printf.printf "PROCDECLARATION\n";PROCDECLARATION}
  | ['i''I']['n''N']['s''S']['t''T'] {Printf.printf "INSTTOKEN\n";INSTTOKEN}
  | "string"|"int" as datavalue {Printf.printf "DATATYPE\n";DATATYPE datavalue}
  | '"' char+ '-'* digit* char* '"' as stringConst {Printf.printf "STRING\n";    STRING stringConst}
  | ':'         {Printf.printf "COLON\n"; COLON}
  | '<'         {Printf.printf "LANGLE\n"; LANGLE }
  | '>'         {Printf.printf "RANGLE\n"; RANGLE }
  | '.'         {Printf.printf "DOT\n"; DOT }
  | '|'         { Printf.printf "PARCOMP\n";PARCOMP }
  | '0'		{ Printf.printf "INACT\n";INACT }
  | '-'		{ Printf.printf "MINUS\n";MINUS }
  | '*'		{ Printf.printf "MULTIPLY\n";MULTIPLY }
  | ','		{ Printf.printf "COMMA\n";COMMA }
  | ';'         { Printf.printf "SEMICOLON\n";SEMICOLON }
  | '='         { Printf.printf "EQUAL\n";EQUAL }
  | '{'         { Printf.printf "TEXTBRACEL\n";TEXTBRACEL }
  | '}'         { Printf.printf "TEXTBRACER\n";TEXTBRACER }
  | '('         { Printf.printf "PARENL\n";PARENL }
  | ')'         { Printf.printf "PARENR\n";PARENR }
  | '"'         { Printf.printf "DOUBLEQUOTE\n";DOUBLEQUOTE }
  | ['\n' ' ' '\t']	{ token lexbuf }
  | ['j''k''x''y']+ '-'* digit* char* as namevariable {Printf.printf "NAME\n"; NAME namevariable}
  | ['s''t']+ '-'* digit* char* as typevariable {Printf.printf "TYPE\n"; TYPE typevariable}
  | char+ '-'* digit* char* as varname {Printf.printf "VARIABLE\n";    VARIABLE varname}
  | digit+ as num { Printf.printf "NUM\n";NUM (int_of_string num) }
  | _		{ token lexbuf } 
  | eof		{ EOF }
