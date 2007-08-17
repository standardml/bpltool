type pos = int
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token
type lexarg = unit
type arg = lexarg

open Tokens

val comlevel : int ref = ref 0

fun error (s,p1,p2) = TextIO.print s

fun mkPos arg p = p
fun tok arg token (p1, p2) = token (mkPos arg p1, mkPos arg p2)
val eof = fn arg => tok arg EOF (0,0)

local
    val keywords = 
	[ ("agent",     AGENT)
	, ("rule",      RULE)
	, ("val",       VAL)
	]

    structure HT = HashTable
    exception KeyWordNotFound

    val keywords_table : (string, (pos * pos) -> (svalue, pos) token) HashTable.hash_table =
	HT.mkTable(HashString.hashString, op = ) (32,KeyWordNotFound)
    val _ = 
	List.app (fn (s,t) => HT.insert keywords_table (s, t)) keywords
in
    fun lookupId arg (s,p1) =
	let val p2 = p1 + String.size s
	in  case HT.find keywords_table s of
		SOME token => tok arg token (p1,p2)
	      | NONE =>
		if Char.isUpper(String.sub(s,0))
		then CTRL(s, mkPos arg p1, mkPos arg p2)
		else VAR(s, mkPos arg p1, mkPos arg p2)
	end
end (*local*)

%%

%header (functor LexerLexFun(structure Tokens : Parser_TOKENS));
%arg (arg);

%s COM STR;

  WS=               [\ \t\013];
  Letter=           [a-zA-Z_];
  Digit=            [0-9];
  Special=          ['_];
  LetterOrDigit=    ({Letter}|{Digit}|{Special});

  Identifier=       {Letter}{LetterOrDigit}*;
  Integer=          {Digit}+;

  NotQuoteBackslash= [^"\];

%%

<INITIAL>{WS}    => (continue ());
<INITIAL>"\n"    => (continue ());
<INITIAL>"("     => (tok arg LPAREN(yypos,yypos+1));
<INITIAL>")"     => (tok arg RPAREN(yypos,yypos+1));
<INITIAL>"."     => (tok arg PERIOD(yypos,yypos+1));
<INITIAL>";"     => (tok arg SEMI(yypos,yypos+1));
<INITIAL>"="     => (tok arg EQ(yypos,yypos+1));
<INITIAL>"->"    => (tok arg ARROW(yypos,yypos+2));
<INITIAL>"|"     => (tok arg BAR(yypos,yypos+1));

<INITIAL>"(*"    => (comlevel := 1; YYBEGIN COM; continue());

<INITIAL>{Identifier} => (lookupId arg (yytext,yypos));
<INITIAL>"["{Integer}"]" => 
    (case Int.fromString(String.extract(yytext,1,SOME(size yytext-1))) of
	 SOME i => HOLE(i, yypos, yypos+size yytext)
       | NONE => (error("Error: illegal integer "^yytext, 
			mkPos arg yypos, mkPos arg (yypos+size yytext));
		  continue())
    );

<INITIAL>.       => (error("Error: unknown character " ^ yytext ^ " / #" ^ 
			   Int.toString(Char.ord (String.sub(yytext,0))),
                           mkPos arg yypos, mkPos arg yypos+1);
                     continue());

<COM>"(*"        => (comlevel := !comlevel + 1; continue());
<COM>"*)"        => (comlevel := !comlevel - 1;
		     if !comlevel = 0 then YYBEGIN INITIAL
			              else ();
		     continue());
<COM>"\n"        => (continue());
<COM>.           => (continue());
