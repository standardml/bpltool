(* Copyright (c) 2006  The BPL Group at the IT University of Copenhagen
 *
 * This file is part of BPL.
 *
 * BPL is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 *
 * BPL is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with BPL; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
 * USA
 *)

(** Lexer specification for MiniML terms.
 * @version $LastChangedRevision$
 * Modified: $Date: 2006/05/19 20:12:35 $ by: $Author: hniss $
 *)

fun error (msg, {pos=p1,src=s1}, {pos=p2,src=s2}) =
    let open Pretty
	val err = SourceLocation.ppSourceLocation "foo" (p1,p2) [ppString msg]
    in  ppPrint err (plainOutput ("(*","*)")) TextIO.stdErr
    end

type pos = {pos:int,src:Source.src}
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token
type lexarg = {src: Source.src}
type arg = lexarg

open Tokens

val comlevel : int ref = ref 0

fun mkPos (arg as {src}) p = {pos=p,src=src}
fun tok arg token (p1, p2) = token (mkPos arg p1, mkPos arg p2)
val eof = fn arg => tok arg EOF (0,0)

local
    val keywords = 
	[ ("case",      CASE)
	, ("datatype",  DATATYPE)
	, ("else",      ELSE)
	, ("end",       END)
	, ("export",    EXPORT)
	, ("fix",       FIX)
	, ("fn",        LAMBDA)
	, ("from",      FROM)
	, ("fun",       FUN)
	, ("in",        IN)
	, ("if",        IF)
	, ("let",       LET)
	, ("of",        OF)
	, ("ref",       REF)
	, ("then",      THEN)
	, ("type",      TYPE)
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
		then CONS(s, mkPos arg p1, mkPos arg p2)
		else ID(s, mkPos arg p1, mkPos arg p2)
	end
end (*local*)

local
    val p : int ref = ref 0
    val current : string list ref = ref []
in
    fun resetString p' = (current := []; p := p')
    fun addString s = current := s :: !current
    fun getString arg = 
	let val s = String.concat (rev (!current))
	in  STRING(s, mkPos arg (!p), mkPos arg (!p + String.size s))
	end
end (*local*)
%%

%header (functor MiniMLLexFun(structure Tokens : MiniML_TOKENS));
%arg (arg);

%s COM STR;

  WS=               [\ \t\013];
  Letter=           [a-zA-Z_];
  Digit=            [0-9];
  Special=          ['_];
  LetterOrDigit=    ({Letter}|{Digit}|{Special});

  Identifier=       {Letter}{LetterOrDigit}*;
  TyId=             [']{Identifier};
  Integer=          {Digit}+;

  NotQuoteBackslash= [^"\];

%%

<INITIAL>{WS}    => (continue ());
<INITIAL>"\n"    => (continue ());
<INITIAL>"("     => (tok arg LPAREN(yypos,yypos+1));
<INITIAL>")"     => (tok arg RPAREN(yypos,yypos+1));
<INITIAL>"["     => (tok arg LBRACKET(yypos,yypos+1));
<INITIAL>"]"     => (tok arg RBRACKET(yypos,yypos+1));
<INITIAL>"."     => (tok arg PERIOD(yypos,yypos+1));
<INITIAL>","     => (tok arg COMMA(yypos,yypos+1));
<INITIAL>";"     => (tok arg SEMICOLON(yypos,yypos+1));
<INITIAL>":"     => (tok arg COLON(yypos,yypos+1));
<INITIAL>"+"     => (tok arg PLUS(yypos,yypos+1));
<INITIAL>"-"     => (tok arg MINUS(yypos,yypos+1));
<INITIAL>"*"     => (tok arg ASTERISK(yypos,yypos+1));
<INITIAL>"/"     => (tok arg DIV(yypos,yypos+1));
<INITIAL>"<="    => (tok arg LE(yypos,yypos+2));
<INITIAL>">="    => (tok arg GE(yypos,yypos+2));
<INITIAL>"="     => (tok arg EQ(yypos,yypos+1));
<INITIAL>"<"     => (tok arg LT(yypos,yypos+1));
<INITIAL>">"     => (tok arg GT(yypos,yypos+1));
<INITIAL>"::"    => (tok arg COLONCOLON(yypos,yypos+2));
<INITIAL>"=>"    => (tok arg DARROW(yypos,yypos+2));
<INITIAL>"->"    => (tok arg ARROW(yypos,yypos+2));
<INITIAL>"|"     => (tok arg BAR(yypos,yypos+1));
<INITIAL>"#"     => (tok arg HASH(yypos,yypos+1));
<INITIAL>"!"     => (tok arg BANG(yypos,yypos+1));
<INITIAL>":="    => (tok arg ASSIGN(yypos,yypos+2));
<INITIAL>"@"     => (tok arg AT(yypos,yypos+1));
<INITIAL>"^"     => (tok arg HAT(yypos,yypos+1));

<INITIAL>\"      => (resetString yypos; YYBEGIN STR; continue());

<INITIAL>"(*"    => (comlevel := 1; YYBEGIN COM; continue());

<INITIAL>{Identifier} => (lookupId arg (yytext,yypos));
<INITIAL>"'"{Identifier} => 
    (TYID(String.extract(yytext,1,NONE),
	  mkPos arg yypos,mkPos arg (yypos+size yytext)));
<INITIAL>{Integer} => 
    (case Int.fromString yytext of
	 SOME i => INTLIT(i, mkPos arg yypos, mkPos arg (yypos+size yytext))
       | NONE => (error("Error: illegal integer "^yytext, 
			mkPos arg yypos, mkPos arg (yypos+size yytext));
		  continue())
    );

<INITIAL>.       => (error("Error: unknown character " ^ yytext ^ " / #" ^ 
			   Int.toString(Char.ord (String.sub(yytext,0))),
			   mkPos arg yypos, mkPos arg (yypos+1));
		     continue());
<STR>"\""        => (YYBEGIN INITIAL; getString arg );
<STR>"\\\""      => (addString "\""; continue());
<STR>{NotQuoteBackslash} => (addString yytext; continue());

<COM>"(*"        => (comlevel := !comlevel + 1; continue());
<COM>"*)"        => (comlevel := !comlevel - 1;
		     if !comlevel = 0 then YYBEGIN INITIAL
			              else ();
		     continue());
<COM>"\n"        => (continue());
<COM>.           => (continue());
