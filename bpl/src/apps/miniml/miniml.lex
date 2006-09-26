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

type pos = int
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token
type lexarg = {comlevel: int ref}
type arg = lexarg

open Tokens

val eof = fn {comlevel} => EOF(0,0)

local
    val keywords = 
	[ ("case",      CASE)
	, ("datatype",  DATATYPE)
	, ("else",      ELSE)
	, ("end",       END)
	, ("fix",       FIX)
	, ("fn",        LAMBDA)
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
    fun lookupId(s,pos) =
	let val epos = pos + String.size s
	in  case HT.find keywords_table s of
		SOME token => token(pos,epos)
	      | NONE =>
		if Char.isUpper(String.sub(s,0))
		then CONS(s,pos,epos)
		else ID(s,pos,epos)
	end
end (*local*)

local
    val pos : int ref = ref 0
    val current : string list ref = ref []
in
    fun resetString p = (current := []; pos := p)
    fun addString s = current := s :: !current
    fun getString () = 
	let val s = String.concat (rev (!current))
	in  (s, !pos, !pos + String.size s)
	end
end (*local*)
%%

%header (functor MiniMLLexFun(structure Tokens : MiniML_TOKENS));
%arg ({comlevel});

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
<INITIAL>"("     => (LPAREN(yypos,yypos+1));
<INITIAL>")"     => (RPAREN(yypos,yypos+1));
<INITIAL>"["     => (LBRACKET(yypos,yypos+1));
<INITIAL>"]"     => (RBRACKET(yypos,yypos+1));
<INITIAL>"."     => (PERIOD(yypos,yypos+1));
<INITIAL>","     => (COMMA(yypos,yypos+1));
<INITIAL>";"     => (SEMICOLON(yypos,yypos+1));
<INITIAL>"+"     => (PLUS(yypos,yypos+1));
<INITIAL>"-"     => (MINUS(yypos,yypos+1));
<INITIAL>"*"     => (ASTERISK(yypos,yypos+1));
<INITIAL>"/"     => (DIV(yypos,yypos+1));
<INITIAL>"<="    => (LE(yypos,yypos+2));
<INITIAL>">="    => (GE(yypos,yypos+2));
<INITIAL>"="     => (EQ(yypos,yypos+1));
<INITIAL>"<"     => (LT(yypos,yypos+1));
<INITIAL>">"     => (GT(yypos,yypos+1));
<INITIAL>"::"    => (COLONCOLON(yypos,yypos+2));
<INITIAL>"=>"    => (DARROW(yypos,yypos+2));
<INITIAL>"->"    => (ARROW(yypos,yypos+2));
<INITIAL>"|"     => (BAR(yypos,yypos+1));
<INITIAL>"#"     => (HASH(yypos,yypos+1));
<INITIAL>"!"     => (BANG(yypos,yypos+1));
<INITIAL>":="    => (ASSIGN(yypos,yypos+2));
<INITIAL>"@"     => (AT(yypos,yypos+1));
<INITIAL>"^"     => (HAT(yypos,yypos+1));

<INITIAL>\"      => (resetString yypos; YYBEGIN STR; continue());

<INITIAL>"(*"    => (comlevel := 1; YYBEGIN COM; continue());

<INITIAL>{Identifier} => (lookupId(yytext,yypos));
<INITIAL>"'"{Identifier} => (TYID(String.extract(yytext,1,NONE),yypos,yypos+size yytext));
<INITIAL>{Integer} => (case Int.fromString yytext of
		        SOME i => INTLIT(i, yypos, yypos+size yytext)
		     |  NONE => (TextIO.print ("Error: illegal integer " ^ yytext ^ "\n");
				 continue())
                    );

<INITIAL>.       => (TextIO.print ("Error: unknown character " ^ yytext ^ " / #" ^ 
				   Int.toString(Char.ord (String.sub(yytext,0))) ^ "\n"); 
		     continue());
<STR>"\""        => (YYBEGIN INITIAL; STRING(getString()) );
<STR>"\\\""      => (addString "\""; continue());
<STR>{NotQuoteBackslash} => (addString yytext; continue());

<COM>"(*"        => (comlevel := !comlevel + 1; continue());
<COM>"*)"        => (comlevel := !comlevel - 1;
		     if !comlevel = 0 then YYBEGIN INITIAL
			              else ();
		     continue());
<COM>"\n"        => (continue());
<COM>.           => (continue());
