(* Copyright (c) 2007  The BPL Group at the IT University of Copenhagen
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

fun error (msg, {pos=p1,src={file=f}}, {pos=p2,src=s2}) =
    let open Pretty
	val err = SourceLocation.ppSourceLocation f (p1,p2) [ppString msg]
    in  ppPrint err (plainOutput ("(*","*)")) TextIO.stdErr
    end

type pos = {pos:int,src:{file:string}}
type svalue = Tokens.svalue
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token
type lexarg = {src: {file:string}}
type arg = lexarg

open Tokens

val comlevel : int ref = ref 0

fun mkPos (arg as {src}) p = {pos=p,src=src}
fun tok arg token (p1, p2) = token (mkPos arg p1, mkPos arg p2)
val eof = fn arg => tok arg EOF (0,0)

local
    val keywords = 
	[ ("active",   ACTIVE)
	, ("atomic",   ATOMIC)
	, ("end",      END)
	, ("passive",  PASSIVE)
	, ("rule",     RULE)
	, ("sig",      SIG)
	, ("signature",SIGNATURE)
        , ("using",    USING)
	, ("val",      VAL)
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
		then NAME(s, mkPos arg p1, mkPos arg p2)
		else ID(s, mkPos arg p1, mkPos arg p2)
	end
end (*local*)

%%

%header (functor BplLexFun(structure Tokens : Bpl_TOKENS));
%arg (arg);

%s COM LINECOM STR;

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
<INITIAL>"()"    => (tok arg BARREN(yypos,yypos+2));
<INITIAL>"->"    => (tok arg ARROW(yypos,yypos+2));
<INITIAL>"||"    => (tok arg DPAR(yypos,yypos+2));
<INITIAL>"("     => (tok arg LPAR(yypos,yypos+1));
<INITIAL>")"     => (tok arg RPAR(yypos,yypos+1));
<INITIAL>"["     => (tok arg LBRK(yypos,yypos+1));
<INITIAL>"]"     => (tok arg RBRK(yypos,yypos+1));
<INITIAL>"{"     => (tok arg LBRC(yypos,yypos+1));
<INITIAL>"}"     => (tok arg RBRC(yypos,yypos+1));
<INITIAL>"<"     => (tok arg LANG(yypos,yypos+1));
<INITIAL>">"     => (tok arg RANG(yypos,yypos+1));
<INITIAL>"/"     => (tok arg SLASH(yypos,yypos+1));
<INITIAL>"|"     => (tok arg PAR(yypos,yypos+1));
<INITIAL>","     => (tok arg COMMA(yypos,yypos+1));
<INITIAL>":"     => (tok arg COLON(yypos,yypos+1));
<INITIAL>"="     => (tok arg EQUAL(yypos,yypos+1));
<INITIAL>"."     => (tok arg DOT(yypos,yypos+1));
<INITIAL>"*"     => (tok arg STAR(yypos,yypos+1));
<INITIAL>"o"     => (tok arg COMP(yypos,yypos+1));
<INITIAL>"_"     => (tok arg UNDERSCORE(yypos,yypos+1));

<INITIAL>"(*"    => (comlevel := 1; YYBEGIN COM; continue());
<INITIAL>"%"     => (YYBEGIN LINECOM; continue());

<INITIAL>{Identifier} => (lookupId arg (yytext,yypos));

<INITIAL>{Integer} => 
    (case Int.fromString yytext of
	 SOME i => INT(i, mkPos arg yypos, mkPos arg (yypos+size yytext))
       | NONE => (error("Error: illegal integer "^yytext, 
			mkPos arg yypos, mkPos arg (yypos+size yytext));
		  continue())
    );

<INITIAL>.       => (error("Error: unknown character " ^ yytext ^ " / #" ^ 
			   Int.toString(Char.ord (String.sub(yytext,0))),
			   mkPos arg yypos, mkPos arg (yypos+1));
		     continue());

<COM>"(*"        => (comlevel := !comlevel + 1; continue());
<COM>"*)"        => (comlevel := !comlevel - 1;
		     if !comlevel = 0 then YYBEGIN INITIAL
			              else ();
		     continue());
<COM>"\n"        => (continue());
<COM>.           => (continue());

<LINECOM>"\n"    => (YYBEGIN INITIAL; continue());
<LINECOM>.       => (continue());
