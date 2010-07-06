type int = Int.int
functor MiniMLLexFun(structure Tokens : MiniML_TOKENS)=
   struct
    structure UserDeclarations =
      struct
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
 * @version $LastChangedRevision: 1245 $
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
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\007\007\007\007\007\007\007\007\007\042\043\007\007\042\007\007\
\\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\007\
\\042\041\040\039\007\007\007\037\035\034\033\032\031\029\028\027\
\\025\025\025\025\025\025\025\025\025\025\022\021\019\017\015\007\
\\014\009\009\009\009\009\009\009\009\009\009\009\009\009\009\009\
\\009\009\009\009\009\009\009\009\009\009\009\013\007\012\011\009\
\\007\009\009\009\009\009\009\009\009\009\009\009\009\009\009\009\
\\009\009\009\009\009\009\009\009\009\009\009\007\008\007\007\007\
\\007"
),
 (3, 
"\044\044\044\044\044\044\044\044\044\044\049\044\044\044\044\044\
\\044\044\044\044\044\044\044\044\044\044\044\044\044\044\044\044\
\\044\044\044\044\044\044\044\044\047\044\045\044\044\044\044\044\
\\044\044\044\044\044\044\044\044\044\044\044\044\044\044\044\044\
\\044\044\044\044\044\044\044\044\044\044\044\044\044\044\044\044\
\\044\044\044\044\044\044\044\044\044\044\044\044\044\044\044\044\
\\044\044\044\044\044\044\044\044\044\044\044\044\044\044\044\044\
\\044\044\044\044\044\044\044\044\044\044\044\044\044\044\044\044\
\\044"
),
 (5, 
"\050\050\050\050\050\050\050\050\050\050\050\050\050\050\050\050\
\\050\050\050\050\050\050\050\050\050\050\050\050\050\050\050\050\
\\050\050\053\050\050\050\050\050\050\050\050\050\050\050\050\050\
\\050\050\050\050\050\050\050\050\050\050\050\050\050\050\050\050\
\\050\050\050\050\050\050\050\050\050\050\050\050\050\050\050\050\
\\050\050\050\050\050\050\050\050\050\050\050\050\051\050\050\050\
\\050\050\050\050\050\050\050\050\050\050\050\050\050\050\050\050\
\\050\050\050\050\050\050\050\050\050\050\050\050\050\050\050\050\
\\050"
),
 (9, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\010\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\000\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\010\
\\000\010\010\010\010\010\010\010\010\010\010\010\010\010\010\010\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000"
),
 (15, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (17, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\018\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (19, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\020\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (22, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\024\000\000\023\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (25, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\026\026\026\026\026\026\026\026\026\026\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (29, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\030\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (35, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\036\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (37, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\038\038\038\038\038\038\038\038\038\038\038\038\038\038\038\
\\038\038\038\038\038\038\038\038\038\038\038\000\000\000\000\038\
\\000\038\038\038\038\038\038\038\038\038\038\038\038\038\038\038\
\\038\038\038\038\038\038\038\038\038\038\038\000\000\000\000\000\
\\000"
),
 (38, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\038\000\000\000\000\000\000\000\000\
\\038\038\038\038\038\038\038\038\038\038\000\000\000\000\000\000\
\\000\038\038\038\038\038\038\038\038\038\038\038\038\038\038\038\
\\038\038\038\038\038\038\038\038\038\038\038\000\000\000\000\038\
\\000\038\038\038\038\038\038\038\038\038\038\038\038\038\038\038\
\\038\038\038\038\038\038\038\038\038\038\038\000\000\000\000\000\
\\000"
),
 (45, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\046\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (47, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\048\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (51, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\052\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
(0, "")]
fun f x = x 
val s = map f (rev (tl (rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [], trans = 3},
{fin = [], trans = 3},
{fin = [], trans = 5},
{fin = [], trans = 5},
{fin = [(N 82)], trans = 0},
{fin = [(N 50),(N 82)], trans = 0},
{fin = [(N 71),(N 82)], trans = 9},
{fin = [(N 71)], trans = 9},
{fin = [(N 61),(N 82)], trans = 0},
{fin = [(N 11),(N 82)], trans = 0},
{fin = [(N 9),(N 82)], trans = 0},
{fin = [(N 59),(N 82)], trans = 0},
{fin = [(N 39),(N 82)], trans = 15},
{fin = [(N 33)], trans = 0},
{fin = [(N 35),(N 82)], trans = 17},
{fin = [(N 45)], trans = 0},
{fin = [(N 37),(N 82)], trans = 19},
{fin = [(N 30)], trans = 0},
{fin = [(N 17),(N 82)], trans = 0},
{fin = [(N 19),(N 82)], trans = 22},
{fin = [(N 57)], trans = 0},
{fin = [(N 42)], trans = 0},
{fin = [(N 80),(N 82)], trans = 25},
{fin = [(N 80)], trans = 25},
{fin = [(N 27),(N 82)], trans = 0},
{fin = [(N 13),(N 82)], trans = 0},
{fin = [(N 23),(N 82)], trans = 29},
{fin = [(N 48)], trans = 0},
{fin = [(N 15),(N 82)], trans = 0},
{fin = [(N 21),(N 82)], trans = 0},
{fin = [(N 25),(N 82)], trans = 0},
{fin = [(N 7),(N 82)], trans = 0},
{fin = [(N 5),(N 82)], trans = 35},
{fin = [(N 66)], trans = 0},
{fin = [(N 82)], trans = 37},
{fin = [(N 77)], trans = 38},
{fin = [(N 52),(N 82)], trans = 0},
{fin = [(N 63),(N 82)], trans = 0},
{fin = [(N 54),(N 82)], trans = 0},
{fin = [(N 1),(N 82)], trans = 0},
{fin = [(N 3)], trans = 0},
{fin = [(N 99)], trans = 0},
{fin = [(N 99)], trans = 45},
{fin = [(N 95)], trans = 0},
{fin = [(N 99)], trans = 47},
{fin = [(N 92)], trans = 0},
{fin = [(N 97)], trans = 0},
{fin = [(N 89)], trans = 0},
{fin = [], trans = 51},
{fin = [(N 87)], trans = 0},
{fin = [(N 84)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val COM = STARTSTATE 3;
val INITIAL = STARTSTATE 1;
val STR = STARTSTATE 5;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

type int = Int.int
fun makeLexer (yyinput: int -> string) =
let	val yygone0:int= ~1
	val yyb = ref "\n" 		(* buffer *)
	val yybl: int ref = ref 1		(*buffer length *)
	val yybufpos: int ref = ref 1		(* location of next character to use *)
	val yygone: int ref = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin: int ref = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex (yyarg as (arg)) =
let fun continue() : Internal.result = 
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0: int) =
	let fun action (i: int,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = String.substring(!yyb,i0,i-i0)
			     val yypos: int = i0+ !yygone
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => (continue ())
| 11 => (tok arg RBRACKET(yypos,yypos+1))
| 13 => (tok arg PERIOD(yypos,yypos+1))
| 15 => (tok arg COMMA(yypos,yypos+1))
| 17 => (tok arg SEMICOLON(yypos,yypos+1))
| 19 => (tok arg COLON(yypos,yypos+1))
| 21 => (tok arg PLUS(yypos,yypos+1))
| 23 => (tok arg MINUS(yypos,yypos+1))
| 25 => (tok arg ASTERISK(yypos,yypos+1))
| 27 => (tok arg DIV(yypos,yypos+1))
| 3 => (continue ())
| 30 => (tok arg LE(yypos,yypos+2))
| 33 => (tok arg GE(yypos,yypos+2))
| 35 => (tok arg EQ(yypos,yypos+1))
| 37 => (tok arg LT(yypos,yypos+1))
| 39 => (tok arg GT(yypos,yypos+1))
| 42 => (tok arg COLONCOLON(yypos,yypos+2))
| 45 => (tok arg DARROW(yypos,yypos+2))
| 48 => (tok arg ARROW(yypos,yypos+2))
| 5 => (tok arg LPAREN(yypos,yypos+1))
| 50 => (tok arg BAR(yypos,yypos+1))
| 52 => (tok arg HASH(yypos,yypos+1))
| 54 => (tok arg BANG(yypos,yypos+1))
| 57 => (tok arg ASSIGN(yypos,yypos+2))
| 59 => (tok arg AT(yypos,yypos+1))
| 61 => (tok arg HAT(yypos,yypos+1))
| 63 => (resetString yypos; YYBEGIN STR; continue())
| 66 => (comlevel := 1; YYBEGIN COM; continue())
| 7 => (tok arg RPAREN(yypos,yypos+1))
| 71 => let val yytext=yymktext() in lookupId arg (yytext,yypos) end
| 77 => let val yytext=yymktext() in TYID(String.extract(yytext,1,NONE),
	  mkPos arg yypos,mkPos arg (yypos+size yytext)) end
| 80 => let val yytext=yymktext() in case Int.fromString yytext of
	 SOME i => INTLIT(i, mkPos arg yypos, mkPos arg (yypos+size yytext))
       | NONE => (error("Error: illegal integer "^yytext, 
			mkPos arg yypos, mkPos arg (yypos+size yytext));
		  continue())
     end
| 82 => let val yytext=yymktext() in error("Error: unknown character " ^ yytext ^ " / #" ^ 
			   Int.toString(Char.ord (String.sub(yytext,0))),
			   mkPos arg yypos, mkPos arg (yypos+1));
		     continue() end
| 84 => (YYBEGIN INITIAL; getString arg )
| 87 => (addString "\""; continue())
| 89 => let val yytext=yymktext() in addString yytext; continue() end
| 9 => (tok arg LBRACKET(yypos,yypos+1))
| 92 => (comlevel := !comlevel + 1; continue())
| 95 => (comlevel := !comlevel - 1;
		     if !comlevel = 0 then YYBEGIN INITIAL
			              else ();
		     continue())
| 97 => (continue())
| 99 => (continue())
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Vector.sub (Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (String.size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof yyarg
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := String.substring(!yyb,i0,l-i0)^newchars;
		     yygone := !yygone+i0;
		     yybl := String.size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord (CharVector.sub (!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord (CharVector.sub (trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if String.substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
in continue end
  in lex
  end
end
