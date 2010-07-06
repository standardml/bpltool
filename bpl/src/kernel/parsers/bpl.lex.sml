type int = Int.int
functor BplLexFun(structure Tokens : Bpl_TOKENS)=
   struct
    structure UserDeclarations =
      struct
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
"\009\009\009\009\009\009\009\009\009\038\039\009\009\038\009\009\
\\009\009\009\009\009\009\009\009\009\009\009\009\009\009\009\009\
\\038\009\009\009\009\037\009\036\033\032\031\009\030\028\027\026\
\\024\024\024\024\024\024\024\024\024\024\023\009\022\021\020\009\
\\009\014\014\014\014\014\014\014\014\014\014\014\014\014\014\014\
\\014\014\014\014\014\014\014\014\014\014\014\019\009\018\009\017\
\\009\014\014\014\014\014\014\014\014\014\014\014\014\014\014\016\
\\014\014\014\014\014\014\014\014\014\014\014\013\011\010\009\009\
\\009"
),
 (3, 
"\040\040\040\040\040\040\040\040\040\040\045\040\040\040\040\040\
\\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\
\\040\040\040\040\040\040\040\040\043\040\041\040\040\040\040\040\
\\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\
\\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\
\\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\
\\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\
\\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\040\
\\040"
),
 (5, 
"\046\046\046\046\046\046\046\046\046\046\047\046\046\046\046\046\
\\046\046\046\046\046\046\046\046\046\046\046\046\046\046\046\046\
\\046\046\046\046\046\046\046\046\046\046\046\046\046\046\046\046\
\\046\046\046\046\046\046\046\046\046\046\046\046\046\046\046\046\
\\046\046\046\046\046\046\046\046\046\046\046\046\046\046\046\046\
\\046\046\046\046\046\046\046\046\046\046\046\046\046\046\046\046\
\\046\046\046\046\046\046\046\046\046\046\046\046\046\046\046\046\
\\046\046\046\046\046\046\046\046\046\046\046\046\046\046\046\046\
\\046"
),
 (11, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\012\000\000\000\
\\000"
),
 (14, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\015\000\000\000\000\000\000\000\000\
\\015\015\015\015\015\015\015\015\015\015\000\000\000\000\000\000\
\\000\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\
\\015\015\015\015\015\015\015\015\015\015\015\000\000\000\000\015\
\\000\015\015\015\015\015\015\015\015\015\015\015\015\015\015\015\
\\015\015\015\015\015\015\015\015\015\015\015\000\000\000\000\000\
\\000"
),
 (24, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\025\025\025\025\025\025\025\025\025\025\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (28, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\029\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (33, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\035\034\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (41, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\042\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (43, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\044\000\000\000\000\000\
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
{fin = [], trans = 0},
{fin = [], trans = 0},
{fin = [(N 63)], trans = 0},
{fin = [(N 24),(N 63)], trans = 0},
{fin = [(N 32),(N 63)], trans = 11},
{fin = [(N 12)], trans = 0},
{fin = [(N 22),(N 63)], trans = 0},
{fin = [(N 58),(N 63)], trans = 14},
{fin = [(N 58)], trans = 14},
{fin = [(N 44),(N 58),(N 63)], trans = 14},
{fin = [(N 48),(N 58),(N 63)], trans = 14},
{fin = [(N 20),(N 63)], trans = 0},
{fin = [(N 18),(N 63)], trans = 0},
{fin = [(N 28),(N 63)], trans = 0},
{fin = [(N 38),(N 63)], trans = 0},
{fin = [(N 26),(N 63)], trans = 0},
{fin = [(N 36),(N 63)], trans = 0},
{fin = [(N 61),(N 63)], trans = 24},
{fin = [(N 61)], trans = 24},
{fin = [(N 30),(N 63)], trans = 0},
{fin = [(N 40),(N 63)], trans = 0},
{fin = [(N 63)], trans = 28},
{fin = [(N 9)], trans = 0},
{fin = [(N 34),(N 63)], trans = 0},
{fin = [(N 42),(N 63)], trans = 0},
{fin = [(N 16),(N 63)], trans = 0},
{fin = [(N 14),(N 63)], trans = 33},
{fin = [(N 51)], trans = 0},
{fin = [(N 6)], trans = 0},
{fin = [(N 46),(N 63)], trans = 0},
{fin = [(N 53),(N 63)], trans = 0},
{fin = [(N 1),(N 63)], trans = 0},
{fin = [(N 3)], trans = 0},
{fin = [(N 73)], trans = 0},
{fin = [(N 73)], trans = 41},
{fin = [(N 69)], trans = 0},
{fin = [(N 73)], trans = 43},
{fin = [(N 66)], trans = 0},
{fin = [(N 71)], trans = 0},
{fin = [(N 77)], trans = 0},
{fin = [(N 75)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val COM = STARTSTATE 3;
val INITIAL = STARTSTATE 1;
val LINECOM = STARTSTATE 5;
val STR = STARTSTATE 7;

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
| 12 => (tok arg DPAR(yypos,yypos+2))
| 14 => (tok arg LPAR(yypos,yypos+1))
| 16 => (tok arg RPAR(yypos,yypos+1))
| 18 => (tok arg LBRK(yypos,yypos+1))
| 20 => (tok arg RBRK(yypos,yypos+1))
| 22 => (tok arg LBRC(yypos,yypos+1))
| 24 => (tok arg RBRC(yypos,yypos+1))
| 26 => (tok arg LANG(yypos,yypos+1))
| 28 => (tok arg RANG(yypos,yypos+1))
| 3 => (continue ())
| 30 => (tok arg SLASH(yypos,yypos+1))
| 32 => (tok arg PAR(yypos,yypos+1))
| 34 => (tok arg COMMA(yypos,yypos+1))
| 36 => (tok arg COLON(yypos,yypos+1))
| 38 => (tok arg EQUAL(yypos,yypos+1))
| 40 => (tok arg DOT(yypos,yypos+1))
| 42 => (tok arg STAR(yypos,yypos+1))
| 44 => (tok arg COMP(yypos,yypos+1))
| 46 => (tok arg QUOT(yypos,yypos+1))
| 48 => (tok arg UNDERSCORE(yypos,yypos+1))
| 51 => (comlevel := 1; YYBEGIN COM; continue())
| 53 => (YYBEGIN LINECOM; continue())
| 58 => let val yytext=yymktext() in lookupId arg (yytext,yypos) end
| 6 => (tok arg BARREN(yypos,yypos+2))
| 61 => let val yytext=yymktext() in case Int.fromString yytext of
	 SOME i => INT(i, mkPos arg yypos, mkPos arg (yypos+size yytext))
       | NONE => (error("Error: illegal integer "^yytext, 
			mkPos arg yypos, mkPos arg (yypos+size yytext));
		  continue())
     end
| 63 => let val yytext=yymktext() in error("Error: unknown character " ^ yytext ^ " / #" ^ 
			   Int.toString(Char.ord (String.sub(yytext,0))),
			   mkPos arg yypos, mkPos arg (yypos+1));
		     continue() end
| 66 => (comlevel := !comlevel + 1; continue())
| 69 => (comlevel := !comlevel - 1;
		     if !comlevel = 0 then YYBEGIN INITIAL
			              else ();
		     continue())
| 71 => (continue())
| 73 => (continue())
| 75 => (YYBEGIN INITIAL; continue())
| 77 => (continue())
| 9 => (tok arg ARROW(yypos,yypos+2))
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
