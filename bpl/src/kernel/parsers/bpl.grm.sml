
functor BplLrValsFun
		   (structure Token : TOKEN
		    structure BPLTerm : BPLTERM)
	 = 
struct
structure ParserData=
struct
structure Header = 
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

structure T = BPLTerm
type id = T.id
type ctrldef = id * T.kind * int * int
type bigraph = T.bigraph

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\010\000\003\000\065\000\004\000\009\000\000\000\
\\001\000\002\000\010\000\004\000\009\000\000\000\
\\001\000\002\000\020\000\000\000\
\\001\000\002\000\045\000\003\000\044\000\004\000\043\000\005\000\042\000\
\\007\000\041\000\009\000\040\000\011\000\039\000\013\000\038\000\
\\014\000\037\000\022\000\036\000\000\000\
\\001\000\002\000\054\000\003\000\044\000\004\000\043\000\005\000\042\000\
\\011\000\039\000\013\000\038\000\000\000\
\\001\000\002\000\062\000\000\000\
\\001\000\002\000\062\000\005\000\090\000\000\000\
\\001\000\002\000\066\000\000\000\
\\001\000\002\000\075\000\009\000\040\000\000\000\
\\001\000\002\000\114\000\000\000\
\\001\000\003\000\094\000\000\000\
\\001\000\003\000\112\000\000\000\
\\001\000\005\000\073\000\000\000\
\\001\000\005\000\105\000\000\000\
\\001\000\006\000\088\000\000\000\
\\001\000\006\000\107\000\016\000\106\000\000\000\
\\001\000\006\000\110\000\000\000\
\\001\000\006\000\115\000\000\000\
\\001\000\006\000\116\000\000\000\
\\001\000\008\000\087\000\000\000\
\\001\000\008\000\113\000\000\000\
\\001\000\008\000\118\000\000\000\
\\001\000\010\000\086\000\000\000\
\\001\000\010\000\117\000\000\000\
\\001\000\012\000\084\000\000\000\
\\001\000\012\000\085\000\000\000\
\\001\000\014\000\071\000\000\000\
\\001\000\014\000\102\000\000\000\
\\001\000\016\000\072\000\000\000\
\\001\000\017\000\029\000\000\000\
\\001\000\018\000\012\000\000\000\
\\001\000\018\000\025\000\000\000\
\\001\000\018\000\026\000\000\000\
\\001\000\019\000\083\000\000\000\
\\001\000\022\000\081\000\000\000\
\\001\000\026\000\051\000\027\000\050\000\029\000\049\000\000\000\
\\001\000\028\000\028\000\000\000\
\\001\000\030\000\016\000\034\000\015\000\000\000\
\\001\000\031\000\018\000\000\000\
\\001\000\033\000\007\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\122\000\032\000\005\000\000\000\
\\123\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\\126\000\004\000\024\000\000\000\
\\127\000\000\000\
\\128\000\000\000\
\\129\000\000\000\
\\130\000\000\000\
\\131\000\000\000\
\\132\000\030\000\016\000\034\000\015\000\000\000\
\\133\000\000\000\
\\134\000\000\000\
\\135\000\000\000\
\\136\000\019\000\059\000\020\000\058\000\021\000\057\000\024\000\056\000\
\\025\000\055\000\000\000\
\\137\000\000\000\
\\138\000\000\000\
\\139\000\000\000\
\\140\000\000\000\
\\141\000\002\000\054\000\003\000\044\000\004\000\043\000\005\000\042\000\
\\011\000\039\000\013\000\038\000\000\000\
\\142\000\019\000\059\000\021\000\057\000\025\000\055\000\000\000\
\\143\000\019\000\059\000\021\000\057\000\025\000\055\000\000\000\
\\144\000\019\000\059\000\021\000\057\000\000\000\
\\145\000\000\000\
\\146\000\000\000\
\\147\000\000\000\
\\148\000\000\000\
\\149\000\007\000\070\000\023\000\069\000\000\000\
\\150\000\000\000\
\\151\000\000\000\
\\152\000\000\000\
\\153\000\000\000\
\\154\000\000\000\
\\155\000\000\000\
\\156\000\000\000\
\\157\000\000\000\
\\158\000\000\000\
\\158\000\014\000\071\000\000\000\
\\159\000\000\000\
\\160\000\007\000\100\000\000\000\
\\161\000\000\000\
\\162\000\000\000\
\\163\000\002\000\062\000\000\000\
\\164\000\000\000\
\\165\000\015\000\082\000\000\000\
\\166\000\000\000\
\\167\000\015\000\052\000\000\000\
\\168\000\000\000\
\\169\000\002\000\092\000\000\000\
\\170\000\000\000\
\\171\000\009\000\109\000\000\000\
\\172\000\000\000\
\\173\000\000\000\
\\174\000\000\000\
\"
val actionRowNumbers =
"\043\000\043\000\040\000\002\000\
\\042\000\002\000\031\000\095\000\
\\096\000\038\000\039\000\053\000\
\\041\000\003\000\002\000\044\000\
\\047\000\054\000\032\000\033\000\
\\047\000\037\000\030\000\004\000\
\\004\000\046\000\045\000\036\000\
\\089\000\061\000\068\000\062\000\
\\057\000\056\000\085\000\085\000\
\\076\000\001\000\008\000\085\000\
\\004\000\070\000\078\000\080\000\
\\055\000\029\000\013\000\051\000\
\\052\000\050\000\009\000\069\000\
\\079\000\005\000\005\000\005\000\
\\005\000\005\000\084\000\035\000\
\\087\000\034\000\025\000\026\000\
\\023\000\020\000\015\000\007\000\
\\085\000\091\000\004\000\011\000\
\\088\000\027\000\065\000\063\000\
\\066\000\064\000\067\000\004\000\
\\006\000\004\000\082\000\082\000\
\\028\000\004\000\077\000\071\000\
\\006\000\014\000\090\000\094\000\
\\016\000\059\000\086\000\058\000\
\\081\000\075\000\085\000\074\000\
\\093\000\060\000\017\000\085\000\
\\012\000\049\000\021\000\010\000\
\\072\000\018\000\019\000\083\000\
\\024\000\022\000\048\000\092\000\
\\073\000\000\000"
val gotoT =
"\
\\001\000\117\000\005\000\002\000\006\000\001\000\000\000\
\\005\000\004\000\006\000\001\000\000\000\
\\000\000\
\\021\000\006\000\000\000\
\\000\000\
\\021\000\009\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\012\000\003\000\011\000\000\000\
\\004\000\015\000\000\000\
\\002\000\017\000\003\000\011\000\000\000\
\\000\000\
\\000\000\
\\021\000\019\000\000\000\
\\000\000\
\\007\000\021\000\008\000\020\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\025\000\008\000\020\000\000\000\
\\000\000\
\\000\000\
\\010\000\033\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\018\000\029\000\019\000\028\000\000\000\
\\010\000\045\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\018\000\029\000\019\000\028\000\020\000\044\000\000\000\
\\000\000\
\\000\000\
\\009\000\046\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\013\000\051\000\000\000\
\\000\000\
\\000\000\
\\016\000\059\000\017\000\058\000\000\000\
\\016\000\061\000\017\000\058\000\000\000\
\\000\000\
\\021\000\062\000\000\000\
\\000\000\
\\016\000\065\000\017\000\058\000\000\000\
\\010\000\066\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\018\000\029\000\019\000\028\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\018\000\072\000\019\000\028\000\000\000\
\\000\000\
\\000\000\
\\011\000\074\000\012\000\031\000\013\000\030\000\000\000\
\\011\000\075\000\012\000\031\000\013\000\030\000\000\000\
\\011\000\076\000\012\000\031\000\013\000\030\000\000\000\
\\011\000\077\000\012\000\031\000\013\000\030\000\000\000\
\\011\000\078\000\012\000\031\000\013\000\030\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\087\000\000\000\
\\016\000\089\000\017\000\058\000\000\000\
\\000\000\
\\010\000\091\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\018\000\029\000\019\000\028\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\093\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\018\000\029\000\019\000\028\000\000\000\
\\017\000\094\000\000\000\
\\010\000\095\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\018\000\029\000\019\000\028\000\000\000\
\\014\000\097\000\015\000\096\000\000\000\
\\014\000\099\000\015\000\096\000\000\000\
\\000\000\
\\010\000\101\000\011\000\032\000\012\000\031\000\013\000\030\000\
\\018\000\029\000\019\000\028\000\000\000\
\\000\000\
\\000\000\
\\017\000\102\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\016\000\106\000\017\000\058\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\016\000\109\000\017\000\058\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 118
val numrules = 55
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = { pos:int,src:{ file:string }  } 
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | NAME of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string) | NAMEID of unit ->  (string)
 | Rule of unit ->  (bigraph*bigraph) | Wire of unit ->  (T.wire)
 | Wiring of unit ->  (T.wire list) | Edges of unit ->  (T.id list)
 | EdgeList of unit ->  (T.id list) | Ports of unit ->  (T.id list)
 | PortsOpt of unit ->  (T.id list)
 | AtBigraph of unit ->  (T.bigraph)
 | JuxtBigraph of unit ->  (T.bigraph)
 | OpBigraph of unit ->  (T.bigraph) | Bigraph of unit ->  (T.bigraph)
 | CtrlKind of unit ->  (T.kind) | CtrlDef of unit ->  (ctrldef)
 | CtrlDefs of unit ->  (ctrldef list) | SigDef of unit ->  (T.sigdef)
 | SigDefs of unit ->  (T.sigdef list) | Sig of unit ->  (T.sign)
 | AtDec of unit ->  (T.dec) | Dec of unit ->  (T.dec list)
 | Prog of unit ->  (T.prog)
end
type svalue = MlyValue.svalue
type result = T.prog
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 25) => true | (T 26) => true | (T 27) => true | (T 28) => true
 | (T 29) => true | (T 30) => true | (T 31) => true | (T 32) => true
 | (T 33) => true | _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "NAME"
  | (T 4) => "LPAR"
  | (T 5) => "RPAR"
  | (T 6) => "LANG"
  | (T 7) => "RANG"
  | (T 8) => "LBRC"
  | (T 9) => "RBRC"
  | (T 10) => "LBRK"
  | (T 11) => "RBRK"
  | (T 12) => "BARREN"
  | (T 13) => "SLASH"
  | (T 14) => "COMMA"
  | (T 15) => "ARROW"
  | (T 16) => "COLON"
  | (T 17) => "EQUAL"
  | (T 18) => "DOT"
  | (T 19) => "STAR"
  | (T 20) => "COMP"
  | (T 21) => "QUOT"
  | (T 22) => "UNDERSCORE"
  | (T 23) => "DPAR"
  | (T 24) => "PAR"
  | (T 25) => "ACTIVE"
  | (T 26) => "ATOMIC"
  | (T 27) => "END"
  | (T 28) => "PASSIVE"
  | (T 29) => "RULE"
  | (T 30) => "SIG"
  | (T 31) => "SIGNATURE"
  | (T 32) => "USING"
  | (T 33) => "VAL"
  | (T 34) => "STATE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28)
 $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21)
 $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14)
 $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7)
 $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.Dec Dec1, _, Dec1right)) :: ( _, ( 
MlyValue.NAMEID NAMEID1, _, _)) :: _ :: ( _, ( MlyValue.SigDefs 
SigDefs1, SigDefs1left, _)) :: rest671)) => let val  result = 
MlyValue.Prog (fn _ => let val  (SigDefs as SigDefs1) = SigDefs1 ()
 val  (NAMEID as NAMEID1) = NAMEID1 ()
 val  (Dec as Dec1) = Dec1 ()
 in ( (SigDefs, NAMEID, Dec) )
end)
 in ( LrTable.NT 0, ( result, SigDefs1left, Dec1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.SigDefs SigDefs1, _, SigDefs1right)) :: ( _,
 ( MlyValue.SigDef SigDef1, SigDef1left, _)) :: rest671)) => let val  
result = MlyValue.SigDefs (fn _ => let val  (SigDef as SigDef1) = 
SigDef1 ()
 val  (SigDefs as SigDefs1) = SigDefs1 ()
 in ( SigDef :: SigDefs )
end)
 in ( LrTable.NT 4, ( result, SigDef1left, SigDefs1right), rest671)

end
|  ( 2, ( rest671)) => let val  result = MlyValue.SigDefs (fn _ => (
 [] ))
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 3, ( ( _, ( MlyValue.Sig Sig1, _, Sig1right)) :: _ :: ( _, ( 
MlyValue.NAMEID NAMEID1, _, _)) :: ( _, ( _, SIGNATURE1left, _)) :: 
rest671)) => let val  result = MlyValue.SigDef (fn _ => let val  (
NAMEID as NAMEID1) = NAMEID1 ()
 val  (Sig as Sig1) = Sig1 ()
 in ( (NAMEID, Sig) )
end)
 in ( LrTable.NT 5, ( result, SIGNATURE1left, Sig1right), rest671)
end
|  ( 4, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.CtrlDefs 
CtrlDefs1, _, _)) :: ( _, ( _, SIG1left, _)) :: rest671)) => let val  
result = MlyValue.Sig (fn _ => let val  (CtrlDefs as CtrlDefs1) = 
CtrlDefs1 ()
 in ( CtrlDefs )
end)
 in ( LrTable.NT 3, ( result, SIG1left, END1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.CtrlDefs CtrlDefs1, _, CtrlDefs1right)) :: (
 _, ( MlyValue.CtrlDef CtrlDef1, CtrlDef1left, _)) :: rest671)) => let
 val  result = MlyValue.CtrlDefs (fn _ => let val  (CtrlDef as 
CtrlDef1) = CtrlDef1 ()
 val  (CtrlDefs as CtrlDefs1) = CtrlDefs1 ()
 in ( CtrlDef :: CtrlDefs )
end)
 in ( LrTable.NT 6, ( result, CtrlDef1left, CtrlDefs1right), rest671)

end
|  ( 6, ( rest671)) => let val  result = MlyValue.CtrlDefs (fn _ => (
 [] ))
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 7, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.INT INT2, _, _)
) :: _ :: ( _, ( MlyValue.INT INT1, _, _)) :: _ :: ( _, ( 
MlyValue.CtrlKind CtrlKind1, _, _)) :: _ :: ( _, ( MlyValue.NAME NAME1
, NAME1left, _)) :: rest671)) => let val  result = MlyValue.CtrlDef
 (fn _ => let val  (NAME as NAME1) = NAME1 ()
 val  (CtrlKind as CtrlKind1) = CtrlKind1 ()
 val  INT1 = INT1 ()
 val  INT2 = INT2 ()
 in ( (NAME, CtrlKind, INT1, INT2) )
end)
 in ( LrTable.NT 7, ( result, NAME1left, RPAR1right), rest671)
end
|  ( 8, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.INT INT1, _, _)
) :: _ :: ( _, ( MlyValue.CtrlKind CtrlKind1, _, _)) :: _ :: ( _, ( 
MlyValue.NAME NAME1, NAME1left, _)) :: rest671)) => let val  result = 
MlyValue.CtrlDef (fn _ => let val  (NAME as NAME1) = NAME1 ()
 val  (CtrlKind as CtrlKind1) = CtrlKind1 ()
 val  (INT as INT1) = INT1 ()
 in ( (NAME, CtrlKind, 0, INT) )
end)
 in ( LrTable.NT 7, ( result, NAME1left, RPAR1right), rest671)
end
|  ( 9, ( ( _, ( _, ACTIVE1left, ACTIVE1right)) :: rest671)) => let
 val  result = MlyValue.CtrlKind (fn _ => ( T.Active ))
 in ( LrTable.NT 8, ( result, ACTIVE1left, ACTIVE1right), rest671)
end
|  ( 10, ( ( _, ( _, PASSIVE1left, PASSIVE1right)) :: rest671)) => let
 val  result = MlyValue.CtrlKind (fn _ => ( T.Passive ))
 in ( LrTable.NT 8, ( result, PASSIVE1left, PASSIVE1right), rest671)

end
|  ( 11, ( ( _, ( _, ATOMIC1left, ATOMIC1right)) :: rest671)) => let
 val  result = MlyValue.CtrlKind (fn _ => ( T.Atomic ))
 in ( LrTable.NT 8, ( result, ATOMIC1left, ATOMIC1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.AtDec AtDec1, AtDec1left, AtDec1right)) :: 
rest671)) => let val  result = MlyValue.Dec (fn _ => let val  (AtDec
 as AtDec1) = AtDec1 ()
 in ( [AtDec] )
end)
 in ( LrTable.NT 1, ( result, AtDec1left, AtDec1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.Dec Dec1, _, Dec1right)) :: ( _, ( 
MlyValue.AtDec AtDec1, AtDec1left, _)) :: rest671)) => let val  result
 = MlyValue.Dec (fn _ => let val  (AtDec as AtDec1) = AtDec1 ()
 val  (Dec as Dec1) = Dec1 ()
 in ( AtDec :: Dec )
end)
 in ( LrTable.NT 1, ( result, AtDec1left, Dec1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.Rule Rule1, _, Rule1right)) :: _ :: ( _, ( 
MlyValue.NAMEID NAMEID1, _, _)) :: ( _, ( _, RULE1left, _)) :: rest671
)) => let val  result = MlyValue.AtDec (fn _ => let val  (NAMEID as 
NAMEID1) = NAMEID1 ()
 val  (Rule as Rule1) = Rule1 ()
 in ( T.Rul(NAMEID, Rule) )
end)
 in ( LrTable.NT 2, ( result, RULE1left, Rule1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.Bigraph Bigraph1, _, Bigraph1right)) :: _
 :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, VAL1left, _)) :: 
rest671)) => let val  result = MlyValue.AtDec (fn _ => let val  (ID
 as ID1) = ID1 ()
 val  (Bigraph as Bigraph1) = Bigraph1 ()
 in ( T.Val(ID, Bigraph) )
end)
 in ( LrTable.NT 2, ( result, VAL1left, Bigraph1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.OpBigraph OpBigraph1, OpBigraph1left, 
OpBigraph1right)) :: rest671)) => let val  result = MlyValue.Bigraph
 (fn _ => let val  (OpBigraph as OpBigraph1) = OpBigraph1 ()
 in ( OpBigraph )
end)
 in ( LrTable.NT 9, ( result, OpBigraph1left, OpBigraph1right), 
rest671)
end
|  ( 17, ( ( _, ( MlyValue.Bigraph Bigraph1, _, Bigraph1right)) :: _
 :: ( _, ( MlyValue.EdgeList EdgeList1, _, _)) :: ( _, ( _, SLASH1left
, _)) :: rest671)) => let val  result = MlyValue.Bigraph (fn _ => let
 val  (EdgeList as EdgeList1) = EdgeList1 ()
 val  (Bigraph as Bigraph1) = Bigraph1 ()
 in ( T.Clo(EdgeList, Bigraph) )
end)
 in ( LrTable.NT 9, ( result, SLASH1left, Bigraph1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.Bigraph Bigraph1, _, Bigraph1right)) :: _
 :: ( _, ( MlyValue.EdgeList EdgeList1, _, _)) :: ( _, ( _, QUOT1left,
 _)) :: rest671)) => let val  result = MlyValue.Bigraph (fn _ => let
 val  (EdgeList as EdgeList1) = EdgeList1 ()
 val  (Bigraph as Bigraph1) = Bigraph1 ()
 in ( T.Con(EdgeList, Bigraph) )
end)
 in ( LrTable.NT 9, ( result, QUOT1left, Bigraph1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.Bigraph Bigraph1, _, Bigraph1right)) :: _
 :: ( _, ( MlyValue.EdgeList EdgeList1, _, _)) :: ( _, ( _, LANG1left,
 _)) :: rest671)) => let val  result = MlyValue.Bigraph (fn _ => let
 val  (EdgeList as EdgeList1) = EdgeList1 ()
 val  (Bigraph as Bigraph1) = Bigraph1 ()
 in ( T.Abs(EdgeList, Bigraph) )
end)
 in ( LrTable.NT 9, ( result, LANG1left, Bigraph1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.Wiring Wiring1, Wiring1left, Wiring1right))
 :: rest671)) => let val  result = MlyValue.Bigraph (fn _ => let val 
 (Wiring as Wiring1) = Wiring1 ()
 in ( T.Wir(Wiring) )
end)
 in ( LrTable.NT 9, ( result, Wiring1left, Wiring1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.JuxtBigraph JuxtBigraph1, JuxtBigraph1left,
 JuxtBigraph1right)) :: rest671)) => let val  result = 
MlyValue.OpBigraph (fn _ => let val  (JuxtBigraph as JuxtBigraph1) = 
JuxtBigraph1 ()
 in ( JuxtBigraph )
end)
 in ( LrTable.NT 10, ( result, JuxtBigraph1left, JuxtBigraph1right), 
rest671)
end
|  ( 22, ( ( _, ( MlyValue.OpBigraph OpBigraph2, _, OpBigraph2right))
 :: _ :: ( _, ( MlyValue.OpBigraph OpBigraph1, OpBigraph1left, _)) :: 
rest671)) => let val  result = MlyValue.OpBigraph (fn _ => let val  
OpBigraph1 = OpBigraph1 ()
 val  OpBigraph2 = OpBigraph2 ()
 in ( T.Par (OpBigraph1, OpBigraph2) )
end)
 in ( LrTable.NT 10, ( result, OpBigraph1left, OpBigraph2right), 
rest671)
end
|  ( 23, ( ( _, ( MlyValue.OpBigraph OpBigraph2, _, OpBigraph2right))
 :: _ :: ( _, ( MlyValue.OpBigraph OpBigraph1, OpBigraph1left, _)) :: 
rest671)) => let val  result = MlyValue.OpBigraph (fn _ => let val  
OpBigraph1 = OpBigraph1 ()
 val  OpBigraph2 = OpBigraph2 ()
 in ( T.Ten (OpBigraph1, OpBigraph2) )
end)
 in ( LrTable.NT 10, ( result, OpBigraph1left, OpBigraph2right), 
rest671)
end
|  ( 24, ( ( _, ( MlyValue.OpBigraph OpBigraph2, _, OpBigraph2right))
 :: _ :: ( _, ( MlyValue.OpBigraph OpBigraph1, OpBigraph1left, _)) :: 
rest671)) => let val  result = MlyValue.OpBigraph (fn _ => let val  
OpBigraph1 = OpBigraph1 ()
 val  OpBigraph2 = OpBigraph2 ()
 in ( T.Pri (OpBigraph1, OpBigraph2) )
end)
 in ( LrTable.NT 10, ( result, OpBigraph1left, OpBigraph2right), 
rest671)
end
|  ( 25, ( ( _, ( MlyValue.OpBigraph OpBigraph2, _, OpBigraph2right))
 :: _ :: ( _, ( MlyValue.OpBigraph OpBigraph1, OpBigraph1left, _)) :: 
rest671)) => let val  result = MlyValue.OpBigraph (fn _ => let val  
OpBigraph1 = OpBigraph1 ()
 val  OpBigraph2 = OpBigraph2 ()
 in ( T.Com (OpBigraph1, OpBigraph2) )
end)
 in ( LrTable.NT 10, ( result, OpBigraph1left, OpBigraph2right), 
rest671)
end
|  ( 26, ( ( _, ( MlyValue.OpBigraph OpBigraph2, _, OpBigraph2right))
 :: _ :: ( _, ( MlyValue.OpBigraph OpBigraph1, OpBigraph1left, _)) :: 
rest671)) => let val  result = MlyValue.OpBigraph (fn _ => let val  
OpBigraph1 = OpBigraph1 ()
 val  OpBigraph2 = OpBigraph2 ()
 in ( T.Com (OpBigraph1, OpBigraph2) )
end)
 in ( LrTable.NT 10, ( result, OpBigraph1left, OpBigraph2right), 
rest671)
end
|  ( 27, ( ( _, ( MlyValue.AtBigraph AtBigraph1, AtBigraph1left, 
AtBigraph1right)) :: rest671)) => let val  result = 
MlyValue.JuxtBigraph (fn _ => let val  (AtBigraph as AtBigraph1) = 
AtBigraph1 ()
 in ( AtBigraph )
end)
 in ( LrTable.NT 11, ( result, AtBigraph1left, AtBigraph1right), 
rest671)
end
|  ( 28, ( ( _, ( MlyValue.AtBigraph AtBigraph1, _, AtBigraph1right))
 :: ( _, ( MlyValue.JuxtBigraph JuxtBigraph1, JuxtBigraph1left, _)) ::
 rest671)) => let val  result = MlyValue.JuxtBigraph (fn _ => let val 
 (JuxtBigraph as JuxtBigraph1) = JuxtBigraph1 ()
 val  (AtBigraph as AtBigraph1) = AtBigraph1 ()
 in ( T.Com (JuxtBigraph, AtBigraph) )
end)
 in ( LrTable.NT 11, ( result, JuxtBigraph1left, AtBigraph1right), 
rest671)
end
|  ( 29, ( ( _, ( MlyValue.NAME NAME1, NAME1left, NAME1right)) :: 
rest671)) => let val  result = MlyValue.AtBigraph (fn _ => let val  (
NAME as NAME1) = NAME1 ()
 in ( T.Ion(NAME,[],[]) )
end)
 in ( LrTable.NT 12, ( result, NAME1left, NAME1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.Edges Edges1, _, Edges1right)) :: _ :: ( _,
 ( MlyValue.NAME NAME1, NAME1left, _)) :: rest671)) => let val  result
 = MlyValue.AtBigraph (fn _ => let val  (NAME as NAME1) = NAME1 ()
 val  (Edges as Edges1) = Edges1 ()
 in ( T.Ion(NAME,Edges,[]) )
end)
 in ( LrTable.NT 12, ( result, NAME1left, Edges1right), rest671)
end
|  ( 31, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Edges Edges1,
 _, _)) :: _ :: _ :: ( _, ( MlyValue.NAME NAME1, NAME1left, _)) :: 
rest671)) => let val  result = MlyValue.AtBigraph (fn _ => let val  (
NAME as NAME1) = NAME1 ()
 val  (Edges as Edges1) = Edges1 ()
 in ( T.Ion(NAME,[],Edges) )
end)
 in ( LrTable.NT 12, ( result, NAME1left, RPAR1right), rest671)
end
|  ( 32, ( ( _, ( _, _, RANG1right)) :: _ :: ( _, ( MlyValue.EdgeList 
EdgeList2, _, _)) :: _ :: ( _, ( MlyValue.EdgeList EdgeList1, _, _))
 :: _ :: ( _, ( MlyValue.NAME NAME1, NAME1left, _)) :: rest671)) =>
 let val  result = MlyValue.AtBigraph (fn _ => let val  (NAME as NAME1
) = NAME1 ()
 val  EdgeList1 = EdgeList1 ()
 val  EdgeList2 = EdgeList2 ()
 in ( T.Ion(NAME,EdgeList1,EdgeList2) )
end)
 in ( LrTable.NT 12, ( result, NAME1left, RANG1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.PortsOpt PortsOpt1, _, PortsOpt1right)) ::
 _ :: ( _, ( MlyValue.INT INT1, _, _)) :: ( _, ( _, LBRK1left, _)) :: 
rest671)) => let val  result = MlyValue.AtBigraph (fn _ => let val  (
INT as INT1) = INT1 ()
 val  (PortsOpt as PortsOpt1) = PortsOpt1 ()
 in ( T.Sit(T.SiteNum INT, PortsOpt) )
end)
 in ( LrTable.NT 12, ( result, LBRK1left, PortsOpt1right), rest671)

end
|  ( 34, ( ( _, ( MlyValue.PortsOpt PortsOpt1, _, PortsOpt1right)) ::
 _ :: ( _, ( MlyValue.NAMEID NAMEID1, _, _)) :: ( _, ( _, LBRK1left, _
)) :: rest671)) => let val  result = MlyValue.AtBigraph (fn _ => let
 val  (NAMEID as NAMEID1) = NAMEID1 ()
 val  (PortsOpt as PortsOpt1) = PortsOpt1 ()
 in ( T.Sit(T.SiteName NAMEID, PortsOpt) )
end)
 in ( LrTable.NT 12, ( result, LBRK1left, PortsOpt1right), rest671)

end
|  ( 35, ( ( _, ( _, BARREN1left, BARREN1right)) :: rest671)) => let
 val  result = MlyValue.AtBigraph (fn _ => ( T.Bar ))
 in ( LrTable.NT 12, ( result, BARREN1left, BARREN1right), rest671)

end
|  ( 36, ( ( _, ( _, _, RPAR1right)) :: ( _, ( MlyValue.Bigraph 
Bigraph1, _, _)) :: ( _, ( _, LPAR1left, _)) :: rest671)) => let val  
result = MlyValue.AtBigraph (fn _ => let val  (Bigraph as Bigraph1) = 
Bigraph1 ()
 in ( Bigraph )
end)
 in ( LrTable.NT 12, ( result, LPAR1left, RPAR1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.AtBigraph (fn _ => let val  (INT as 
INT1) = INT1 ()
 in (
 if INT = 1 then T.Bar
                                         else raise Fail("bpl.grm: AtBigraph of INT encountered, but not 1 (barren root)") 
)
end)
 in ( LrTable.NT 12, ( result, INT1left, INT1right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.AtBigraph (fn _ => let val  (ID as ID1) = 
ID1 ()
 in ( T.Ref (ID) )
end)
 in ( LrTable.NT 12, ( result, ID1left, ID1right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.Ports Ports1, Ports1left, Ports1right)) :: 
rest671)) => let val  result = MlyValue.PortsOpt (fn _ => let val  (
Ports as Ports1) = Ports1 ()
 in ( Ports )
end)
 in ( LrTable.NT 13, ( result, Ports1left, Ports1right), rest671)
end
|  ( 40, ( rest671)) => let val  result = MlyValue.PortsOpt (fn _ => (
 [] ))
 in ( LrTable.NT 13, ( result, defaultPos, defaultPos), rest671)
end
|  ( 41, ( ( _, ( _, _, RANG1right)) :: ( _, ( MlyValue.EdgeList 
EdgeList1, _, _)) :: ( _, ( _, LANG1left, _)) :: rest671)) => let val 
 result = MlyValue.Ports (fn _ => let val  (EdgeList as EdgeList1) = 
EdgeList1 ()
 in ( EdgeList )
end)
 in ( LrTable.NT 14, ( result, LANG1left, RANG1right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.Edges Edges1, Edges1left, Edges1right)) :: 
rest671)) => let val  result = MlyValue.EdgeList (fn _ => let val  (
Edges as Edges1) = Edges1 ()
 in ( Edges )
end)
 in ( LrTable.NT 15, ( result, Edges1left, Edges1right), rest671)
end
|  ( 43, ( rest671)) => let val  result = MlyValue.EdgeList (fn _ => (
 [] ))
 in ( LrTable.NT 15, ( result, defaultPos, defaultPos), rest671)
end
|  ( 44, ( ( _, ( MlyValue.Edges Edges1, _, Edges1right)) :: _ :: ( _,
 ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.Edges (fn _ => let val  (ID as ID1) = ID1 ()
 val  (Edges as Edges1) = Edges1 ()
 in ( ID :: Edges )
end)
 in ( LrTable.NT 16, ( result, ID1left, Edges1right), rest671)
end
|  ( 45, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.Edges (fn _ => let val  (ID as ID1) = ID1
 ()
 in ( [ID] )
end)
 in ( LrTable.NT 16, ( result, ID1left, ID1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.Wiring Wiring1, _, Wiring1right)) :: _ :: (
 _, ( MlyValue.Wire Wire1, Wire1left, _)) :: rest671)) => let val  
result = MlyValue.Wiring (fn _ => let val  (Wire as Wire1) = Wire1 ()
 val  (Wiring as Wiring1) = Wiring1 ()
 in ( Wire :: Wiring )
end)
 in ( LrTable.NT 17, ( result, Wire1left, Wiring1right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.Wire Wire1, Wire1left, Wire1right)) :: 
rest671)) => let val  result = MlyValue.Wiring (fn _ => let val  (Wire
 as Wire1) = Wire1 ()
 in ( [Wire] )
end)
 in ( LrTable.NT 17, ( result, Wire1left, Wire1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: ( _, ( 
MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = 
MlyValue.Wire (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in ( T.GRen(ID1,ID2) )
end)
 in ( LrTable.NT 18, ( result, ID1left, ID2right), rest671)
end
|  ( 49, ( ( _, ( _, _, SLASH1right)) :: ( _, ( MlyValue.ID ID1, 
ID1left, _)) :: rest671)) => let val  result = MlyValue.Wire (fn _ =>
 let val  (ID as ID1) = ID1 ()
 in ( T.GInt(ID) )
end)
 in ( LrTable.NT 18, ( result, ID1left, SLASH1right), rest671)
end
|  ( 50, ( ( _, ( _, _, RBRC2right)) :: ( _, ( MlyValue.ID ID2, _, _))
 :: _ :: _ :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, 
LBRC1left, _)) :: rest671)) => let val  result = MlyValue.Wire (fn _
 => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in ( T.LRen(ID1,ID2) )
end)
 in ( LrTable.NT 18, ( result, LBRC1left, RBRC2right), rest671)
end
|  ( 51, ( ( _, ( _, _, SLASH1right)) :: _ :: ( _, ( MlyValue.ID ID1,
 _, _)) :: ( _, ( _, LBRC1left, _)) :: rest671)) => let val  result = 
MlyValue.Wire (fn _ => let val  (ID as ID1) = ID1 ()
 in ( T.LInt(ID) )
end)
 in ( LrTable.NT 18, ( result, LBRC1left, SLASH1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.Bigraph Bigraph2, _, Bigraph2right)) :: _
 :: ( _, ( MlyValue.Bigraph Bigraph1, Bigraph1left, _)) :: rest671))
 => let val  result = MlyValue.Rule (fn _ => let val  Bigraph1 = 
Bigraph1 ()
 val  Bigraph2 = Bigraph2 ()
 in ( (Bigraph1,Bigraph2) )
end)
 in ( LrTable.NT 19, ( result, Bigraph1left, Bigraph2right), rest671)

end
|  ( 53, ( ( _, ( MlyValue.NAME NAME1, NAME1left, NAME1right)) :: 
rest671)) => let val  result = MlyValue.NAMEID (fn _ => let val  (NAME
 as NAME1) = NAME1 ()
 in ( NAME )
end)
 in ( LrTable.NT 20, ( result, NAME1left, NAME1right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.NAMEID (fn _ => let val  (ID as ID1) = ID1
 ()
 in ( ID )
end)
 in ( LrTable.NT 20, ( result, ID1left, ID1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.Prog x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Bpl_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun NAME (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.NAME (fn () => i),p1,p2))
fun LPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun LANG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun RANG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun BARREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun SLASH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun STAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun COMP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun QUOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun UNDERSCORE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun DPAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun PAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun ACTIVE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun ATOMIC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun PASSIVE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun RULE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun SIG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun SIGNATURE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun USING (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun VAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun STATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
end
end
