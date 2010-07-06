
functor PepaBgLrVals
		   (structure Info : INFO
		    structure Token : TOKEN
		    structure Name : NAME
		    structure Control : CONTROL
        structure Node : NODE
        structure BgAspects : BGASPECTS
          where type root = word
            and type portindex = word
        val get_ctrl : (string * int * int -> Control.control) ref
        datatype result
          = StateResult of int * BgAspects.value BgAspects.AspectMap.map
          | StateListResult
            of (int * BgAspects.value BgAspects.AspectMap.map) list
        sharing type Name.name =
                     BgAspects.name =
                     BgAspects.edge
        sharing type Control.control =
                     BgAspects.control
        sharing type Node.node =
                     BgAspects.node
		    )
	 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* Simple grammar file for ML-Yacc (SML/NJ) defining the parser 
   for the PEPA representation of concrete bigraphs.
*)

(* Position info stuff - below *)

open BgAspects

fun warning str v = (print ("Parser warning: "^str^"\n"); v)

fun mk_origin p1 p2
  = Origin.mk_file_origin
      (!ErrorMsg.fileName)
      (Origin.POS (ErrorMsg.toCoords p1 p2))

fun mk_info p1 p2 = Info.make (mk_origin p1 p2)

(* FIXME function definitions *)


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\006\000\000\000\
\\001\000\002\000\025\000\000\000\
\\001\000\002\000\046\000\000\000\
\\001\000\002\000\047\000\000\000\
\\001\000\002\000\058\000\000\000\
\\001\000\003\000\026\000\000\000\
\\001\000\003\000\027\000\000\000\
\\001\000\003\000\028\000\000\000\
\\001\000\003\000\051\000\000\000\
\\001\000\003\000\059\000\000\000\
\\001\000\003\000\062\000\000\000\
\\001\000\003\000\063\000\000\000\
\\001\000\004\000\009\000\000\000\
\\001\000\005\000\020\000\000\000\
\\001\000\005\000\021\000\000\000\
\\001\000\005\000\022\000\000\000\
\\001\000\005\000\023\000\000\000\
\\001\000\005\000\029\000\000\000\
\\001\000\005\000\030\000\000\000\
\\001\000\005\000\031\000\000\000\
\\001\000\005\000\032\000\000\000\
\\001\000\005\000\042\000\000\000\
\\001\000\005\000\043\000\000\000\
\\001\000\005\000\044\000\000\000\
\\001\000\005\000\045\000\000\000\
\\001\000\005\000\052\000\000\000\
\\001\000\005\000\053\000\000\000\
\\001\000\005\000\054\000\000\000\
\\001\000\005\000\060\000\000\000\
\\001\000\005\000\061\000\000\000\
\\001\000\006\000\050\000\009\000\049\000\000\000\
\\001\000\007\000\057\000\008\000\056\000\000\000\
\\001\000\010\000\041\000\011\000\040\000\012\000\039\000\013\000\035\000\
\\014\000\034\000\000\000\
\\001\000\013\000\035\000\014\000\034\000\000\000\
\\001\000\015\000\011\000\000\000\
\\001\000\016\000\019\000\000\000\
\\001\000\017\000\004\000\018\000\003\000\000\000\
\\065\000\000\000\
\\066\000\000\000\
\\067\000\000\000\
\\068\000\002\000\006\000\000\000\
\\069\000\000\000\
\\070\000\000\000\
\\071\000\004\000\018\000\000\000\
\\072\000\006\000\017\000\007\000\016\000\008\000\015\000\009\000\014\000\000\000\
\\073\000\000\000\
\\074\000\000\000\
\\075\000\000\000\
\\076\000\000\000\
\\077\000\000\000\
\\078\000\000\000\
\\079\000\000\000\
\\080\000\000\000\
\\081\000\000\000\
\\082\000\000\000\
\\083\000\000\000\
\\084\000\000\000\
\\085\000\000\000\
\"
val actionRowNumbers =
"\037\000\001\000\041\000\039\000\
\\013\000\038\000\041\000\035\000\
\\040\000\045\000\044\000\036\000\
\\014\000\015\000\016\000\017\000\
\\045\000\042\000\002\000\006\000\
\\007\000\008\000\043\000\018\000\
\\019\000\020\000\021\000\034\000\
\\034\000\034\000\033\000\052\000\
\\054\000\022\000\051\000\050\000\
\\049\000\023\000\024\000\025\000\
\\003\000\004\000\031\000\009\000\
\\053\000\026\000\047\000\027\000\
\\028\000\046\000\032\000\005\000\
\\010\000\048\000\029\000\030\000\
\\056\000\055\000\011\000\012\000\
\\058\000\057\000\000\000"
val gotoT =
"\
\\001\000\062\000\000\000\
\\002\000\003\000\000\000\
\\002\000\006\000\003\000\005\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\002\000\006\000\003\000\008\000\000\000\
\\000\000\
\\000\000\
\\004\000\011\000\005\000\010\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\022\000\005\000\010\000\000\000\
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
\\007\000\031\000\000\000\
\\007\000\034\000\000\000\
\\007\000\035\000\000\000\
\\007\000\036\000\000\000\
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
\\008\000\046\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\009\000\053\000\000\000\
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
val numstates = 63
val numrules = 21
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
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | ID of unit ->  (string) | INT of unit ->  (int)
 | link of unit ->  (link) | place of unit ->  (place)
 | presence of unit ->  (value) | entity of unit ->  (entity)
 | aspectvalue of unit ->  (aspect*value)
 | aspectvaluelist of unit ->  (value AspectMap.map)
 | statelist of unit ->  ( ( int * value AspectMap.map )  list)
 | state of unit ->  (int*value AspectMap.map)
 | start of unit ->  (result)
end
type svalue = MlyValue.svalue
type result = result
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "INT"
  | (T 2) => "ID"
  | (T 3) => "COLLABSTAR"
  | (T 4) => "UUSCORE"
  | (T 5) => "NODE"
  | (T 6) => "EDGE"
  | (T 7) => "NAME"
  | (T 8) => "ROOT"
  | (T 9) => "CTRL"
  | (T 10) => "PRNT"
  | (T 11) => "PORT"
  | (T 12) => "PRESENT"
  | (T 13) => "ABSENT"
  | (T 14) => "LBRACE"
  | (T 15) => "RBRACE"
  | (T 16) => "STATELIST"
  | (T 17) => "STATE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11)
 $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ 
(T 3) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.statelist statelist1, _, statelist1right))
 :: ( _, ( _, STATELIST1left, _)) :: rest671)) => let val  result = 
MlyValue.start (fn _ => let val  (statelist as statelist1) = 
statelist1 ()
 in (StateListResult statelist)
end)
 in ( LrTable.NT 0, ( result, STATELIST1left, statelist1right), 
rest671)
end
|  ( 1, ( ( _, ( MlyValue.state state1, _, state1right)) :: ( _, ( _, 
STATE1left, _)) :: rest671)) => let val  result = MlyValue.start (fn _
 => let val  (state as state1) = state1 ()
 in (StateResult state)
end)
 in ( LrTable.NT 0, ( result, STATE1left, state1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.statelist statelist1, _, statelist1right))
 :: ( _, ( MlyValue.state state1, state1left, _)) :: rest671)) => let
 val  result = MlyValue.statelist (fn _ => let val  (state as state1)
 = state1 ()
 val  (statelist as statelist1) = statelist1 ()
 in (state :: statelist)
end)
 in ( LrTable.NT 2, ( result, state1left, statelist1right), rest671)

end
|  ( 3, ( rest671)) => let val  result = MlyValue.statelist (fn _ => (
[]))
 in ( LrTable.NT 2, ( result, defaultPos, defaultPos), rest671)
end
|  ( 4, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( 
MlyValue.aspectvaluelist aspectvaluelist1, _, _)) :: _ :: _ :: ( _, ( 
MlyValue.INT INT1, INT1left, _)) :: rest671)) => let val  result = 
MlyValue.state (fn _ => let val  (INT as INT1) = INT1 ()
 val  (aspectvaluelist as aspectvaluelist1) = aspectvaluelist1 ()
 in ((INT, aspectvaluelist))
end)
 in ( LrTable.NT 1, ( result, INT1left, RBRACE1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.aspectvaluelist aspectvaluelist1, _, 
aspectvaluelist1right)) :: _ :: ( _, ( MlyValue.aspectvalue 
aspectvalue1, aspectvalue1left, _)) :: rest671)) => let val  result = 
MlyValue.aspectvaluelist (fn _ => let val  (aspectvalue as 
aspectvalue1) = aspectvalue1 ()
 val  (aspectvaluelist as aspectvaluelist1) = aspectvaluelist1 ()
 in (
(AspectMap.add'
          (fn _ => false)
          (#1 aspectvalue, #2 aspectvalue, aspectvaluelist))
       handle AspectMap.DATACHANGED =>
              raise Fail "FIXME duplicate aspect"
)
end)
 in ( LrTable.NT 3, ( result, aspectvalue1left, aspectvaluelist1right)
, rest671)
end
|  ( 6, ( ( _, ( MlyValue.aspectvalue aspectvalue1, aspectvalue1left, 
aspectvalue1right)) :: rest671)) => let val  result = 
MlyValue.aspectvaluelist (fn _ => let val  (aspectvalue as 
aspectvalue1) = aspectvalue1 ()
 in (AspectMap.singleton aspectvalue)
end)
 in ( LrTable.NT 3, ( result, aspectvalue1left, aspectvalue1right), 
rest671)
end
|  ( 7, ( rest671)) => let val  result = MlyValue.aspectvaluelist (fn
 _ => (AspectMap.empty))
 in ( LrTable.NT 3, ( result, defaultPos, defaultPos), rest671)
end
|  ( 8, ( ( _, ( MlyValue.ID ID2, _, ID2right)) :: _ :: _ :: _ :: ( _,
 ( MlyValue.ID ID1, IDleft, IDright)) :: _ :: ( _, ( _, NODE1left, _))
 :: rest671)) => let val  result = MlyValue.aspectvalue (fn _ => let
 val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 in (
(NodeControl (Node.make ID1),
        Control (!get_ctrl (ID2, IDleft, IDright)))
)
end)
 in ( LrTable.NT 4, ( result, NODE1left, ID2right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.place place1, _, place1right)) :: _ :: _ ::
 _ :: ( _, ( MlyValue.ID ID1, _, _)) :: _ :: ( _, ( _, NODE1left, _))
 :: rest671)) => let val  result = MlyValue.aspectvalue (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  (place as place1) = place1 ()
 in ((ChildParent (CNode (Node.make ID)), Place place))
end)
 in ( LrTable.NT 4, ( result, NODE1left, place1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.link link1, _, link1right)) :: _ :: ( _, ( 
MlyValue.INT INT1, _, _)) :: _ :: _ :: _ :: ( _, ( MlyValue.ID ID1, _,
 _)) :: _ :: ( _, ( _, NODE1left, _)) :: rest671)) => let val  result
 = MlyValue.aspectvalue (fn _ => let val  (ID as ID1) = ID1 ()
 val  (INT as INT1) = INT1 ()
 val  (link as link1) = link1 ()
 in ((PointLink (PPort (Node.make ID, Word.fromInt INT)), Link link))

end)
 in ( LrTable.NT 4, ( result, NODE1left, link1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.presence presence1, _, presence1right)) ::
 _ :: ( _, ( MlyValue.ID ID1, _, _)) :: _ :: ( _, ( _, NODE1left, _))
 :: rest671)) => let val  result = MlyValue.aspectvalue (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  (presence as presence1) = presence1 ()
 in (Presence (ENode (Node.make ID)), presence)
end)
 in ( LrTable.NT 4, ( result, NODE1left, presence1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.presence presence1, _, presence1right)) ::
 _ :: ( _, ( MlyValue.ID ID1, _, _)) :: _ :: ( _, ( _, EDGE1left, _))
 :: rest671)) => let val  result = MlyValue.aspectvalue (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  (presence as presence1) = presence1 ()
 in (Presence (EEdge (Name.make ID)), presence)
end)
 in ( LrTable.NT 4, ( result, EDGE1left, presence1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.presence presence1, _, presence1right)) ::
 _ :: ( _, ( MlyValue.ID ID1, _, _)) :: _ :: ( _, ( _, NAME1left, _))
 :: rest671)) => let val  result = MlyValue.aspectvalue (fn _ => let
 val  (ID as ID1) = ID1 ()
 val  (presence as presence1) = presence1 ()
 in (Presence (EName (Name.make ID)), presence)
end)
 in ( LrTable.NT 4, ( result, NAME1left, presence1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.presence presence1, _, presence1right)) ::
 _ :: ( _, ( MlyValue.INT INT1, _, _)) :: _ :: ( _, ( _, ROOT1left, _)
) :: rest671)) => let val  result = MlyValue.aspectvalue (fn _ => let
 val  (INT as INT1) = INT1 ()
 val  (presence as presence1) = presence1 ()
 in (Presence (ERoot (Word.fromInt INT)), presence)
end)
 in ( LrTable.NT 4, ( result, ROOT1left, presence1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.INT INT1, _, INT1right)) :: _ :: ( _, ( _, 
PRESENT1left, _)) :: rest671)) => let val  result = MlyValue.presence
 (fn _ => let val  (INT as INT1) = INT1 ()
 in (Present (Word.fromInt INT))
end)
 in ( LrTable.NT 6, ( result, PRESENT1left, INT1right), rest671)
end
|  ( 16, ( ( _, ( _, ABSENT1left, ABSENT1right)) :: rest671)) => let
 val  result = MlyValue.presence (fn _ => (Absent))
 in ( LrTable.NT 6, ( result, ABSENT1left, ABSENT1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, 
NODE1left, _)) :: rest671)) => let val  result = MlyValue.place (fn _
 => let val  (ID as ID1) = ID1 ()
 in (PNode (Node.make ID))
end)
 in ( LrTable.NT 7, ( result, NODE1left, ID1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.INT INT1, _, INT1right)) :: _ :: ( _, ( _, 
ROOT1left, _)) :: rest671)) => let val  result = MlyValue.place (fn _
 => let val  (INT as INT1) = INT1 ()
 in (PRoot (Word.fromInt INT))
end)
 in ( LrTable.NT 7, ( result, ROOT1left, INT1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, 
EDGE1left, _)) :: rest671)) => let val  result = MlyValue.link (fn _
 => let val  (ID as ID1) = ID1 ()
 in (LEdge (Name.make ID))
end)
 in ( LrTable.NT 8, ( result, EDGE1left, ID1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, 
NAME1left, _)) :: rest671)) => let val  result = MlyValue.link (fn _
 => let val  (ID as ID1) = ID1 ()
 in (LName (Name.make ID))
end)
 in ( LrTable.NT 8, ( result, NAME1left, ID1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : PepaBg_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun COLLABSTAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun UUSCORE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun NODE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun EDGE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun NAME (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun ROOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun CTRL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun PRNT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun PORT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun PRESENT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun ABSENT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun STATELIST (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun STATE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
end
end
