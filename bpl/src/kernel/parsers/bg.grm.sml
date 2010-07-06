
functor BgTermLrVals
		   (structure Info : INFO
		    structure Token : TOKEN
		    structure Control : CONTROL
		    structure Name : NAME
		    structure NameSet : MONO_SET
		    structure Link : LINK
		    structure LinkSet : MONO_SET
		    structure Wiring : WIRING
		    structure Ion : ION
		    structure Permutation : PERMUTATION
		    structure BgTerm : BGTERM
		    sharing type Control.control = Ion.control
		    sharing type Name.name = 
				 NameSet.elt = 
				 Link.name =
				 Ion.name
		    sharing type NameSet.Set =
				 Link.nameset =
				 Ion.nameset =
				 Permutation.nameset =
				 BgTerm.nameset
		    sharing type Link.link =
				 LinkSet.elt =
				 Wiring.link
		    sharing type LinkSet.Set = Wiring.linkset
		    sharing type Wiring.wiring = BgTerm.wiring
		    sharing type Ion.ion = BgTerm.ion
		    sharing type Permutation.permutation =
				 BgTerm.permutation
		    sharing type Permutation.Immutable =
		                 BgTerm.Immutable
                    sharing type Info.info =
                                 BgTerm.info
		    )
	 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* Simple grammar file for ML-Yacc (SML/NJ) defining the parser 
   for bg term language (mainly for debug) 

   Functionality - for now - just read a file containing a single dec 
   on the form:

   bg

   Syntax and grammar summary :

   bg := ()
      |  merge
      |  'nset'
      |  name/nset                (and name/name, /name, and /nset)
      |  ctrlid <nlist> <nsetlist>
      |  ctrlid <nlist>
      |  ctrlid
      |  [ permentrylist ]
      |  (nset) bg
      |  bg x bg
      |  bg | bg
      |  bg || bg
      |  bg o bg
  
   nsetlist      = 
                 | nset
	         | nset, nsetlist

   nset          = {nlist}

   nlist         = 
                 | name
	         | name, nlist

   permentrylist = 
                 | permentry 
		 | permentry, permentrylist

   permentry     = int nset
                 | int
   
   name   = [a-n|p-v|yz]
          | [a-z][A-za-z0-9_]+

   (single o and x are keywords (might change to oo and xx))

   ctrlid  = [A-Z][A-za-z0-9_]*

   - and ML-style comments (* ... *) 

-----

  Precedences (with tightest binding at bottom):

  ( )         abstraction
  x, |, ||    tensor product, prime product, parallel product
  o           composition

-----

Notes:
-  For now, I'm just creating a fresh ctrl for each
   ion - hence ctrl-eq must be tested by testing constituent parts.
   Change datatype bg (and this call) to let ions point to a common ctrl-def.
-  Tensor product XX is parsed as if it was a binary operator
   (as are prime product and parallel product)
-  Nice-to-have extended forms : 
     id_iface      =>  id on iface - iface
     id		   =>  (not only grammar issue) 
			- id that can be instatiated to 
     'nlist'       => '{nlist}'
*)

(* Position info stuff - below *)

open BgTerm

type control = Control.control
type name = Name.name
type link = Link.link
type nameset = NameSet.Set
type linkset = LinkSet.Set
type ion = Ion.ion
type wiring = Wiring.wiring
type 'kind permutation = 'kind Permutation.permutation

(** Signal that a set with duplicate names has been encountered. 
 * @params smlfile leftpos rightpos errtxt
 * @param smlfile   SML file with the code that encountered the error.
 * @param leftpos   Position of lefthand side of error term.
 * @param rightpos  Position of righthand side of error term.
 * @param errtxt    Text detailing the error.
*)
exception DuplicateNames of string * (int * int) * string

fun warning str v = (print ("Parser warning: "^str^"\n"); v)

fun mk_origin p1 p2 = Origin.mk_file_origin (!ErrorMsg.fileName) (Origin.POS (ErrorMsg.toCoords p1 p2))

fun mk_info p1 p2 = Info.make (mk_origin p1 p2)

fun nset2idlinkset nset =
    NameSet.fold 
      (fn name => 
	  fn links =>
	     LinkSet.insert
	       (Link.make {outer = SOME name, 
			   inner = NameSet.singleton name}) 
	       links)
      LinkSet.empty
      nset


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\001\000\110\000\007\000\110\000\009\000\110\000\014\000\131\000\
\\015\000\110\000\016\000\110\000\017\000\110\000\018\000\110\000\
\\021\000\110\000\000\000\
\\001\000\001\000\112\000\007\000\112\000\009\000\112\000\014\000\134\000\
\\015\000\112\000\016\000\112\000\017\000\112\000\018\000\112\000\
\\021\000\112\000\000\000\
\\001\000\001\000\117\000\007\000\117\000\009\000\117\000\014\000\133\000\
\\015\000\117\000\016\000\117\000\017\000\117\000\018\000\117\000\
\\021\000\117\000\000\000\
\\001\000\001\000\122\000\007\000\122\000\009\000\122\000\014\000\135\000\
\\015\000\122\000\016\000\122\000\017\000\122\000\018\000\122\000\
\\021\000\122\000\000\000\
\\001\000\002\000\025\000\004\000\024\000\005\000\023\000\006\000\022\000\
\\007\000\052\000\008\000\021\000\010\000\034\000\014\000\020\000\
\\019\000\019\000\020\000\018\000\022\000\017\000\023\000\016\000\
\\024\000\015\000\025\000\014\000\026\000\013\000\027\000\012\000\000\000\
\\001\000\002\000\025\000\004\000\024\000\005\000\023\000\006\000\022\000\
\\008\000\021\000\014\000\020\000\019\000\019\000\020\000\018\000\
\\022\000\017\000\023\000\016\000\024\000\015\000\025\000\014\000\
\\026\000\013\000\027\000\012\000\000\000\
\\001\000\002\000\025\000\005\000\044\000\010\000\034\000\014\000\043\000\
\\022\000\042\000\025\000\041\000\026\000\040\000\027\000\039\000\000\000\
\\001\000\002\000\025\000\005\000\044\000\010\000\034\000\022\000\042\000\
\\025\000\041\000\026\000\040\000\027\000\039\000\000\000\
\\001\000\007\000\070\000\000\000\
\\001\000\007\000\071\000\015\000\032\000\016\000\031\000\017\000\030\000\
\\018\000\029\000\000\000\
\\001\000\008\000\077\000\000\000\
\\001\000\008\000\078\000\000\000\
\\001\000\009\000\069\000\000\000\
\\001\000\009\000\090\000\000\000\
\\001\000\009\000\092\000\000\000\
\\001\000\010\000\034\000\000\000\
\\001\000\011\000\076\000\000\000\
\\001\000\012\000\075\000\000\000\
\\001\000\012\000\082\000\000\000\
\\001\000\014\000\026\000\000\000\
\\001\000\014\000\066\000\000\000\
\\001\000\016\000\035\000\000\000\
\\001\000\016\000\064\000\000\000\
\\001\000\019\000\065\000\000\000\
\\097\000\000\000\
\\098\000\015\000\032\000\016\000\031\000\017\000\030\000\018\000\029\000\000\000\
\\099\000\000\000\
\\100\000\015\000\032\000\000\000\
\\101\000\000\000\
\\102\000\015\000\032\000\000\000\
\\103\000\015\000\032\000\000\000\
\\104\000\000\000\
\\105\000\000\000\
\\106\000\000\000\
\\107\000\015\000\032\000\016\000\031\000\017\000\030\000\018\000\029\000\
\\021\000\091\000\000\000\
\\108\000\002\000\025\000\004\000\024\000\005\000\023\000\006\000\022\000\
\\008\000\021\000\014\000\020\000\019\000\019\000\020\000\018\000\
\\022\000\017\000\023\000\016\000\024\000\015\000\025\000\014\000\
\\026\000\013\000\027\000\012\000\000\000\
\\109\000\000\000\
\\111\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\118\000\000\000\
\\119\000\013\000\056\000\000\000\
\\120\000\013\000\028\000\000\000\
\\121\000\000\000\
\\123\000\015\000\032\000\016\000\031\000\017\000\030\000\018\000\029\000\000\000\
\\124\000\000\000\
\\125\000\000\000\
\\126\000\000\000\
\\127\000\000\000\
\\128\000\021\000\074\000\000\000\
\\129\000\002\000\025\000\005\000\044\000\022\000\042\000\025\000\041\000\
\\026\000\040\000\027\000\039\000\000\000\
\\130\000\000\000\
\\131\000\000\000\
\\132\000\000\000\
\\132\000\010\000\034\000\000\000\
\\133\000\000\000\
\\134\000\000\000\
\\135\000\000\000\
\\136\000\000\000\
\\137\000\000\000\
\\138\000\021\000\081\000\000\000\
\\139\000\010\000\034\000\000\000\
\\140\000\000\000\
\\141\000\000\000\
\\142\000\000\000\
\\143\000\000\000\
\\144\000\000\000\
\\145\000\000\000\
\\146\000\000\000\
\\147\000\000\000\
\\148\000\021\000\068\000\000\000\
\\149\000\003\000\049\000\024\000\048\000\000\000\
\\150\000\000\000\
\\151\000\010\000\034\000\000\000\
\\152\000\000\000\
\\153\000\000\000\
\\154\000\021\000\093\000\000\000\
\\155\000\002\000\025\000\005\000\044\000\014\000\088\000\022\000\042\000\
\\025\000\041\000\026\000\040\000\027\000\039\000\000\000\
\\156\000\000\000\
\\157\000\000\000\
\"
val actionRowNumbers =
"\006\000\020\000\049\000\046\000\
\\041\000\045\000\029\000\027\000\
\\026\000\025\000\004\000\003\000\
\\057\000\039\000\038\000\001\000\
\\022\000\016\000\007\000\074\000\
\\005\000\002\000\066\000\054\000\
\\008\000\044\000\053\000\006\000\
\\006\000\006\000\006\000\042\000\
\\053\000\023\000\024\000\069\000\
\\067\000\060\000\058\000\056\000\
\\055\000\021\000\059\000\076\000\
\\073\000\013\000\082\000\081\000\
\\009\000\010\000\037\000\068\000\
\\070\000\043\000\064\000\052\000\
\\018\000\031\000\030\000\028\000\
\\032\000\017\000\011\000\040\000\
\\012\000\075\000\074\000\071\000\
\\006\000\048\000\063\000\019\000\
\\053\000\050\000\065\000\036\000\
\\080\000\072\000\047\000\064\000\
\\061\000\051\000\014\000\035\000\
\\015\000\079\000\008\000\062\000\
\\033\000\036\000\077\000\080\000\
\\034\000\078\000\000\000"
val gotoT =
"\
\\001\000\094\000\002\000\009\000\003\000\008\000\004\000\007\000\
\\005\000\006\000\012\000\005\000\013\000\004\000\014\000\003\000\
\\017\000\002\000\019\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\007\000\025\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\031\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\034\000\000\000\
\\011\000\036\000\019\000\035\000\000\000\
\\015\000\045\000\016\000\044\000\020\000\043\000\000\000\
\\003\000\049\000\004\000\007\000\005\000\006\000\011\000\048\000\
\\012\000\005\000\013\000\004\000\014\000\003\000\017\000\002\000\
\\019\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\052\000\019\000\051\000\000\000\
\\009\000\053\000\000\000\
\\008\000\056\000\019\000\055\000\000\000\
\\003\000\057\000\004\000\007\000\005\000\006\000\012\000\005\000\
\\013\000\004\000\014\000\003\000\017\000\002\000\019\000\001\000\000\000\
\\003\000\058\000\004\000\007\000\005\000\006\000\012\000\005\000\
\\013\000\004\000\014\000\003\000\017\000\002\000\019\000\001\000\000\000\
\\003\000\059\000\004\000\007\000\005\000\006\000\012\000\005\000\
\\013\000\004\000\014\000\003\000\017\000\002\000\019\000\001\000\000\000\
\\003\000\060\000\004\000\007\000\005\000\006\000\012\000\005\000\
\\013\000\004\000\014\000\003\000\017\000\002\000\019\000\001\000\000\000\
\\000\000\
\\008\000\061\000\019\000\055\000\000\000\
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
\\011\000\065\000\000\000\
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
\\010\000\071\000\011\000\070\000\000\000\
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
\\015\000\077\000\016\000\044\000\020\000\043\000\000\000\
\\000\000\
\\003\000\078\000\004\000\007\000\005\000\006\000\012\000\005\000\
\\013\000\004\000\014\000\003\000\017\000\002\000\019\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\081\000\019\000\055\000\000\000\
\\000\000\
\\000\000\
\\003\000\083\000\004\000\007\000\005\000\006\000\006\000\082\000\
\\012\000\005\000\013\000\004\000\014\000\003\000\017\000\002\000\
\\019\000\001\000\000\000\
\\013\000\085\000\018\000\084\000\019\000\001\000\000\000\
\\000\000\
\\000\000\
\\010\000\087\000\011\000\070\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\011\000\036\000\019\000\035\000\000\000\
\\000\000\
\\000\000\
\\003\000\083\000\004\000\007\000\005\000\006\000\006\000\092\000\
\\012\000\005\000\013\000\004\000\014\000\003\000\017\000\002\000\
\\019\000\001\000\000\000\
\\000\000\
\\013\000\085\000\018\000\093\000\019\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 95
val numrules = 61
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
 | IDPn of unit ->  (int) | MERGEn of unit ->  (int)
 | CTRLID of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string) | int of unit ->  (int)
 | id of unit ->  (string) | wiringentrylist of unit ->  (link list)
 | wiring of unit ->  (wiring)
 | permentry of unit ->  ( ( int * nameset ) )
 | permentrylist of unit ->  ( ( int * nameset )  list)
 | perm of unit ->  (Immutable permutation) | link of unit ->  (link)
 | ctrlid of unit ->  (control) | nset of unit ->  (nameset)
 | nsetlist of unit ->  (nameset list)
 | bports of unit ->  (nameset list) | nlist of unit ->  (name list)
 | fports of unit ->  (name list) | bglist of unit ->  (bgterm list)
 | tenlist of unit ->  (bgterm list) | altbg of unit ->  (bgterm)
 | bg of unit ->  (bgterm) | dec of unit ->  (bgterm)
 | prog of unit ->  (bgterm)
end
type svalue = MlyValue.svalue
type result = bgterm
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
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "CTRLID"
  | (T 4) => "MERGEn"
  | (T 5) => "LPAREN"
  | (T 6) => "RPAREN"
  | (T 7) => "LBRACK"
  | (T 8) => "RBRACK"
  | (T 9) => "LBRACE"
  | (T 10) => "RBRACE"
  | (T 11) => "GT"
  | (T 12) => "LT"
  | (T 13) => "SLASH"
  | (T 14) => "OO"
  | (T 15) => "XX"
  | (T 16) => "PRI"
  | (T 17) => "PAR"
  | (T 18) => "QUOTE"
  | (T 19) => "BQUOTE"
  | (T 20) => "COMMA"
  | (T 21) => "IDX0"
  | (T 22) => "IDBB0"
  | (T 23) => "ONE"
  | (T 24) => "IDW"
  | (T 25) => "IDW0"
  | (T 26) => "IDPn"
  | (T 27) => "AT"
  | (T 28) => "ATAT"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 28) $$ (T 27) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21)
 $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14)
 $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7)
 $$ (T 6) $$ (T 5) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.dec dec1, dec1left, dec1right)) :: rest671)
) => let val  result = MlyValue.prog (fn _ => let val  (dec as dec1) =
 dec1 ()
 in (dec)
end)
 in ( LrTable.NT 0, ( result, dec1left, dec1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.bg bg1, bg1left, bg1right)) :: rest671)) =>
 let val  result = MlyValue.dec (fn _ => let val  (bg as bg1) = bg1 ()
 in (bg)
end)
 in ( LrTable.NT 1, ( result, bg1left, bg1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.altbg altbg1, altbg1left, altbg1right)) :: 
rest671)) => let val  result = MlyValue.bg (fn _ => let val  (altbg
 as altbg1) = altbg1 ()
 in (altbg)
end)
 in ( LrTable.NT 2, ( result, altbg1left, altbg1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.bg bg2, _, bg2right)) :: _ :: ( _, ( 
MlyValue.bg bg1, bg1left, _)) :: rest671)) => let val  result = 
MlyValue.bg (fn _ => let val  bg1 = bg1 ()
 val  bg2 = bg2 ()
 in (Ten ([bg1, bg2], mk_info bg1left bg2right))
end)
 in ( LrTable.NT 2, ( result, bg1left, bg2right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.tenlist tenlist1, (tenlistleft as 
tenlist1left), (tenlistright as tenlist1right))) :: rest671)) => let
 val  result = MlyValue.bg (fn _ => let val  (tenlist as tenlist1) = 
tenlist1 ()
 in (Ten (tenlist, mk_info tenlistleft tenlistright))
end)
 in ( LrTable.NT 2, ( result, tenlist1left, tenlist1right), rest671)

end
|  ( 5, ( ( _, ( MlyValue.bg bg2, _, bg2right)) :: _ :: ( _, ( 
MlyValue.bg bg1, bg1left, _)) :: rest671)) => let val  result = 
MlyValue.bg (fn _ => let val  bg1 = bg1 ()
 val  bg2 = bg2 ()
 in (Pri ([bg1, bg2], mk_info bg1left bg2right))
end)
 in ( LrTable.NT 2, ( result, bg1left, bg2right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.bg bg2, _, bg2right)) :: _ :: ( _, ( 
MlyValue.bg bg1, bg1left, _)) :: rest671)) => let val  result = 
MlyValue.bg (fn _ => let val  bg1 = bg1 ()
 val  bg2 = bg2 ()
 in (Par ([bg1, bg2], mk_info bg1left bg2right))
end)
 in ( LrTable.NT 2, ( result, bg1left, bg2right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.bg bg2, _, bg2right)) :: _ :: ( _, ( 
MlyValue.bg bg1, bg1left, _)) :: rest671)) => let val  result = 
MlyValue.bg (fn _ => let val  bg1 = bg1 ()
 val  bg2 = bg2 ()
 in (Com (bg1, bg2, mk_info bg1left bg2right))
end)
 in ( LrTable.NT 2, ( result, bg1left, bg2right), rest671)
end
|  ( 8, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.bglist 
bglist1, _, _)) :: _ :: _ :: _ :: ( _, ( _, BQUOTE1left, _)) :: 
rest671)) => let val  result = MlyValue.tenlist (fn _ => let val  (
bglist as bglist1) = bglist1 ()
 in (bglist)
end)
 in ( LrTable.NT 4, ( result, BQUOTE1left, RBRACK1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.bglist bglist1, _, bglist1right)) :: _ :: (
 _, ( MlyValue.bg bg1, bg1left, _)) :: rest671)) => let val  result = 
MlyValue.bglist (fn _ => let val  (bg as bg1) = bg1 ()
 val  (bglist as bglist1) = bglist1 ()
 in (bg :: bglist)
end)
 in ( LrTable.NT 5, ( result, bg1left, bglist1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.bg bg1, bg1left, bg1right)) :: rest671)) =>
 let val  result = MlyValue.bglist (fn _ => let val  (bg as bg1) = bg1
 ()
 in ([bg])
end)
 in ( LrTable.NT 5, ( result, bg1left, bg1right), rest671)
end
|  ( 11, ( rest671)) => let val  result = MlyValue.bglist (fn _ => ([]
))
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 12, ( ( _, ( _, _, (RPARENright as RPAREN1right))) :: ( _, ( _, (
LPARENleft as LPAREN1left), _)) :: rest671)) => let val  result = 
MlyValue.altbg (fn _ => (Mer (0, mk_info LPARENleft RPARENright)))
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 13, ( ( _, ( _, (IDX0left as IDX01left), (IDX0right as IDX01right
))) :: rest671)) => let val  result = MlyValue.altbg (fn _ => (
Ten ([], mk_info IDX0left IDX0right)))
 in ( LrTable.NT 3, ( result, IDX01left, IDX01right), rest671)
end
|  ( 14, ( ( _, ( _, (IDBB0left as IDBB01left), (IDBB0right as 
IDBB01right))) :: rest671)) => let val  result = MlyValue.altbg (fn _
 => (Par ([], mk_info IDBB0left IDBB0right)))
 in ( LrTable.NT 3, ( result, IDBB01left, IDBB01right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.MERGEn MERGEn1, (MERGEnleft as MERGEn1left)
, (MERGEnright as MERGEn1right))) :: rest671)) => let val  result = 
MlyValue.altbg (fn _ => let val  (MERGEn as MERGEn1) = MERGEn1 ()
 in (Mer (MERGEn, mk_info MERGEnleft MERGEnright))
end)
 in ( LrTable.NT 3, ( result, MERGEn1left, MERGEn1right), rest671)
end
|  ( 16, ( ( _, ( _, (ONEleft as ONE1left), (ONEright as ONE1right)))
 :: rest671)) => let val  result = MlyValue.altbg (fn _ => (
Mer (0, mk_info ONEright ONEleft)))
 in ( LrTable.NT 3, ( result, ONE1left, ONE1right), rest671)
end
|  ( 17, ( ( _, ( _, _, QUOTE2right)) :: ( _, ( MlyValue.nset nset1, _
, _)) :: ( _, ( _, QUOTE1left, _)) :: rest671)) => let val  result = 
MlyValue.altbg (fn _ => let val  (nset as nset1) = nset1 ()
 in (Con (nset, mk_info QUOTE1left QUOTE2right))
end)
 in ( LrTable.NT 3, ( result, QUOTE1left, QUOTE2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.link link1, (linkleft as link1left), (
linkright as link1right))) :: rest671)) => let val  result = 
MlyValue.altbg (fn _ => let val  (link as link1) = link1 ()
 in (
Wir (Wiring.make 
				      (LinkSet.singleton link), 
				      mk_info linkleft linkright)
)
end)
 in ( LrTable.NT 3, ( result, link1left, link1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.nset nset1, _, nset1right)) :: ( _, ( _, (
IDWleft as IDW1left), IDWright)) :: rest671)) => let val  result = 
MlyValue.altbg (fn _ => let val  (nset as nset1) = nset1 ()
 in (
Wir (Wiring.make (nset2idlinkset nset),
				    mk_info IDWleft IDWright)
)
end)
 in ( LrTable.NT 3, ( result, IDW1left, nset1right), rest671)
end
|  ( 20, ( ( _, ( _, (IDW0left as IDW01left), (IDW0right as IDW01right
))) :: rest671)) => let val  result = MlyValue.altbg (fn _ => (
Wir (Wiring.id_0, mk_info IDW0left IDW0right)))
 in ( LrTable.NT 3, ( result, IDW01left, IDW01right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.bports bports1, _, (bportsright as 
bports1right))) :: ( _, ( MlyValue.fports fports1, _, _)) :: ( _, ( 
MlyValue.ctrlid ctrlid1, (ctrlidleft as ctrlid1left), _)) :: rest671))
 => let val  result = MlyValue.altbg (fn _ => let val  (ctrlid as 
ctrlid1) = ctrlid1 ()
 val  (fports as fports1) = fports1 ()
 val  (bports as bports1) = bports1 ()
 in (
let 
				 val fp = fports
				 val bp = bports
			       in
				 Ion (Ion.make 
					({ctrl = ctrlid, 
					  free = fports,
					  bound = bports}),
					mk_info ctrlidleft bportsright)
			       end
)
end)
 in ( LrTable.NT 3, ( result, ctrlid1left, bports1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.fports fports1, _, (fportsright as 
fports1right))) :: ( _, ( MlyValue.ctrlid ctrlid1, (ctrlidleft as 
ctrlid1left), _)) :: rest671)) => let val  result = MlyValue.altbg (fn
 _ => let val  (ctrlid as ctrlid1) = ctrlid1 ()
 val  (fports as fports1) = fports1 ()
 in (
let 
				 val fp = fports
			       in
				 Ion (Ion.make 
					({ctrl = ctrlid, 
					  free = fports,
					  bound = []}),
					mk_info ctrlidleft fportsright)
			       end
)
end)
 in ( LrTable.NT 3, ( result, ctrlid1left, fports1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.ctrlid ctrlid1, (ctrlidleft as ctrlid1left)
, (ctrlidright as ctrlid1right))) :: rest671)) => let val  result = 
MlyValue.altbg (fn _ => let val  (ctrlid as ctrlid1) = ctrlid1 ()
 in (
Ion (Ion.make 
					({ctrl = ctrlid, 
					  free = [],
					  bound = []}),
					mk_info ctrlidleft ctrlidright)
)
end)
 in ( LrTable.NT 3, ( result, ctrlid1left, ctrlid1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.perm perm1, (permleft as perm1left), (
permright as perm1right))) :: rest671)) => let val  result = 
MlyValue.altbg (fn _ => let val  (perm as perm1) = perm1 ()
 in (Per (perm, mk_info permleft permright))
end)
 in ( LrTable.NT 3, ( result, perm1left, perm1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.IDPn IDPn1, (IDPnleft as IDPn1left), (
IDPnright as IDPn1right))) :: rest671)) => let val  result = 
MlyValue.altbg (fn _ => let val  (IDPn as IDPn1) = IDPn1 ()
 in (Per (Permutation.id_n IDPn, mk_info IDPnleft IDPnright))
end)
 in ( LrTable.NT 3, ( result, IDPn1left, IDPn1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.bg bg1, _, (bgright as bg1right))) :: _ :: 
( _, ( MlyValue.nset nset1, _, _)) :: ( _, ( _, (LPARENleft as 
LPAREN1left), _)) :: rest671)) => let val  result = MlyValue.altbg (fn
 _ => let val  (nset as nset1) = nset1 ()
 val  (bg as bg1) = bg1 ()
 in (Abs (nset, bg, mk_info LPARENleft bgright))
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, bg1right), rest671)
end
|  ( 27, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.bg bg1, _, _
)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.altbg (fn _ => let val  (bg as bg1) = bg1 ()
 in (bg)
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.wiring wiring1, (wiringleft as wiring1left)
, (wiringright as wiring1right))) :: rest671)) => let val  result = 
MlyValue.altbg (fn _ => let val  (wiring as wiring1) = wiring1 ()
 in (Wir (wiring, mk_info wiringleft wiringright))
end)
 in ( LrTable.NT 3, ( result, wiring1left, wiring1right), rest671)
end
|  ( 29, ( ( _, ( _, _, GT1right)) :: ( _, ( MlyValue.nlist nlist1, _,
 _)) :: ( _, ( _, LT1left, _)) :: rest671)) => let val  result = 
MlyValue.fports (fn _ => let val  (nlist as nlist1) = nlist1 ()
 in (nlist)
end)
 in ( LrTable.NT 6, ( result, LT1left, GT1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.nlist nlist1, _, nlist1right)) :: _ :: ( _,
 ( MlyValue.id id1, id1left, _)) :: rest671)) => let val  result = 
MlyValue.nlist (fn _ => let val  (id as id1) = id1 ()
 val  (nlist as nlist1) = nlist1 ()
 in (Name.make id :: nlist)
end)
 in ( LrTable.NT 7, ( result, id1left, nlist1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.id id1, id1left, id1right)) :: rest671)) =>
 let val  result = MlyValue.nlist (fn _ => let val  (id as id1) = id1
 ()
 in ([Name.make id])
end)
 in ( LrTable.NT 7, ( result, id1left, id1right), rest671)
end
|  ( 32, ( rest671)) => let val  result = MlyValue.nlist (fn _ => ([])
)
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 33, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.id (fn _ => let val  (ID as ID1) = ID1 ()
 in (ID)
end)
 in ( LrTable.NT 18, ( result, ID1left, ID1right), rest671)
end
|  ( 34, ( ( _, ( _, IDX01left, IDX01right)) :: rest671)) => let val  
result = MlyValue.id (fn _ => ("idx_0"))
 in ( LrTable.NT 18, ( result, IDX01left, IDX01right), rest671)
end
|  ( 35, ( ( _, ( _, IDW1left, IDW1right)) :: rest671)) => let val  
result = MlyValue.id (fn _ => ("idw_"))
 in ( LrTable.NT 18, ( result, IDW1left, IDW1right), rest671)
end
|  ( 36, ( ( _, ( _, IDW01left, IDW01right)) :: rest671)) => let val  
result = MlyValue.id (fn _ => ("idw_0"))
 in ( LrTable.NT 18, ( result, IDW01left, IDW01right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.MERGEn MERGEn1, MERGEn1left, MERGEn1right))
 :: rest671)) => let val  result = MlyValue.id (fn _ => let val  (
MERGEn as MERGEn1) = MERGEn1 ()
 in ("merge_" ^ Int.toString MERGEn)
end)
 in ( LrTable.NT 18, ( result, MERGEn1left, MERGEn1right), rest671)

end
|  ( 38, ( ( _, ( MlyValue.IDPn IDPn1, IDPn1left, IDPn1right)) :: 
rest671)) => let val  result = MlyValue.id (fn _ => let val  (IDPn as 
IDPn1) = IDPn1 ()
 in ("idp_" ^ Int.toString IDPn)
end)
 in ( LrTable.NT 18, ( result, IDPn1left, IDPn1right), rest671)
end
|  ( 39, ( ( _, ( _, _, GT1right)) :: ( _, ( MlyValue.nsetlist 
nsetlist1, _, _)) :: ( _, ( _, LT1left, _)) :: rest671)) => let val  
result = MlyValue.bports (fn _ => let val  (nsetlist as nsetlist1) = 
nsetlist1 ()
 in (nsetlist)
end)
 in ( LrTable.NT 8, ( result, LT1left, GT1right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.nsetlist nsetlist1, _, nsetlist1right)) ::
 _ :: ( _, ( MlyValue.nset nset1, nset1left, _)) :: rest671)) => let
 val  result = MlyValue.nsetlist (fn _ => let val  (nset as nset1) = 
nset1 ()
 val  (nsetlist as nsetlist1) = nsetlist1 ()
 in (nset :: nsetlist)
end)
 in ( LrTable.NT 9, ( result, nset1left, nsetlist1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.nset nset1, nset1left, nset1right)) :: 
rest671)) => let val  result = MlyValue.nsetlist (fn _ => let val  (
nset as nset1) = nset1 ()
 in ([nset])
end)
 in ( LrTable.NT 9, ( result, nset1left, nset1right), rest671)
end
|  ( 42, ( rest671)) => let val  result = MlyValue.nsetlist (fn _ => (
[]))
 in ( LrTable.NT 9, ( result, defaultPos, defaultPos), rest671)
end
|  ( 43, ( ( _, ( _, _, (RBRACEright as RBRACE1right))) :: ( _, ( 
MlyValue.nlist nlist1, _, _)) :: ( _, ( _, (LBRACEleft as LBRACE1left)
, _)) :: rest671)) => let val  result = MlyValue.nset (fn _ => let
 val  (nlist as nlist1) = nlist1 ()
 in (
NameSet.fromList (nlist)
			       handle NameSet.DuplicatesRemoved
				      => raise 
					DuplicateNames 
					  ("bg.grm", (LBRACEleft, RBRACEright),
					   "while parsing")
)
end)
 in ( LrTable.NT 10, ( result, LBRACE1left, RBRACE1right), rest671)

end
|  ( 44, ( ( _, ( MlyValue.CTRLID CTRLID1, CTRLID1left, CTRLID1right))
 :: rest671)) => let val  result = MlyValue.ctrlid (fn _ => let val  (
CTRLID as CTRLID1) = CTRLID1 ()
 in (Control.make (CTRLID, Control.Active, 0, 0))
end)
 in ( LrTable.NT 11, ( result, CTRLID1left, CTRLID1right), rest671)

end
|  ( 45, ( ( _, ( MlyValue.nset nset1, _, nset1right)) :: ( _, ( _, 
SLASH1left, _)) :: rest671)) => let val  result = MlyValue.link (fn _
 => let val  (nset as nset1) = nset1 ()
 in (Link.make {outer = NONE, inner = nset})
end)
 in ( LrTable.NT 12, ( result, SLASH1left, nset1right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.id id2, _, id2right)) :: _ :: ( _, ( 
MlyValue.id id1, id1left, _)) :: rest671)) => let val  result = 
MlyValue.link (fn _ => let val  id1 = id1 ()
 val  id2 = id2 ()
 in (
Link.make {outer = SOME (Name.make id1), 
						    inner = NameSet.singleton (Name.make id2)}
)
end)
 in ( LrTable.NT 12, ( result, id1left, id2right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.id id1, _, id1right)) :: ( _, ( _, 
SLASH1left, _)) :: rest671)) => let val  result = MlyValue.link (fn _
 => let val  (id as id1) = id1 ()
 in (
Link.make {outer = NONE, 
						    inner = NameSet.singleton (Name.make id)}
)
end)
 in ( LrTable.NT 12, ( result, SLASH1left, id1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.nset nset1, _, nset1right)) :: _ :: ( _, ( 
MlyValue.id id1, id1left, _)) :: rest671)) => let val  result = 
MlyValue.link (fn _ => let val  (id as id1) = id1 ()
 val  (nset as nset1) = nset1 ()
 in (Link.make {outer = SOME (Name.make id), inner = nset})
end)
 in ( LrTable.NT 12, ( result, id1left, nset1right), rest671)
end
|  ( 49, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( 
MlyValue.permentrylist permentrylist1, _, _)) :: ( _, ( _, LBRACK1left
, _)) :: rest671)) => let val  result = MlyValue.perm (fn _ => let
 val  (permentrylist as permentrylist1) = permentrylist1 ()
 in (Permutation.make permentrylist)
end)
 in ( LrTable.NT 13, ( result, LBRACK1left, RBRACK1right), rest671)

end
|  ( 50, ( ( _, ( MlyValue.permentrylist permentrylist1, _, 
permentrylist1right)) :: _ :: ( _, ( MlyValue.permentry permentry1, 
permentry1left, _)) :: rest671)) => let val  result = 
MlyValue.permentrylist (fn _ => let val  (permentry as permentry1) = 
permentry1 ()
 val  (permentrylist as permentrylist1) = permentrylist1 ()
 in (permentry :: permentrylist)
end)
 in ( LrTable.NT 14, ( result, permentry1left, permentrylist1right), 
rest671)
end
|  ( 51, ( ( _, ( MlyValue.permentry permentry1, permentry1left, 
permentry1right)) :: rest671)) => let val  result = 
MlyValue.permentrylist (fn _ => let val  (permentry as permentry1) = 
permentry1 ()
 in ([permentry])
end)
 in ( LrTable.NT 14, ( result, permentry1left, permentry1right), 
rest671)
end
|  ( 52, ( rest671)) => let val  result = MlyValue.permentrylist (fn _
 => ([]))
 in ( LrTable.NT 14, ( result, defaultPos, defaultPos), rest671)
end
|  ( 53, ( ( _, ( MlyValue.nset nset1, _, nset1right)) :: ( _, ( 
MlyValue.int int1, int1left, _)) :: rest671)) => let val  result = 
MlyValue.permentry (fn _ => let val  (int as int1) = int1 ()
 val  (nset as nset1) = nset1 ()
 in ((int, nset))
end)
 in ( LrTable.NT 15, ( result, int1left, nset1right), rest671)
end
|  ( 54, ( ( _, ( MlyValue.int int1, int1left, int1right)) :: rest671)
) => let val  result = MlyValue.permentry (fn _ => let val  (int as 
int1) = int1 ()
 in ((int, NameSet.empty))
end)
 in ( LrTable.NT 15, ( result, int1left, int1right), rest671)
end
|  ( 55, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( 
MlyValue.wiringentrylist wiringentrylist1, _, _)) :: _ :: _ :: _ :: (
 _, ( _, SLASH1left, _)) :: rest671)) => let val  result = 
MlyValue.wiring (fn _ => let val  (wiringentrylist as wiringentrylist1
) = wiringentrylist1 ()
 in (Wiring.make' wiringentrylist)
end)
 in ( LrTable.NT 16, ( result, SLASH1left, RBRACK1right), rest671)
end
|  ( 56, ( ( _, ( MlyValue.wiringentrylist wiringentrylist1, _, 
wiringentrylist1right)) :: _ :: ( _, ( MlyValue.link link1, link1left,
 _)) :: rest671)) => let val  result = MlyValue.wiringentrylist (fn _
 => let val  (link as link1) = link1 ()
 val  (wiringentrylist as wiringentrylist1) = wiringentrylist1 ()
 in (link :: wiringentrylist)
end)
 in ( LrTable.NT 17, ( result, link1left, wiringentrylist1right), 
rest671)
end
|  ( 57, ( ( _, ( MlyValue.link link1, link1left, link1right)) :: 
rest671)) => let val  result = MlyValue.wiringentrylist (fn _ => let
 val  (link as link1) = link1 ()
 in ([link])
end)
 in ( LrTable.NT 17, ( result, link1left, link1right), rest671)
end
|  ( 58, ( rest671)) => let val  result = MlyValue.wiringentrylist (fn
 _ => ([]))
 in ( LrTable.NT 17, ( result, defaultPos, defaultPos), rest671)
end
|  ( 59, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671)
) => let val  result = MlyValue.int (fn _ => let val  (INT as INT1) = 
INT1 ()
 in (INT)
end)
 in ( LrTable.NT 19, ( result, INT1left, INT1right), rest671)
end
|  ( 60, ( ( _, ( _, ONE1left, ONE1right)) :: rest671)) => let val  
result = MlyValue.int (fn _ => (1))
 in ( LrTable.NT 19, ( result, ONE1left, ONE1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.prog x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : BgTerm_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun CTRLID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.CTRLID (fn () => i),p1,p2))
fun MERGEn (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.MERGEn (fn () => i),p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun SLASH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun OO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun XX (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun PRI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun PAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun QUOTE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun BQUOTE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun IDX0 (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun IDBB0 (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun ONE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun IDW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun IDW0 (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun IDPn (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.IDPn (fn () => i),p1,p2))
fun AT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun ATAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
end
end
