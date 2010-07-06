
functor RulesLrVals
		   (structure Info : INFO
		    structure Token : TOKEN
		    structure Interface : INTERFACE
		    structure Control : CONTROL
		    structure Name : NAME
		    structure NameSet : MONO_SET
		    structure Link : LINK
		    structure LinkSet : MONO_SET
		    structure Wiring : WIRING
		    structure Ion : ION
		    structure Permutation : PERMUTATION
		    structure BgTerm : BGTERM
		    structure BgVal : BGVAL
		    structure BgBDNF : BGBDNF
		    structure Instantiation : INSTANTIATION
		    sharing type Control.control = Ion.control
		    sharing type Interface.interface = Instantiation.interface
		    sharing type Name.name = 
				 NameSet.elt = 
				 Instantiation.name = 
				 Link.name =
				 Ion.name
		    sharing type NameSet.Set =
				 Interface.nameset =
				 Link.nameset =
				 Ion.nameset =
				 Permutation.nameset =
				 BgTerm.nameset
		    sharing type Link.link = LinkSet.elt
		    sharing type LinkSet.Set = Wiring.linkset
		    sharing type Wiring.wiring = BgTerm.wiring
		    sharing type Ion.ion = BgTerm.ion
		    sharing type Permutation.permutation =
				 BgTerm.permutation
		    sharing type Permutation.Immutable =
		                 BgTerm.Immutable
                    sharing type Info.info =
                                 BgTerm.info =
                                 BgVal.info
                    sharing type BgTerm.bgterm = BgVal.bgterm
                    sharing type BgVal.bgval = BgBDNF.bgval
                    sharing type BgBDNF.interface =
                                 BgVal.interface =
                                 Instantiation.interface
		    )
	 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* Simple grammar file for ML-Yacc (SML/NJ) defining the parser 
   for reaction rules. 

   Functionality - for now - just read a file containing a single dec 
   on the form:

   bg

   Syntax and grammar summary :

   bg := <->
      |  merge(n)
      |  `nset`
      |  name/name 
      |  -/name 
      |  name//nset
      |  -//nset
      |  ctrlid nset nsetlist
      |  ctrlid nset
      |  ctrlid
      |  @[ permentrylist ]
      |  @@[ permentrylist ]
      |  <nset> bg
      |  bg * bg
      |  bg `|` bg
      |  bg || bg
      |  bg o bg
  
   nsetlist      = [nsets]
                 | []
   
   nsets         = nset
	         | nset, nsets

   nset          = [names]
                 | []

   names         = name
	         | name, names

   permentrylist = [permentries]
                 | []
   
   permentries   = permentry 
		 | permentry, permentrylist

   permentry     = int & nset
                 | int
   
   name   = [a-n|p-v|yz]
          | [a-z][A-za-z0-9_]+

   (single o and * are keywords)

   ctrlid  = [A-Z][A-za-z0-9_]*

   - and ML-style comments (* ... *) 

-----

  Precedences (with tightest binding at bottom):

  ( )         abstraction
  *, <|>, ||  tensor product, prime product, parallel product
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
type kind = Control.kind
type name = Name.name
type link = Link.link
type nameset = NameSet.Set
type linkset = LinkSet.Set
type ion = Ion.ion
type 'kind permutation = 'kind Permutation.permutation
type map = (int * name list) * (int * name list)
type inst = Instantiation.inst
type rule = {
  name : string,
  redex : bgterm,
  react : bgterm,
  maps : map list,
  info : info}
datatype ctrlarity = INTARITY of int * int
                     | PORTNAMES of string list * string list
datatype ports = ORDEREDPORTS of name list * nameset list
                | NAMEDPORTS of (string * name) list * (string * name) list
datatype fieldval = REDEX    of bgterm
                  | REACT    of bgterm
                  | INST     of map list * (int * int)
                  | RULENAME of string
type rulefield = {name : string, value : fieldval}
datatype parserresult
  = BGTERMRESULT of bgterm
  | RULESRESULT of rule list
  | SIGRESULT of control list

(** Signal that a set with duplicate names has been encountered. 
 * @params smlfile leftpos rightpos errtxt
 * @param smlfile   SML file with the code that encountered the error.
 * @param leftpos   Position of lefthand side of error term.
 * @param rightpos  Position of righthand side of error term.
 * @param errtxt    Text detailing the error.
*)
exception DuplicateNames of string * (int * int) * string

exception UnknownField of string * (int * int)

exception WrongFieldType of int * int

exception RedexOrReactMissing of int * int

fun warning str v = (print ("Parser warning: "^str^"\n"); v)

fun mk_origin p1 p2
  = Origin.mk_file_origin
      (!ErrorMsg.fileName)
      (Origin.POS (ErrorMsg.toCoords p1 p2))

fun mk_info p1 p2 = Info.make (mk_origin p1 p2)

val nextruleno = ref 0
fun mk_rule (rulefields, (left, right)) =
  let
    fun populate {name = NONE, redex, react, inst, info} []
      = populate
         {name = SOME (Int.toString (!nextruleno)
                       before nextruleno := !nextruleno + 1),
          redex = redex, react = react, inst = inst, info = info} []
      | populate {name, redex, react, inst = NONE, info} []
      = populate {name = name, redex = redex,
                  react = react, inst = SOME [], info = info} []
      | populate {name = SOME name, redex = SOME redex,
                  react = SOME react, inst = SOME inst, info} []
      = {name = name, redex = redex, react = react,
        maps = inst, info = info}
      | populate _ [] = raise RedexOrReactMissing (left, right)
      | populate {name, redex, react, inst, info}
                 ((RULENAME name', _) :: rulefields)
      = populate {name = SOME name', redex = redex,
                  react = react, inst = inst, info = info} rulefields
      | populate {name, redex, react, inst, info}
                 ((REDEX   redex', _) :: rulefields)
      = populate {name = name, redex = SOME redex',
                  react = react, inst = inst, info = info} rulefields
      | populate {name, redex, react, inst, info}
                 ((REACT   react', _) :: rulefields)
      = populate {name = name, redex = redex,
                  react = SOME react', inst = inst, info = info} rulefields
      | populate {name, redex, react, inst, info}
                 ((INST (inst', p'), _) :: rulefields)
      = populate {name = name, redex = redex, react = react,
                  inst = SOME inst', info = info} rulefields
  in
    populate
      {name = NONE, redex = NONE, react = NONE, inst = NONE,
       info = mk_info left right}
      rulefields
  end

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
\\001\000\001\000\245\000\029\000\245\000\033\000\245\000\036\000\019\001\
\\037\000\019\001\040\000\245\000\041\000\245\000\042\000\245\000\
\\043\000\245\000\045\000\245\000\047\000\245\000\000\000\
\\001\000\001\000\251\000\029\000\251\000\033\000\251\000\036\000\021\001\
\\037\000\021\001\040\000\251\000\041\000\251\000\042\000\251\000\
\\043\000\251\000\045\000\251\000\047\000\251\000\000\000\
\\001\000\002\000\005\000\003\000\004\000\004\000\003\000\000\000\
\\001\000\005\000\036\000\007\000\035\000\023\000\034\000\024\000\033\000\
\\025\000\032\000\026\000\031\000\027\000\030\000\028\000\029\000\
\\035\000\028\000\038\000\027\000\039\000\026\000\044\000\025\000\
\\046\000\024\000\048\000\023\000\049\000\022\000\050\000\021\000\
\\051\000\020\000\052\000\019\000\053\000\018\000\054\000\017\000\
\\055\000\016\000\000\000\
\\001\000\005\000\036\000\023\000\034\000\024\000\033\000\025\000\032\000\
\\026\000\031\000\027\000\073\000\031\000\092\000\050\000\072\000\
\\053\000\071\000\054\000\070\000\055\000\069\000\000\000\
\\001\000\005\000\036\000\023\000\034\000\024\000\033\000\025\000\032\000\
\\026\000\031\000\027\000\073\000\031\000\101\000\050\000\072\000\
\\053\000\071\000\054\000\070\000\055\000\069\000\000\000\
\\001\000\005\000\036\000\023\000\034\000\024\000\033\000\025\000\032\000\
\\026\000\031\000\027\000\073\000\031\000\191\000\050\000\072\000\
\\053\000\071\000\054\000\070\000\055\000\069\000\000\000\
\\001\000\005\000\036\000\023\000\034\000\024\000\033\000\025\000\032\000\
\\026\000\031\000\027\000\073\000\031\000\195\000\050\000\072\000\
\\053\000\071\000\054\000\070\000\055\000\069\000\000\000\
\\001\000\005\000\036\000\023\000\034\000\024\000\033\000\025\000\032\000\
\\026\000\031\000\027\000\073\000\050\000\072\000\053\000\071\000\
\\054\000\070\000\055\000\069\000\000\000\
\\001\000\006\000\060\000\028\000\059\000\052\000\058\000\000\000\
\\001\000\006\000\060\000\031\000\160\000\052\000\058\000\000\000\
\\001\000\006\000\060\000\052\000\058\000\000\000\
\\001\000\007\000\111\000\000\000\
\\001\000\008\000\143\000\000\000\
\\001\000\009\000\043\000\010\000\042\000\011\000\041\000\000\000\
\\001\000\009\000\043\000\010\000\042\000\011\000\041\000\031\000\040\000\000\000\
\\001\000\012\000\171\000\014\000\170\000\000\000\
\\001\000\012\000\202\000\000\000\
\\001\000\013\000\116\000\000\000\
\\001\000\013\000\117\000\000\000\
\\001\000\013\000\118\000\000\000\
\\001\000\013\000\119\000\000\000\
\\001\000\015\000\193\000\000\000\
\\001\000\020\000\169\000\000\000\
\\001\000\021\000\166\000\000\000\
\\001\000\022\000\188\000\000\000\
\\001\000\023\000\087\000\024\000\086\000\025\000\085\000\026\000\084\000\000\000\
\\001\000\028\000\077\000\000\000\
\\001\000\029\000\109\000\040\000\055\000\041\000\054\000\042\000\053\000\
\\043\000\052\000\000\000\
\\001\000\029\000\125\000\000\000\
\\001\000\029\000\133\000\000\000\
\\001\000\030\000\007\000\000\000\
\\001\000\030\000\009\000\000\000\
\\001\000\030\000\062\000\000\000\
\\001\000\030\000\063\000\000\000\
\\001\000\030\000\064\000\000\000\
\\001\000\030\000\140\000\000\000\
\\001\000\030\000\153\000\000\000\
\\001\000\030\000\177\000\000\000\
\\001\000\030\000\186\000\000\000\
\\001\000\031\000\047\000\032\000\046\000\000\000\
\\001\000\031\000\078\000\000\000\
\\001\000\031\000\080\000\000\000\
\\001\000\031\000\122\000\045\000\121\000\000\000\
\\001\000\031\000\126\000\000\000\
\\001\000\031\000\130\000\000\000\
\\001\000\031\000\131\000\000\000\
\\001\000\031\000\162\000\045\000\161\000\000\000\
\\001\000\031\000\163\000\000\000\
\\001\000\031\000\165\000\000\000\
\\001\000\031\000\172\000\000\000\
\\001\000\031\000\182\000\000\000\
\\001\000\031\000\189\000\000\000\
\\001\000\031\000\198\000\000\000\
\\001\000\031\000\200\000\000\000\
\\001\000\032\000\046\000\000\000\
\\001\000\033\000\114\000\000\000\
\\001\000\034\000\108\000\000\000\
\\001\000\036\000\049\000\037\000\048\000\000\000\
\\001\000\040\000\055\000\041\000\054\000\042\000\053\000\043\000\052\000\
\\047\000\106\000\000\000\
\\001\000\044\000\107\000\000\000\
\\206\000\040\000\055\000\041\000\054\000\042\000\053\000\043\000\052\000\000\000\
\\207\000\000\000\
\\208\000\000\000\
\\209\000\000\000\
\\210\000\000\000\
\\211\000\045\000\079\000\000\000\
\\212\000\000\000\
\\213\000\000\000\
\\214\000\000\000\
\\215\000\000\000\
\\216\000\000\000\
\\217\000\016\000\137\000\017\000\136\000\018\000\135\000\019\000\134\000\000\000\
\\218\000\000\000\
\\219\000\000\000\
\\220\000\000\000\
\\221\000\000\000\
\\222\000\000\000\
\\223\000\000\000\
\\224\000\045\000\081\000\000\000\
\\225\000\000\000\
\\226\000\000\000\
\\227\000\045\000\115\000\000\000\
\\228\000\000\000\
\\229\000\040\000\055\000\041\000\054\000\042\000\053\000\043\000\052\000\000\000\
\\230\000\040\000\055\000\041\000\054\000\042\000\053\000\043\000\052\000\000\000\
\\231\000\000\000\
\\232\000\000\000\
\\233\000\000\000\
\\234\000\000\000\
\\235\000\045\000\173\000\000\000\
\\236\000\000\000\
\\237\000\000\000\
\\238\000\000\000\
\\239\000\000\000\
\\240\000\040\000\055\000\041\000\054\000\042\000\053\000\043\000\052\000\000\000\
\\241\000\040\000\055\000\042\000\053\000\000\000\
\\242\000\040\000\055\000\041\000\054\000\042\000\053\000\043\000\052\000\000\000\
\\243\000\040\000\055\000\000\000\
\\244\000\000\000\
\\246\000\000\000\
\\247\000\000\000\
\\248\000\000\000\
\\249\000\000\000\
\\250\000\000\000\
\\252\000\000\000\
\\253\000\030\000\051\000\000\000\
\\254\000\000\000\
\\255\000\000\000\
\\000\001\040\000\055\000\041\000\054\000\042\000\053\000\043\000\052\000\000\000\
\\001\001\000\000\
\\002\001\000\000\
\\003\001\000\000\
\\004\001\000\000\
\\005\001\000\000\
\\006\001\000\000\
\\007\001\000\000\
\\008\001\000\000\
\\009\001\000\000\
\\010\001\000\000\
\\011\001\045\000\201\000\000\000\
\\012\001\000\000\
\\013\001\045\000\127\000\000\000\
\\014\001\000\000\
\\014\001\022\000\120\000\000\000\
\\015\001\000\000\
\\016\001\045\000\181\000\000\000\
\\017\001\005\000\036\000\023\000\034\000\024\000\033\000\025\000\032\000\
\\026\000\031\000\027\000\073\000\050\000\072\000\053\000\071\000\
\\054\000\070\000\055\000\069\000\000\000\
\\018\001\000\000\
\\019\001\000\000\
\\020\001\000\000\
\\020\001\030\000\062\000\000\000\
\\021\001\000\000\
\\022\001\000\000\
\\022\001\006\000\060\000\028\000\059\000\052\000\058\000\000\000\
\\023\001\000\000\
\\023\001\006\000\060\000\028\000\059\000\052\000\058\000\000\000\
\\024\001\000\000\
\\025\001\000\000\
\\026\001\000\000\
\\027\001\000\000\
\\028\001\000\000\
\\029\001\030\000\124\000\000\000\
\\030\001\000\000\
\\031\001\045\000\164\000\000\000\
\\032\001\030\000\062\000\000\000\
\\033\001\000\000\
\\034\001\000\000\
\\035\001\000\000\
\\036\001\000\000\
\\037\001\000\000\
\\038\001\000\000\
\\039\001\000\000\
\\040\001\000\000\
\\041\001\000\000\
\\042\001\000\000\
\\043\001\000\000\
\\044\001\000\000\
\\045\001\000\000\
\\046\001\045\000\129\000\000\000\
\\047\001\006\000\060\000\052\000\058\000\000\000\
\\048\001\000\000\
\\049\001\012\000\128\000\000\000\
\\050\001\000\000\
\\051\001\000\000\
\\052\001\000\000\
\\053\001\000\000\
\"
val actionRowNumbers =
"\003\000\032\000\033\000\004\000\
\\064\000\016\000\063\000\041\000\
\\059\000\108\000\104\000\107\000\
\\095\000\062\000\137\000\002\000\
\\132\000\100\000\101\000\001\000\
\\035\000\036\000\004\000\034\000\
\\034\000\009\000\034\000\004\000\
\\135\000\141\000\140\000\139\000\
\\138\000\152\000\129\000\028\000\
\\042\000\067\000\066\000\072\000\
\\071\000\070\000\043\000\080\000\
\\027\000\079\000\034\000\009\000\
\\106\000\005\000\004\000\004\000\
\\004\000\004\000\164\000\109\000\
\\167\000\010\000\166\000\105\000\
\\006\000\161\000\161\000\060\000\
\\061\000\153\000\155\000\136\000\
\\133\000\131\000\130\000\134\000\
\\058\000\029\000\102\000\013\000\
\\065\000\015\000\078\000\056\000\
\\057\000\083\000\019\000\020\000\
\\021\000\022\000\156\000\154\000\
\\125\000\044\000\143\000\098\000\
\\097\000\096\000\099\000\030\000\
\\124\000\045\000\123\000\150\000\
\\163\000\160\000\046\000\047\000\
\\112\000\103\000\004\000\111\000\
\\031\000\073\000\068\000\081\000\
\\082\000\027\000\037\000\004\000\
\\004\000\014\000\009\000\009\000\
\\143\000\113\000\146\000\165\000\
\\151\000\009\000\034\000\161\000\
\\158\000\157\000\110\000\069\000\
\\038\000\038\000\012\000\012\000\
\\084\000\087\000\011\000\086\000\
\\085\000\088\000\048\000\049\000\
\\114\000\145\000\050\000\122\000\
\\162\000\159\000\025\000\128\000\
\\076\000\024\000\074\000\017\000\
\\051\000\091\000\090\000\009\000\
\\039\000\143\000\146\000\142\000\
\\038\000\127\000\052\000\012\000\
\\012\000\040\000\089\000\012\000\
\\026\000\053\000\116\000\007\000\
\\115\000\144\000\077\000\128\000\
\\149\000\075\000\093\000\023\000\
\\008\000\092\000\009\000\039\000\
\\054\000\119\000\126\000\012\000\
\\055\000\147\000\121\000\117\000\
\\118\000\018\000\148\000\009\000\
\\040\000\120\000\094\000\000\000"
val gotoT =
"\
\\001\000\203\000\000\000\
\\003\000\004\000\000\000\
\\008\000\006\000\000\000\
\\015\000\013\000\016\000\012\000\028\000\011\000\029\000\010\000\
\\030\000\009\000\033\000\008\000\000\000\
\\000\000\
\\002\000\037\000\004\000\036\000\005\000\035\000\000\000\
\\000\000\
\\007\000\043\000\009\000\042\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\022\000\048\000\000\000\
\\000\000\
\\000\000\
\\034\000\055\000\035\000\054\000\000\000\
\\000\000\
\\027\000\059\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\015\000\063\000\016\000\012\000\028\000\011\000\029\000\010\000\
\\030\000\009\000\033\000\008\000\000\000\
\\027\000\064\000\000\000\
\\027\000\065\000\000\000\
\\033\000\066\000\000\000\
\\027\000\072\000\000\000\
\\015\000\073\000\016\000\012\000\028\000\011\000\029\000\010\000\
\\030\000\009\000\033\000\008\000\000\000\
\\034\000\074\000\035\000\054\000\000\000\
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
\\000\000\
\\010\000\081\000\011\000\080\000\000\000\
\\000\000\
\\027\000\086\000\000\000\
\\033\000\087\000\000\000\
\\000\000\
\\020\000\089\000\033\000\088\000\000\000\
\\015\000\091\000\016\000\012\000\028\000\011\000\029\000\010\000\
\\030\000\009\000\033\000\008\000\000\000\
\\015\000\092\000\016\000\012\000\028\000\011\000\029\000\010\000\
\\030\000\009\000\033\000\008\000\000\000\
\\015\000\093\000\016\000\012\000\028\000\011\000\029\000\010\000\
\\030\000\009\000\033\000\008\000\000\000\
\\015\000\094\000\016\000\012\000\028\000\011\000\029\000\010\000\
\\030\000\009\000\033\000\008\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\034\000\095\000\035\000\054\000\000\000\
\\000\000\
\\000\000\
\\020\000\098\000\021\000\097\000\033\000\096\000\000\000\
\\031\000\102\000\032\000\101\000\035\000\100\000\000\000\
\\031\000\103\000\032\000\101\000\035\000\100\000\000\000\
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
\\006\000\108\000\000\000\
\\000\000\
\\002\000\037\000\004\000\110\000\005\000\035\000\000\000\
\\000\000\
\\007\000\043\000\009\000\111\000\000\000\
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
\\023\000\121\000\000\000\
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
\\000\000\
\\015\000\130\000\016\000\012\000\028\000\011\000\029\000\010\000\
\\030\000\009\000\033\000\008\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\081\000\011\000\136\000\000\000\
\\012\000\137\000\000\000\
\\015\000\139\000\016\000\012\000\028\000\011\000\029\000\010\000\
\\030\000\009\000\033\000\008\000\000\000\
\\015\000\140\000\016\000\012\000\028\000\011\000\029\000\010\000\
\\030\000\009\000\033\000\008\000\000\000\
\\000\000\
\\020\000\142\000\033\000\096\000\000\000\
\\020\000\098\000\021\000\143\000\033\000\096\000\000\000\
\\023\000\144\000\000\000\
\\000\000\
\\026\000\146\000\027\000\145\000\000\000\
\\000\000\
\\000\000\
\\020\000\098\000\021\000\147\000\033\000\096\000\000\000\
\\027\000\148\000\000\000\
\\031\000\149\000\032\000\101\000\035\000\100\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\017\000\150\000\000\000\
\\017\000\152\000\000\000\
\\035\000\153\000\000\000\
\\035\000\154\000\000\000\
\\000\000\
\\000\000\
\\013\000\157\000\014\000\156\000\035\000\155\000\000\000\
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
\\018\000\166\000\033\000\165\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\024\000\173\000\033\000\172\000\000\000\
\\025\000\174\000\000\000\
\\023\000\176\000\000\000\
\\026\000\177\000\027\000\145\000\000\000\
\\000\000\
\\017\000\178\000\000\000\
\\000\000\
\\000\000\
\\035\000\181\000\000\000\
\\035\000\182\000\000\000\
\\019\000\183\000\000\000\
\\000\000\
\\013\000\157\000\014\000\185\000\035\000\155\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\024\000\188\000\033\000\172\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\018\000\190\000\033\000\165\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\020\000\098\000\021\000\192\000\033\000\096\000\000\000\
\\000\000\
\\020\000\194\000\033\000\096\000\000\000\
\\025\000\195\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\035\000\197\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\024\000\201\000\033\000\172\000\000\000\
\\019\000\202\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 204
val numrules = 104
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
 | STRING of unit ->  (string) | CTRLID of unit ->  (string)
 | INT of unit ->  (int) | ID of unit ->  (string)
 | int of unit ->  (int) | pint of unit ->  (int)
 | id of unit ->  (string)
 | permentry of unit ->  ( ( int * nameset ) )
 | permentrylist of unit ->  ( ( int * nameset )  list)
 | perm of unit ->  (Immutable permutation)
 | link of unit ->  (link list) | ctrlid of unit ->  (control)
 | nset of unit ->  (nameset) | nsetlist of unit ->  (nameset list)
 | portsassign of unit ->  ( ( string * name )  list)
 | portassigns of unit ->  ( ( string * name )  list)
 | bports of unit ->  (nameset list) | ports of unit ->  (ports)
 | names of unit ->  (name list) | name of unit ->  (name)
 | nlist of unit ->  (name list) | strings of unit ->  (string list)
 | slist of unit ->  (string list) | altbg of unit ->  (bgterm)
 | bg of unit ->  (bgterm) | maps of unit ->  (map list)
 | map of unit ->  (map) | inst of unit ->  (map list* ( int * int ) )
 | rulefields of unit ->  ( ( fieldval * (int * int) )  list)
 | rulefield of unit ->  (fieldval) | rules of unit ->  (rule list)
 | rulelist of unit ->  (rule list) | rule of unit ->  (rule)
 | cdata of unit ->  (string*ctrlarity) | kind of unit ->  (kind)
 | ctrls of unit ->  (control list)
 | ctrllist of unit ->  (control list) | ctrl of unit ->  (control)
 | start of unit ->  (parserresult)
end
type svalue = MlyValue.svalue
type result = parserresult
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
  | (T 1) => "BGTERM"
  | (T 2) => "RULELIST"
  | (T 3) => "SIGNATURE"
  | (T 4) => "ID"
  | (T 5) => "INT"
  | (T 6) => "CTRLID"
  | (T 7) => "STRING"
  | (T 8) => "ACTIVE"
  | (T 9) => "PASSIVE"
  | (T 10) => "ATOMIC"
  | (T 11) => "AMP"
  | (T 12) => "EQ"
  | (T 13) => "MAPSTO"
  | (T 14) => "MAPPSTO"
  | (T 15) => "DASHCOLON"
  | (T 16) => "EQCOLON"
  | (T 17) => "DASHDASHCOLON"
  | (T 18) => "EQEQCOLON"
  | (T 19) => "RARROW"
  | (T 20) => "RRARROW"
  | (T 21) => "EQEQ"
  | (T 22) => "NAME"
  | (T 23) => "REDEX"
  | (T 24) => "REACT"
  | (T 25) => "INST"
  | (T 26) => "MERGE"
  | (T 27) => "LPAREN"
  | (T 28) => "RPAREN"
  | (T 29) => "LBRACK"
  | (T 30) => "RBRACK"
  | (T 31) => "LBRACE"
  | (T 32) => "RBRACE"
  | (T 33) => "GT"
  | (T 34) => "LT"
  | (T 35) => "SLASH"
  | (T 36) => "SLASHSLASH"
  | (T 37) => "DASHSLASH"
  | (T 38) => "DASHSLASHSLASH"
  | (T 39) => "OO"
  | (T 40) => "XX"
  | (T 41) => "PRI"
  | (T 42) => "PAR"
  | (T 43) => "BQUOTE"
  | (T 44) => "COMMA"
  | (T 45) => "LTLT"
  | (T 46) => "GTGT"
  | (T 47) => "AT"
  | (T 48) => "ATAT"
  | (T 49) => "IDX0"
  | (T 50) => "IDBB0"
  | (T 51) => "ONE"
  | (T 52) => "IDW"
  | (T 53) => "IDW0"
  | (T 54) => "IDP"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 54) $$ (T 53) $$ (T 52) $$ (T 51) $$ (T 50) $$ (T 49) $$ (T 48)
 $$ (T 47) $$ (T 46) $$ (T 45) $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41)
 $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34)
 $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27)
 $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20)
 $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13)
 $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 3) $$ (T 2) $$ 
(T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.bg bg1, _, bg1right)) :: ( _, ( _, 
BGTERM1left, _)) :: rest671)) => let val  result = MlyValue.start (fn
 _ => let val  (bg as bg1) = bg1 ()
 in (BGTERMRESULT bg)
end)
 in ( LrTable.NT 0, ( result, BGTERM1left, bg1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.rulelist rulelist1, _, rulelist1right)) :: (
 _, ( _, RULELIST1left, _)) :: rest671)) => let val  result = 
MlyValue.start (fn _ => let val  (rulelist as rulelist1) = rulelist1
 ()
 in (RULESRESULT rulelist)
end)
 in ( LrTable.NT 0, ( result, RULELIST1left, rulelist1right), rest671)

end
|  ( 2, ( ( _, ( MlyValue.ctrllist ctrllist1, _, ctrllist1right)) :: (
 _, ( _, SIGNATURE1left, _)) :: rest671)) => let val  result = 
MlyValue.start (fn _ => let val  (ctrllist as ctrllist1) = ctrllist1
 ()
 in (SIGRESULT ctrllist)
end)
 in ( LrTable.NT 0, ( result, SIGNATURE1left, ctrllist1right), rest671
)
end
|  ( 3, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.ctrls ctrls1,
 _, _)) :: ( _, ( _, LBRACK1left, _)) :: rest671)) => let val  result
 = MlyValue.ctrllist (fn _ => let val  (ctrls as ctrls1) = ctrls1 ()
 in (ctrls)
end)
 in ( LrTable.NT 2, ( result, LBRACK1left, RBRACK1right), rest671)
end
|  ( 4, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( _, LBRACK1left, _)) ::
 rest671)) => let val  result = MlyValue.ctrllist (fn _ => ([]))
 in ( LrTable.NT 2, ( result, LBRACK1left, RBRACK1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.ctrl ctrl1, ctrl1left, ctrl1right)) :: 
rest671)) => let val  result = MlyValue.ctrls (fn _ => let val  (ctrl
 as ctrl1) = ctrl1 ()
 in ([ctrl])
end)
 in ( LrTable.NT 3, ( result, ctrl1left, ctrl1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.ctrls ctrls1, _, ctrls1right)) :: _ :: ( _, 
( MlyValue.ctrl ctrl1, ctrl1left, _)) :: rest671)) => let val  result
 = MlyValue.ctrls (fn _ => let val  (ctrl as ctrl1) = ctrl1 ()
 val  (ctrls as ctrls1) = ctrls1 ()
 in (ctrl :: ctrls)
end)
 in ( LrTable.NT 3, ( result, ctrl1left, ctrls1right), rest671)
end
|  ( 7, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.cdata cdata1,
 _, _)) :: _ :: ( _, ( MlyValue.kind kind1, kind1left, _)) :: rest671)
) => let val  result = MlyValue.ctrl (fn _ => let val  (kind as kind1)
 = kind1 ()
 val  (cdata as cdata1) = cdata1 ()
 in (
case #2 cdata of
                                 INTARITY (b, f) =>
                                 Control.make (#1 cdata, kind, b, f)
                               | PORTNAMES (b, f) =>
                                 Control.make' (#1 cdata, kind, b, f)
)
end)
 in ( LrTable.NT 1, ( result, kind1left, RPAREN1right), rest671)
end
|  ( 8, ( ( _, ( _, ACTIVE1left, ACTIVE1right)) :: rest671)) => let
 val  result = MlyValue.kind (fn _ => (Control.Active))
 in ( LrTable.NT 4, ( result, ACTIVE1left, ACTIVE1right), rest671)
end
|  ( 9, ( ( _, ( _, PASSIVE1left, PASSIVE1right)) :: rest671)) => let
 val  result = MlyValue.kind (fn _ => (Control.Passive))
 in ( LrTable.NT 4, ( result, PASSIVE1left, PASSIVE1right), rest671)

end
|  ( 10, ( ( _, ( _, ATOMIC1left, ATOMIC1right)) :: rest671)) => let
 val  result = MlyValue.kind (fn _ => (Control.Atomic))
 in ( LrTable.NT 4, ( result, ATOMIC1left, ATOMIC1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.CTRLID CTRLID1, CTRLID1left, CTRLID1right))
 :: rest671)) => let val  result = MlyValue.cdata (fn _ => let val  (
CTRLID as CTRLID1) = CTRLID1 ()
 in ((CTRLID, INTARITY (0, 0)))
end)
 in ( LrTable.NT 5, ( result, CTRLID1left, CTRLID1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.int int1, _, int1right)) :: _ :: ( _, ( 
MlyValue.CTRLID CTRLID1, CTRLID1left, _)) :: rest671)) => let val  
result = MlyValue.cdata (fn _ => let val  (CTRLID as CTRLID1) = 
CTRLID1 ()
 val  (int as int1) = int1 ()
 in ((CTRLID, INTARITY (0, int)))
end)
 in ( LrTable.NT 5, ( result, CTRLID1left, int1right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.int int2, _, int2right)) :: _ :: ( _, ( 
MlyValue.int int1, _, _)) :: _ :: ( _, ( MlyValue.CTRLID CTRLID1, 
CTRLID1left, _)) :: rest671)) => let val  result = MlyValue.cdata (fn
 _ => let val  (CTRLID as CTRLID1) = CTRLID1 ()
 val  int1 = int1 ()
 val  int2 = int2 ()
 in ((CTRLID, INTARITY (int1, int2)))
end)
 in ( LrTable.NT 5, ( result, CTRLID1left, int2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.slist slist1, _, slist1right)) :: _ :: ( _,
 ( MlyValue.CTRLID CTRLID1, CTRLID1left, _)) :: rest671)) => let val  
result = MlyValue.cdata (fn _ => let val  (CTRLID as CTRLID1) = 
CTRLID1 ()
 val  (slist as slist1) = slist1 ()
 in ((CTRLID, PORTNAMES ([], slist)))
end)
 in ( LrTable.NT 5, ( result, CTRLID1left, slist1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.slist slist2, _, slist2right)) :: _ :: ( _,
 ( MlyValue.slist slist1, _, _)) :: _ :: ( _, ( MlyValue.CTRLID 
CTRLID1, CTRLID1left, _)) :: rest671)) => let val  result = 
MlyValue.cdata (fn _ => let val  (CTRLID as CTRLID1) = CTRLID1 ()
 val  slist1 = slist1 ()
 val  slist2 = slist2 ()
 in ((CTRLID, PORTNAMES (slist1, slist2)))
end)
 in ( LrTable.NT 5, ( result, CTRLID1left, slist2right), rest671)
end
|  ( 16, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.rules rules1
, _, _)) :: ( _, ( _, LBRACK1left, _)) :: rest671)) => let val  result
 = MlyValue.rulelist (fn _ => let val  (rules as rules1) = rules1 ()
 in (rules)
end)
 in ( LrTable.NT 7, ( result, LBRACK1left, RBRACK1right), rest671)
end
|  ( 17, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( _, LBRACK1left, _))
 :: rest671)) => let val  result = MlyValue.rulelist (fn _ => ([]))
 in ( LrTable.NT 7, ( result, LBRACK1left, RBRACK1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.rule rule1, rule1left, rule1right)) :: 
rest671)) => let val  result = MlyValue.rules (fn _ => let val  (rule
 as rule1) = rule1 ()
 in ([rule])
end)
 in ( LrTable.NT 8, ( result, rule1left, rule1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.rules rules1, _, rules1right)) :: _ :: ( _,
 ( MlyValue.rule rule1, rule1left, _)) :: rest671)) => let val  result
 = MlyValue.rules (fn _ => let val  (rule as rule1) = rule1 ()
 val  (rules as rules1) = rules1 ()
 in (rule :: rules)
end)
 in ( LrTable.NT 8, ( result, rule1left, rules1right), rest671)
end
|  ( 20, ( ( _, ( _, _, (RBRACEright as RBRACE1right))) :: ( _, ( 
MlyValue.rulefields rulefields1, _, _)) :: ( _, ( _, (LBRACEleft as 
LBRACE1left), _)) :: rest671)) => let val  result = MlyValue.rule (fn
 _ => let val  (rulefields as rulefields1) = rulefields1 ()
 in (
mk_rule (rulefields,
                                        (LBRACEleft, RBRACEright))
)
end)
 in ( LrTable.NT 6, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.rulefield rulefield1, (rulefieldleft as 
rulefield1left), (rulefieldright as rulefield1right))) :: rest671)) =>
 let val  result = MlyValue.rulefields (fn _ => let val  (rulefield
 as rulefield1) = rulefield1 ()
 in ([(rulefield, (rulefieldleft, rulefieldright))])
end)
 in ( LrTable.NT 10, ( result, rulefield1left, rulefield1right), 
rest671)
end
|  ( 22, ( ( _, ( MlyValue.rulefields rulefields1, _, rulefields1right
)) :: _ :: ( _, ( MlyValue.rulefield rulefield1, (rulefieldleft as 
rulefield1left), rulefieldright)) :: rest671)) => let val  result = 
MlyValue.rulefields (fn _ => let val  (rulefield as rulefield1) = 
rulefield1 ()
 val  (rulefields as rulefields1) = rulefields1 ()
 in (
(rulefield, (rulefieldleft, rulefieldright))
                               :: rulefields
)
end)
 in ( LrTable.NT 10, ( result, rulefield1left, rulefields1right), 
rest671)
end
|  ( 23, ( ( _, ( MlyValue.bg bg1, _, bg1right)) :: _ :: ( _, ( _, 
REDEX1left, _)) :: rest671)) => let val  result = MlyValue.rulefield
 (fn _ => let val  (bg as bg1) = bg1 ()
 in (REDEX bg)
end)
 in ( LrTable.NT 9, ( result, REDEX1left, bg1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.bg bg1, _, bg1right)) :: _ :: ( _, ( _, 
REACT1left, _)) :: rest671)) => let val  result = MlyValue.rulefield
 (fn _ => let val  (bg as bg1) = bg1 ()
 in (REACT bg)
end)
 in ( LrTable.NT 9, ( result, REACT1left, bg1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.inst inst1, _, inst1right)) :: _ :: ( _, (
 _, INST1left, _)) :: rest671)) => let val  result = 
MlyValue.rulefield (fn _ => let val  (inst as inst1) = inst1 ()
 in (INST inst)
end)
 in ( LrTable.NT 9, ( result, INST1left, inst1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.STRING STRING1, _, STRING1right)) :: _ :: (
 _, ( _, NAME1left, _)) :: rest671)) => let val  result = 
MlyValue.rulefield (fn _ => let val  (STRING as STRING1) = STRING1 ()
 in (RULENAME STRING)
end)
 in ( LrTable.NT 9, ( result, NAME1left, STRING1right), rest671)
end
|  ( 27, ( ( _, ( _, _, (RBRACKright as RBRACK1right))) :: ( _, ( 
MlyValue.maps maps1, _, _)) :: ( _, ( _, (LBRACKleft as LBRACK1left),
 _)) :: rest671)) => let val  result = MlyValue.inst (fn _ => let val 
 (maps as maps1) = maps1 ()
 in ((maps, (LBRACKleft, RBRACKright)))
end)
 in ( LrTable.NT 11, ( result, LBRACK1left, RBRACK1right), rest671)

end
|  ( 28, ( ( _, ( _, _, (RBRACKright as RBRACK1right))) :: ( _, ( _, (
LBRACKleft as LBRACK1left), _)) :: rest671)) => let val  result = 
MlyValue.inst (fn _ => (([], (LBRACKleft, RBRACKright))))
 in ( LrTable.NT 11, ( result, LBRACK1left, RBRACK1right), rest671)

end
|  ( 29, ( ( _, ( MlyValue.map map1, map1left, map1right)) :: rest671)
) => let val  result = MlyValue.maps (fn _ => let val  (map as map1) =
 map1 ()
 in ([map])
end)
 in ( LrTable.NT 13, ( result, map1left, map1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.maps maps1, _, maps1right)) :: _ :: ( _, ( 
MlyValue.map map1, map1left, _)) :: rest671)) => let val  result = 
MlyValue.maps (fn _ => let val  (map as map1) = map1 ()
 val  (maps as maps1) = maps1 ()
 in (map :: maps)
end)
 in ( LrTable.NT 13, ( result, map1left, maps1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.int int2, _, int2right)) :: _ :: ( _, ( 
MlyValue.int int1, int1left, _)) :: rest671)) => let val  result = 
MlyValue.map (fn _ => let val  int1 = int1 ()
 val  int2 = int2 ()
 in (((int1, []), (int2, [])))
end)
 in ( LrTable.NT 12, ( result, int1left, int2right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.nlist nlist2, _, nlist2right)) :: _ :: ( _,
 ( MlyValue.int int2, _, _)) :: _ :: ( _, ( MlyValue.nlist nlist1, _,
 _)) :: _ :: ( _, ( MlyValue.int int1, int1left, _)) :: rest671)) =>
 let val  result = MlyValue.map (fn _ => let val  int1 = int1 ()
 val  nlist1 = nlist1 ()
 val  int2 = int2 ()
 val  nlist2 = nlist2 ()
 in (((int1, nlist1), (int2, nlist2)))
end)
 in ( LrTable.NT 12, ( result, int1left, nlist2right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.altbg altbg1, altbg1left, altbg1right)) :: 
rest671)) => let val  result = MlyValue.bg (fn _ => let val  (altbg
 as altbg1) = altbg1 ()
 in (altbg)
end)
 in ( LrTable.NT 14, ( result, altbg1left, altbg1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.bg bg2, _, bg2right)) :: _ :: ( _, ( 
MlyValue.bg bg1, bg1left, _)) :: rest671)) => let val  result = 
MlyValue.bg (fn _ => let val  bg1 = bg1 ()
 val  bg2 = bg2 ()
 in (Ten ([bg1, bg2], mk_info bg1left bg2right))
end)
 in ( LrTable.NT 14, ( result, bg1left, bg2right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.bg bg2, _, bg2right)) :: _ :: ( _, ( 
MlyValue.bg bg1, bg1left, _)) :: rest671)) => let val  result = 
MlyValue.bg (fn _ => let val  bg1 = bg1 ()
 val  bg2 = bg2 ()
 in (Pri ([bg1, bg2], mk_info bg1left bg2right))
end)
 in ( LrTable.NT 14, ( result, bg1left, bg2right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.bg bg2, _, bg2right)) :: _ :: ( _, ( 
MlyValue.bg bg1, bg1left, _)) :: rest671)) => let val  result = 
MlyValue.bg (fn _ => let val  bg1 = bg1 ()
 val  bg2 = bg2 ()
 in (Par ([bg1, bg2], mk_info bg1left bg2right))
end)
 in ( LrTable.NT 14, ( result, bg1left, bg2right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.bg bg2, _, bg2right)) :: _ :: ( _, ( 
MlyValue.bg bg1, bg1left, _)) :: rest671)) => let val  result = 
MlyValue.bg (fn _ => let val  bg1 = bg1 ()
 val  bg2 = bg2 ()
 in (Com (bg1, bg2, mk_info bg1left bg2right))
end)
 in ( LrTable.NT 14, ( result, bg1left, bg2right), rest671)
end
|  ( 38, ( ( _, ( _, (ONEleft as ONE1left), (ONEright as ONE1right)))
 :: rest671)) => let val  result = MlyValue.altbg (fn _ => (
Mer (0, mk_info ONEright ONEleft)))
 in ( LrTable.NT 15, ( result, ONE1left, ONE1right), rest671)
end
|  ( 39, ( ( _, ( _, (IDX0left as IDX01left), (IDX0right as IDX01right
))) :: rest671)) => let val  result = MlyValue.altbg (fn _ => (
Ten ([], mk_info IDX0left IDX0right)))
 in ( LrTable.NT 15, ( result, IDX01left, IDX01right), rest671)
end
|  ( 40, ( ( _, ( _, (IDBB0left as IDBB01left), (IDBB0right as 
IDBB01right))) :: rest671)) => let val  result = MlyValue.altbg (fn _
 => (Par ([], mk_info IDBB0left IDBB0right)))
 in ( LrTable.NT 15, ( result, IDBB01left, IDBB01right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.pint pint1, _, (pintright as pint1right)))
 :: ( _, ( _, (MERGEleft as MERGE1left), _)) :: rest671)) => let val  
result = MlyValue.altbg (fn _ => let val  (pint as pint1) = pint1 ()
 in (Mer (pint, mk_info MERGEleft pintright))
end)
 in ( LrTable.NT 15, ( result, MERGE1left, pint1right), rest671)
end
|  ( 42, ( ( _, ( _, _, BQUOTE2right)) :: ( _, ( MlyValue.nset nset1,
 _, _)) :: ( _, ( _, BQUOTE1left, _)) :: rest671)) => let val  result
 = MlyValue.altbg (fn _ => let val  (nset as nset1) = nset1 ()
 in (Con (nset, mk_info BQUOTE1left BQUOTE2right))
end)
 in ( LrTable.NT 15, ( result, BQUOTE1left, BQUOTE2right), rest671)

end
|  ( 43, ( ( _, ( MlyValue.link link1, (linkleft as link1left), (
linkright as link1right))) :: rest671)) => let val  result = 
MlyValue.altbg (fn _ => let val  (link as link1) = link1 ()
 in (
Wir (Wiring.make 
				      (LinkSet.fromList link), 
				      mk_info linkleft linkright)
)
end)
 in ( LrTable.NT 15, ( result, link1left, link1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.nset nset1, _, nset1right)) :: ( _, ( _, (
IDWleft as IDW1left), IDWright)) :: rest671)) => let val  result = 
MlyValue.altbg (fn _ => let val  (nset as nset1) = nset1 ()
 in (
Wir (Wiring.make (nset2idlinkset nset),
				    mk_info IDWleft IDWright)
)
end)
 in ( LrTable.NT 15, ( result, IDW1left, nset1right), rest671)
end
|  ( 45, ( ( _, ( _, (IDW0left as IDW01left), (IDW0right as IDW01right
))) :: rest671)) => let val  result = MlyValue.altbg (fn _ => (
Wir (Wiring.id_0, mk_info IDW0left IDW0right)))
 in ( LrTable.NT 15, ( result, IDW01left, IDW01right), rest671)
end
|  ( 46, ( ( _, ( MlyValue.ports ports1, _, (portsright as ports1right
))) :: ( _, ( MlyValue.ctrlid ctrlid1, (ctrlidleft as ctrlid1left), _)
) :: rest671)) => let val  result = MlyValue.altbg (fn _ => let val  (
ctrlid as ctrlid1) = ctrlid1 ()
 val  (ports as ports1) = ports1 ()
 in (
case ports of
                                 ORDEREDPORTS (fp, bp) =>
                                 Ion (Ion.make 
                                          ({ctrl  = ctrlid, 
                                            free  = fp,
                                            bound = bp}),
                                          mk_info ctrlidleft portsright)
                               | NAMEDPORTS (fp, bp) =>
                                 let
                                   fun compare ((p1,_), (p2,_)) = String.compare (p1, p2)
                                   val K  = Control.name ctrlid
                                   val fp = ListSort.sort compare fp
                                   val bp = ListSort.sort compare bp
                                 in
                                   Ion (Ion.make {ctrl = Control.make'
                                                           (K, Control.Active,
                                                            map #1 bp,
                                                            map #1 fp),
                                                  free  = map #2 fp,
                                                  bound = map (fn (_, n) => NameSet.singleton n) bp},
                                        mk_info ctrlidleft portsright)
                                 end
)
end)
 in ( LrTable.NT 15, ( result, ctrlid1left, ports1right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.ctrlid ctrlid1, (ctrlidleft as ctrlid1left)
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
 in ( LrTable.NT 15, ( result, ctrlid1left, ctrlid1right), rest671)

end
|  ( 48, ( ( _, ( MlyValue.perm perm1, (permleft as perm1left), (
permright as perm1right))) :: rest671)) => let val  result = 
MlyValue.altbg (fn _ => let val  (perm as perm1) = perm1 ()
 in (Per (perm, mk_info permleft permright))
end)
 in ( LrTable.NT 15, ( result, perm1left, perm1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.pint pint1, _, (pintright as pint1right)))
 :: ( _, ( _, (IDPleft as IDP1left), _)) :: rest671)) => let val  
result = MlyValue.altbg (fn _ => let val  (pint as pint1) = pint1 ()
 in (Per (Permutation.id_n pint, mk_info IDPleft pintright))
end)
 in ( LrTable.NT 15, ( result, IDP1left, pint1right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.bg bg1, _, (bgright as bg1right))) :: _ :: 
( _, ( MlyValue.nset nset1, _, _)) :: ( _, ( _, (LTleft as LT1left), _
)) :: rest671)) => let val  result = MlyValue.altbg (fn _ => let val 
 (nset as nset1) = nset1 ()
 val  (bg as bg1) = bg1 ()
 in (Abs (nset, bg, mk_info LTleft bgright))
end)
 in ( LrTable.NT 15, ( result, LT1left, bg1right), rest671)
end
|  ( 51, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.bg bg1, _, _
)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.altbg (fn _ => let val  (bg as bg1) = bg1 ()
 in (bg)
end)
 in ( LrTable.NT 15, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 52, ( ( _, ( _, _, (GTGTright as GTGT1right))) :: ( _, ( 
MlyValue.bg bg1, _, _)) :: ( _, ( _, (LTLTleft as LTLT1left), _)) :: 
rest671)) => let val  result = MlyValue.altbg (fn _ => let val  (bg
 as bg1) = bg1 ()
 in (Hop (bg, mk_info LTLTleft GTGTright))
end)
 in ( LrTable.NT 15, ( result, LTLT1left, GTGT1right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.bports bports1, _, bports1right)) :: _ :: (
 _, ( _, LBRACK1left, _)) :: rest671)) => let val  result = 
MlyValue.ports (fn _ => let val  (bports as bports1) = bports1 ()
 in (ORDEREDPORTS ([], bports))
end)
 in ( LrTable.NT 21, ( result, LBRACK1left, bports1right), rest671)

end
|  ( 54, ( ( _, ( MlyValue.bports bports1, _, bports1right)) :: _ :: (
 _, ( MlyValue.name name1, _, _)) :: ( _, ( _, LBRACK1left, _)) :: 
rest671)) => let val  result = MlyValue.ports (fn _ => let val  (name
 as name1) = name1 ()
 val  (bports as bports1) = bports1 ()
 in (ORDEREDPORTS ([name], bports))
end)
 in ( LrTable.NT 21, ( result, LBRACK1left, bports1right), rest671)

end
|  ( 55, ( ( _, ( MlyValue.bports bports1, _, bports1right)) :: _ :: (
 _, ( MlyValue.names names1, _, _)) :: _ :: ( _, ( MlyValue.name name1
, _, _)) :: ( _, ( _, LBRACK1left, _)) :: rest671)) => let val  result
 = MlyValue.ports (fn _ => let val  (name as name1) = name1 ()
 val  (names as names1) = names1 ()
 val  (bports as bports1) = bports1 ()
 in (ORDEREDPORTS (name::names, bports))
end)
 in ( LrTable.NT 21, ( result, LBRACK1left, bports1right), rest671)

end
|  ( 56, ( ( _, ( MlyValue.portsassign portsassign1, _, 
portsassign1right)) :: _ :: ( _, ( MlyValue.name name1, _, _)) :: _ ::
 ( _, ( MlyValue.id id1, _, _)) :: ( _, ( _, LBRACK1left, _)) :: 
rest671)) => let val  result = MlyValue.ports (fn _ => let val  (id
 as id1) = id1 ()
 val  (name as name1) = name1 ()
 val  (portsassign as portsassign1) = portsassign1 ()
 in (NAMEDPORTS ([(id, name)], portsassign))
end)
 in ( LrTable.NT 21, ( result, LBRACK1left, portsassign1right), 
rest671)
end
|  ( 57, ( ( _, ( MlyValue.portsassign portsassign1, _, 
portsassign1right)) :: _ :: ( _, ( MlyValue.portassigns portassigns1,
 _, _)) :: _ :: ( _, ( MlyValue.name name1, _, _)) :: _ :: ( _, ( 
MlyValue.id id1, _, _)) :: ( _, ( _, LBRACK1left, _)) :: rest671)) =>
 let val  result = MlyValue.ports (fn _ => let val  (id as id1) = id1
 ()
 val  (name as name1) = name1 ()
 val  (portassigns as portassigns1) = portassigns1 ()
 val  (portsassign as portsassign1) = portsassign1 ()
 in (NAMEDPORTS ((id, name)::portassigns, portsassign))
end)
 in ( LrTable.NT 21, ( result, LBRACK1left, portsassign1right), 
rest671)
end
|  ( 58, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.portassigns 
portassigns1, _, _)) :: ( _, ( _, LBRACK1left, _)) :: rest671)) => let
 val  result = MlyValue.portsassign (fn _ => let val  (portassigns as 
portassigns1) = portassigns1 ()
 in (portassigns)
end)
 in ( LrTable.NT 24, ( result, LBRACK1left, RBRACK1right), rest671)

end
|  ( 59, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( _, LBRACK1left, _))
 :: rest671)) => let val  result = MlyValue.portsassign (fn _ => ([]))
 in ( LrTable.NT 24, ( result, LBRACK1left, RBRACK1right), rest671)

end
|  ( 60, ( ( _, ( MlyValue.portassigns portassigns1, _, 
portassigns1right)) :: _ :: ( _, ( MlyValue.name name1, _, _)) :: _ ::
 ( _, ( MlyValue.id id1, id1left, _)) :: rest671)) => let val  result
 = MlyValue.portassigns (fn _ => let val  (id as id1) = id1 ()
 val  (name as name1) = name1 ()
 val  (portassigns as portassigns1) = portassigns1 ()
 in ((id, name) :: portassigns)
end)
 in ( LrTable.NT 23, ( result, id1left, portassigns1right), rest671)

end
|  ( 61, ( ( _, ( MlyValue.name name1, _, name1right)) :: _ :: ( _, ( 
MlyValue.id id1, id1left, _)) :: rest671)) => let val  result = 
MlyValue.portassigns (fn _ => let val  (id as id1) = id1 ()
 val  (name as name1) = name1 ()
 in ([(id, name)])
end)
 in ( LrTable.NT 23, ( result, id1left, name1right), rest671)
end
|  ( 62, ( ( _, ( MlyValue.names names1, _, names1right)) :: _ :: ( _,
 ( MlyValue.name name1, name1left, _)) :: rest671)) => let val  result
 = MlyValue.names (fn _ => let val  (name as name1) = name1 ()
 val  (names as names1) = names1 ()
 in (name :: names)
end)
 in ( LrTable.NT 20, ( result, name1left, names1right), rest671)
end
|  ( 63, ( ( _, ( MlyValue.name name1, name1left, name1right)) :: 
rest671)) => let val  result = MlyValue.names (fn _ => let val  (name
 as name1) = name1 ()
 in ([name])
end)
 in ( LrTable.NT 20, ( result, name1left, name1right), rest671)
end
|  ( 64, ( ( _, ( MlyValue.id id1, id1left, id1right)) :: rest671)) =>
 let val  result = MlyValue.name (fn _ => let val  (id as id1) = id1
 ()
 in (Name.make id)
end)
 in ( LrTable.NT 19, ( result, id1left, id1right), rest671)
end
|  ( 65, ( ( _, ( MlyValue.strings strings1, _, strings1right)) :: _
 :: ( _, ( MlyValue.id id1, id1left, _)) :: rest671)) => let val  
result = MlyValue.strings (fn _ => let val  (id as id1) = id1 ()
 val  (strings as strings1) = strings1 ()
 in (id :: strings)
end)
 in ( LrTable.NT 17, ( result, id1left, strings1right), rest671)
end
|  ( 66, ( ( _, ( MlyValue.id id1, id1left, id1right)) :: rest671)) =>
 let val  result = MlyValue.strings (fn _ => let val  (id as id1) = 
id1 ()
 in ([id])
end)
 in ( LrTable.NT 17, ( result, id1left, id1right), rest671)
end
|  ( 67, ( rest671)) => let val  result = MlyValue.strings (fn _ => (
[]))
 in ( LrTable.NT 17, ( result, defaultPos, defaultPos), rest671)
end
|  ( 68, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.id (fn _ => let val  (ID as ID1) = ID1 ()
 in (ID)
end)
 in ( LrTable.NT 32, ( result, ID1left, ID1right), rest671)
end
|  ( 69, ( ( _, ( _, IDX01left, IDX01right)) :: rest671)) => let val  
result = MlyValue.id (fn _ => ("idx0"))
 in ( LrTable.NT 32, ( result, IDX01left, IDX01right), rest671)
end
|  ( 70, ( ( _, ( _, IDW1left, IDW1right)) :: rest671)) => let val  
result = MlyValue.id (fn _ => ("idw"))
 in ( LrTable.NT 32, ( result, IDW1left, IDW1right), rest671)
end
|  ( 71, ( ( _, ( _, IDW01left, IDW01right)) :: rest671)) => let val  
result = MlyValue.id (fn _ => ("idw0"))
 in ( LrTable.NT 32, ( result, IDW01left, IDW01right), rest671)
end
|  ( 72, ( ( _, ( _, MERGE1left, MERGE1right)) :: rest671)) => let
 val  result = MlyValue.id (fn _ => ("merge"))
 in ( LrTable.NT 32, ( result, MERGE1left, MERGE1right), rest671)
end
|  ( 73, ( ( _, ( _, IDP1left, IDP1right)) :: rest671)) => let val  
result = MlyValue.id (fn _ => ("idp"))
 in ( LrTable.NT 32, ( result, IDP1left, IDP1right), rest671)
end
|  ( 74, ( ( _, ( _, NAME1left, NAME1right)) :: rest671)) => let val  
result = MlyValue.id (fn _ => ("name"))
 in ( LrTable.NT 32, ( result, NAME1left, NAME1right), rest671)
end
|  ( 75, ( ( _, ( _, REDEX1left, REDEX1right)) :: rest671)) => let
 val  result = MlyValue.id (fn _ => ("redex"))
 in ( LrTable.NT 32, ( result, REDEX1left, REDEX1right), rest671)
end
|  ( 76, ( ( _, ( _, REACT1left, REACT1right)) :: rest671)) => let
 val  result = MlyValue.id (fn _ => ("react"))
 in ( LrTable.NT 32, ( result, REACT1left, REACT1right), rest671)
end
|  ( 77, ( ( _, ( _, INST1left, INST1right)) :: rest671)) => let val  
result = MlyValue.id (fn _ => ("inst"))
 in ( LrTable.NT 32, ( result, INST1left, INST1right), rest671)
end
|  ( 78, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.nsetlist 
nsetlist1, _, _)) :: ( _, ( _, LBRACK1left, _)) :: rest671)) => let
 val  result = MlyValue.bports (fn _ => let val  (nsetlist as 
nsetlist1) = nsetlist1 ()
 in (nsetlist)
end)
 in ( LrTable.NT 22, ( result, LBRACK1left, RBRACK1right), rest671)

end
|  ( 79, ( rest671)) => let val  result = MlyValue.bports (fn _ => ([]
))
 in ( LrTable.NT 22, ( result, defaultPos, defaultPos), rest671)
end
|  ( 80, ( ( _, ( MlyValue.nsetlist nsetlist1, _, nsetlist1right)) ::
 _ :: ( _, ( MlyValue.nset nset1, nset1left, _)) :: rest671)) => let
 val  result = MlyValue.nsetlist (fn _ => let val  (nset as nset1) = 
nset1 ()
 val  (nsetlist as nsetlist1) = nsetlist1 ()
 in (nset :: nsetlist)
end)
 in ( LrTable.NT 25, ( result, nset1left, nsetlist1right), rest671)

end
|  ( 81, ( ( _, ( MlyValue.nset nset1, nset1left, nset1right)) :: 
rest671)) => let val  result = MlyValue.nsetlist (fn _ => let val  (
nset as nset1) = nset1 ()
 in ([nset])
end)
 in ( LrTable.NT 25, ( result, nset1left, nset1right), rest671)
end
|  ( 82, ( rest671)) => let val  result = MlyValue.nsetlist (fn _ => (
[]))
 in ( LrTable.NT 25, ( result, defaultPos, defaultPos), rest671)
end
|  ( 83, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( _, LBRACK1left, _))
 :: rest671)) => let val  result = MlyValue.nlist (fn _ => ([]))
 in ( LrTable.NT 18, ( result, LBRACK1left, RBRACK1right), rest671)

end
|  ( 84, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.names names1
, _, _)) :: ( _, ( _, LBRACK1left, _)) :: rest671)) => let val  result
 = MlyValue.nlist (fn _ => let val  (names as names1) = names1 ()
 in (names)
end)
 in ( LrTable.NT 18, ( result, LBRACK1left, RBRACK1right), rest671)

end
|  ( 85, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.strings 
strings1, _, _)) :: ( _, ( _, LBRACK1left, _)) :: rest671)) => let
 val  result = MlyValue.slist (fn _ => let val  (strings as strings1)
 = strings1 ()
 in (strings)
end)
 in ( LrTable.NT 16, ( result, LBRACK1left, RBRACK1right), rest671)

end
|  ( 86, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( _, LBRACK1left, _))
 :: rest671)) => let val  result = MlyValue.nset (fn _ => (
NameSet.empty))
 in ( LrTable.NT 26, ( result, LBRACK1left, RBRACK1right), rest671)

end
|  ( 87, ( ( _, ( _, _, (RBRACKright as RBRACK1right))) :: ( _, ( 
MlyValue.names names1, _, _)) :: ( _, ( _, (LBRACKleft as LBRACK1left)
, _)) :: rest671)) => let val  result = MlyValue.nset (fn _ => let
 val  (names as names1) = names1 ()
 in (
NameSet.fromList (names)
			       handle NameSet.DuplicatesRemoved
				      => raise 
					DuplicateNames 
					  ("rules.grm", (LBRACKleft, RBRACKright),
					   "while parsing")
)
end)
 in ( LrTable.NT 26, ( result, LBRACK1left, RBRACK1right), rest671)

end
|  ( 88, ( ( _, ( MlyValue.CTRLID CTRLID1, CTRLID1left, CTRLID1right))
 :: rest671)) => let val  result = MlyValue.ctrlid (fn _ => let val  (
CTRLID as CTRLID1) = CTRLID1 ()
 in (Control.make (CTRLID, Control.Active, 0, 0))
end)
 in ( LrTable.NT 27, ( result, CTRLID1left, CTRLID1right), rest671)

end
|  ( 89, ( ( _, ( MlyValue.nset nset1, _, nset1right)) :: ( _, ( _, 
DASHSLASHSLASH1left, _)) :: rest671)) => let val  result = 
MlyValue.link (fn _ => let val  (nset as nset1) = nset1 ()
 in (
map (fn name => 
                                              Link.make {outer = NONE, inner = NameSet.singleton name})
                                             (NameSet.list nset)
)
end)
 in ( LrTable.NT 28, ( result, DASHSLASHSLASH1left, nset1right), 
rest671)
end
|  ( 90, ( ( _, ( MlyValue.id id2, _, id2right)) :: _ :: ( _, ( 
MlyValue.id id1, id1left, _)) :: rest671)) => let val  result = 
MlyValue.link (fn _ => let val  id1 = id1 ()
 val  id2 = id2 ()
 in (
[Link.make {outer = SOME (Name.make id1), 
					 	     inner = NameSet.singleton (Name.make id2)}]
)
end)
 in ( LrTable.NT 28, ( result, id1left, id2right), rest671)
end
|  ( 91, ( ( _, ( MlyValue.id id1, _, id1right)) :: ( _, ( _, 
DASHSLASH1left, _)) :: rest671)) => let val  result = MlyValue.link
 (fn _ => let val  (id as id1) = id1 ()
 in (
[Link.make {outer = NONE, 
						     inner = NameSet.singleton (Name.make id)}]
)
end)
 in ( LrTable.NT 28, ( result, DASHSLASH1left, id1right), rest671)
end
|  ( 92, ( ( _, ( MlyValue.nset nset1, _, nset1right)) :: _ :: ( _, ( 
MlyValue.id id1, id1left, _)) :: rest671)) => let val  result = 
MlyValue.link (fn _ => let val  (id as id1) = id1 ()
 val  (nset as nset1) = nset1 ()
 in ([Link.make {outer = SOME (Name.make id), inner = nset}])
end)
 in ( LrTable.NT 28, ( result, id1left, nset1right), rest671)
end
|  ( 93, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( 
MlyValue.permentrylist permentrylist1, _, _)) :: _ :: ( _, ( _, 
AT1left, _)) :: rest671)) => let val  result = MlyValue.perm (fn _ =>
 let val  (permentrylist as permentrylist1) = permentrylist1 ()
 in (Permutation.make permentrylist)
end)
 in ( LrTable.NT 29, ( result, AT1left, RBRACK1right), rest671)
end
|  ( 94, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( 
MlyValue.permentrylist permentrylist1, _, _)) :: _ :: ( _, ( _, 
ATAT1left, _)) :: rest671)) => let val  result = MlyValue.perm (fn _
 => let val  (permentrylist as permentrylist1) = permentrylist1 ()
 in (Permutation.make permentrylist)
end)
 in ( LrTable.NT 29, ( result, ATAT1left, RBRACK1right), rest671)
end
|  ( 95, ( ( _, ( MlyValue.permentrylist permentrylist1, _, 
permentrylist1right)) :: _ :: ( _, ( MlyValue.permentry permentry1, 
permentry1left, _)) :: rest671)) => let val  result = 
MlyValue.permentrylist (fn _ => let val  (permentry as permentry1) = 
permentry1 ()
 val  (permentrylist as permentrylist1) = permentrylist1 ()
 in (permentry :: permentrylist)
end)
 in ( LrTable.NT 30, ( result, permentry1left, permentrylist1right), 
rest671)
end
|  ( 96, ( ( _, ( MlyValue.permentry permentry1, permentry1left, 
permentry1right)) :: rest671)) => let val  result = 
MlyValue.permentrylist (fn _ => let val  (permentry as permentry1) = 
permentry1 ()
 in ([permentry])
end)
 in ( LrTable.NT 30, ( result, permentry1left, permentry1right), 
rest671)
end
|  ( 97, ( rest671)) => let val  result = MlyValue.permentrylist (fn _
 => ([]))
 in ( LrTable.NT 30, ( result, defaultPos, defaultPos), rest671)
end
|  ( 98, ( ( _, ( MlyValue.nset nset1, _, nset1right)) :: _ :: ( _, ( 
MlyValue.int int1, int1left, _)) :: rest671)) => let val  result = 
MlyValue.permentry (fn _ => let val  (int as int1) = int1 ()
 val  (nset as nset1) = nset1 ()
 in ((int, nset))
end)
 in ( LrTable.NT 31, ( result, int1left, nset1right), rest671)
end
|  ( 99, ( ( _, ( MlyValue.int int1, int1left, int1right)) :: rest671)
) => let val  result = MlyValue.permentry (fn _ => let val  (int as 
int1) = int1 ()
 in ((int, NameSet.empty))
end)
 in ( LrTable.NT 31, ( result, int1left, int1right), rest671)
end
|  ( 100, ( ( _, ( MlyValue.int int1, int1left, int1right)) :: rest671
)) => let val  result = MlyValue.pint (fn _ => let val  (int as int1)
 = int1 ()
 in (int)
end)
 in ( LrTable.NT 33, ( result, int1left, int1right), rest671)
end
|  ( 101, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.pint pint1,
 _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.pint (fn _ => let val  (pint as pint1) = pint1 ()
 in (pint)
end)
 in ( LrTable.NT 33, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 102, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671
)) => let val  result = MlyValue.int (fn _ => let val  (INT as INT1) =
 INT1 ()
 in (INT)
end)
 in ( LrTable.NT 34, ( result, INT1left, INT1right), rest671)
end
|  ( 103, ( ( _, ( _, ONE1left, ONE1right)) :: rest671)) => let val  
result = MlyValue.int (fn _ => (1))
 in ( LrTable.NT 34, ( result, ONE1left, ONE1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Rules_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun BGTERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun RULELIST (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun SIGNATURE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun CTRLID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.CTRLID (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun ACTIVE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun PASSIVE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun ATOMIC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun AMP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun MAPSTO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun MAPPSTO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun DASHCOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun EQCOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun DASHDASHCOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun EQEQCOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun RARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun RRARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun EQEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun NAME (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun REDEX (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun REACT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun INST (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun MERGE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun SLASH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun SLASHSLASH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun DASHSLASH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun DASHSLASHSLASH (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun OO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun XX (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun PRI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun PAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun BQUOTE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun LTLT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
fun GTGT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.VOID,p1,p2))
fun AT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(
ParserData.MlyValue.VOID,p1,p2))
fun ATAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 48,(
ParserData.MlyValue.VOID,p1,p2))
fun IDX0 (p1,p2) = Token.TOKEN (ParserData.LrTable.T 49,(
ParserData.MlyValue.VOID,p1,p2))
fun IDBB0 (p1,p2) = Token.TOKEN (ParserData.LrTable.T 50,(
ParserData.MlyValue.VOID,p1,p2))
fun ONE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 51,(
ParserData.MlyValue.VOID,p1,p2))
fun IDW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 52,(
ParserData.MlyValue.VOID,p1,p2))
fun IDW0 (p1,p2) = Token.TOKEN (ParserData.LrTable.T 53,(
ParserData.MlyValue.VOID,p1,p2))
fun IDP (p1,p2) = Token.TOKEN (ParserData.LrTable.T 54,(
ParserData.MlyValue.VOID,p1,p2))
end
end
