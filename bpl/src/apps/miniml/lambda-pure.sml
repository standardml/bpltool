(***************************************************************************
 Ebbe Elsborg, 5/7-2007
 ---------------------------------------------------------------------------
 Compile: Do 'make lambda-pure' in 'impl/bpl/src'
 Run: Do './lambda-pure' in 'impl/bpl/src'
 ---------------------------------------------------------------------------
 Encoding of lambda calculus with explicit substitution "at a distance"
 in PURE bigraphs.

 Source language:
  M,N ::= x | \x.M | M N | M<x:=N>

  1    (\x.M) N       --> M<x:=N>
  2    ({x/y}M)<x:=N> --> ({N/y}M)<x:=N> , if y in fv(M) and unique
  3    M<x:=N>        --> M              , if x not in fv(M)

  All contexts are active.

 Target language:
  - Signature
  - Translation of lambda terms into bigraphs
  - Reaction rules

  lam : active(1)
  app : active(0) , derived (ordered children 'appl' and 'appr')
  var : atomic(1)
  def : active(1)

  [x]_X\u{x}  = var_x \t X
  [\x.M]_X    = (/x \t id_1 \t id_X)(lam_x || id_X\u{x})[M]_X\u{x}
  [M N]_X     = (app \t id_X)([M]_X || [N]_X)
  [M<x:=N>]_X = (/x \t id_1 \t id_X)([M]_X\u{x} | (def_x \t id_X)[N]_X)

  where
  - for [M]_X we require fv(M) subset of X,
  - \u is disjoint set union,
  - \t is tensor product,
  - | is prime product, and
  - || is parallel product.

  Name Redex                           Reactum        Eta         Iota
  ---------------------------------------------------------------------------
  A    (app \ id_x)(lam_x || id_1) ==> id_1 | def_x   {0->0,1->1} (Id_Ø,Id_Ø)
  C    var_x || def_x              ==> id_1 || def_x  {0->0,1->0} (Id_Ø,Id_Ø)
  D    (/x \t id_1) def_x          ==> 1              {}          ()

****************************************************************************)

structure BG = BG (structure ErrorHandler = PrintErrorHandler)
structure B = BG.BgVal
structure S = BG.Sugar
structure P = BG.Permutation
structure R = BG.Rule
structure Bdnf = BG.BgBDNF
structure M = BG.Match
structure C = BG.Control
structure Name = BG.Name
structure NameSet = BG.NameSet
structure Ion = BG.Ion
structure Wiring = BG.Wiring
structure Link = BG.Link
structure Inst = BG.Instantiation
structure Iface = BG.Interface
structure Re = Reaction (structure RuleNameMap = Util.StringMap
                         structure Info = BG.Info
                         structure Interface = BG.Interface
			 structure Wiring = BG.Wiring
			 structure BgVal = BG.BgVal
			 structure BgBDNF = BG.BgBDNF
			 structure Match = BG.Match
			 structure Instantiation = BG.Instantiation
			 structure Rule = BG.Rule
			 structure Origin = Origin
			 structure ErrorHandler = PrintErrorHandler)

(* useful constants *)
val info = BG.Info.noinfo
val barren = S.<->
val id_1 = B.Per info (P.id_n 1)

(* shorthand functions *)
fun s2n s = Name.make s
fun n2s n = Name.unmk n
fun v2n x = Name.make (String.toString x)
fun ion2bg ion = B.Ion info ion

(* interface functions *)
fun getInner b = let val (b,inn,out) = B.unmk b in inn end
fun getOuter b = let val (b,inn,out) = B.unmk b in out end
fun isGround b = let val (bgterm,inner,outer) = B.unmk b
		 in Iface.eq (inner, Iface.zero) end

(* printing *)
fun printWidth w = print(Int.toString w)
fun printName n = print(", " ^ (n2s n))
fun printNameset set =
    let fun loop set strAcc flag =
	    if NameSet.size set = 0 then strAcc
	    else let val member = NameSet.someElement set
		     val set' = NameSet.remove member set
		 in if flag (* at first member *)
		    then loop set' (strAcc ^ (n2s member)) false
		    else loop set' (strAcc ^ "," ^ (n2s member)) false
		 end
    in print ("{" ^ (loop set "" true) ^ "}") end

fun printLoc l =
    let fun loop list flag =
	    case list
	     of [] => print ""
	      | (x::xs) => if flag then ( printNameset x ; loop xs false )
			   else ( print "," ; printNameset x ; loop xs false )
    in ( print "(" ; loop l true ; print ")") end

fun printGlob s = printNameset s

fun printIface i = 
    let val i_parts = Iface.unmk i
	val w = #width(i_parts)
	val l = #loc(i_parts)
	val g = #glob(i_parts)
    in ( print "<"
       ; printWidth w ; print ", " ; printLoc l ; print ", " ; printGlob g
       ; print ">\n") end

fun printIfaces t i j =
    ( print(t ^ " : ")
    ; printIface i
    ; print " -> "
    ; printIface j
    ; print "\n")

fun prtSimp name bgval =
    print(name ^ "= " ^ B.toString(B.simplify bgval) ^ "\n")

fun printMts m =
    ( print "Matches:\n"
    ; LazyList.lzprint M.toString m
    ; print "\n" )

fun printRes tname agents =
    ( print("\nAgent(s) resulting from reaction(s) on " ^ tname ^ ":\n")
    ; LazyList.lzprint (B.toString o B.simplify) agents
    ; print "\n" )

(* SIGNATURE *)
fun lam x = ion2bg (Ion.make {ctrl = C.make("lam", C.Active, 1, 0),
			      free = [x], bound = []})
fun var x = S.o (ion2bg (Ion.make {ctrl = C.make("var", C.Atomic, 1, 0),
				   free = [x], bound = []}),
		 barren)
fun def x = ion2bg (Ion.make {ctrl = C.make("def", C.Active, 1, 0),
			      free = [x], bound = []})

val appl = ion2bg (Ion.make {ctrl = C.make("appl", C.Active, 0, 0),
			     free = [], bound = []})
val appr = ion2bg (Ion.make {ctrl = C.make("appr", C.Active, 0, 0),
			     free = [], bound = []})
val app = let val a = ion2bg (Ion.make {ctrl = C.make("app", C.Active, 0, 0),
					free = [], bound = []})
	  in S.o (a, (S.`|` (appl, appr))) end

(* example: (\x.xx) k , k is a constant *)
val var_x = var(s2n "x")
val par_xx = S.|| (var_x, var_x)
val id_x = S.idw ["x"]
val app' = S.* (app, id_x)
val lam_x = lam(s2n "x")
val lam_x' = S.|| (lam_x, id_x)
val lam_xx = S.o (S.* (S.-/ "x", id_1), S.o (lam_x', S.o (app', par_xx)))
val k = S.atomic0 "k"
val term = S.o (app, S.* (lam_xx, k))

val _ = prtSimp "(Lx.x x) k " term
val _ = printIfaces "term" (getInner term) (getOuter term)

(* aux. function *)
fun makeBR bgval = Bdnf.regularize (Bdnf.make bgval)

(* RULES *)
val def_x = def(s2n "x")
val set_x = NameSet.insert (s2n "x") NameSet.empty
val redex_innerface_C = Iface.m 1
val react_innerface_C = Iface.m 2
val instC = Inst.make { I = redex_innerface_C ,
			J = react_innerface_C ,
			maps = [((0,[]), (0,[])), ((1,[]), (0,[]))] }

val redexA = S.o (S.* (app, id_x), S.|| (lam_x, id_1))
val reactA = S.`|` (id_1, def_x)
val ruleA = R.make' { name = "A" , redex = makeBR redexA , react = reactA }

val redexC = S.|| (var_x, def_x)
val reactC = S.|| (id_1, def_x)
val ruleC = R.make { name = "C" , redex = makeBR redexC , react = reactC,
		     inst = instC }

val redexD = S.o (S.* (S.-/ "x", id_1), def_x)
val reactD = barren
val ruleD = R.make' { name = "D" , redex = makeBR redexD , react = reactD }

(* REACTIONS *)
(* example:
   (\x.x x) k --1> (x x)<x:=k> --2> (k x)<x:=k> --2> (k k)<x:=k> --3> k k
*)

fun parts agent matches =
    let val agent' = M.unmk (LazyList.lzhd matches)
	val agent'_ctx = #context(agent')
	val agent'_par = #parameter(agent')
	fun peel x = (B.toString o B.simplify o Bdnf.unmk) x
    in ["agent_ctx= " ^ (peel agent'_ctx) ^ "\n",
	"agent_par= " ^ (peel agent'_par) ^ "\n"] end

fun lzlength l = Int.toString(List.length(LazyList.lztolist l))

fun handler exn = print (BaseErrorHandler.explain' exn)

(* (lx.x x) --1> (x x)<x:=k> *)
val mtsA = M.matches { agent = makeBR term , rule = ruleA }
(*
val _ = printMts mtsA
val _ = map print (parts term mtsA)
*)
val terms' = LazyList.lzmap (Re.react term) mtsA
val _ = printRes "term" terms'

(* (x x)<x:=k> --2> (k x)<x:=k> *)
val _ = print("\n")
val term' = LazyList.lzhd terms' (* the resulting agent we want *)
val mtsC = M.matches { agent = makeBR term' , rule = ruleC }
(*
val _ = printMts mtsC
val _ = print("length(mtsC) = " ^ (lzlength mtsC) ^ "\n")
*)
val terms'' = LazyList.lzmap (Re.react term') mtsC
(*
val _ = print("length(terms'') = " ^ (lzlength terms'') ^ "\n")
*)
val _ = printRes "term'" terms''

(* (k x)<x:=k> --2> (k k)<x:=k> *)
val term'' = LazyList.lzhd terms'' (* the resulting agent we want *)
(* print all the matches
val mtsC2 = LazyList.lzmap
		(fn t => M.matches { agent = t , rule = ruleC }) terms''
*)
val mtC2 = M.matches { agent = makeBR term'' , rule = ruleC }
(*
val _ = print("length(mtC2) = " ^ (lzlength mtC2) ^ "\n")
val _ = printMts mtC2
*)
val terms''' = LazyList.lzmap (Re.react term'') mtC2
(*
val _ = print("length(terms''') = " ^ (lzlength terms''') ^ "\n")
*)
val _ = printRes "term''" terms'''

(* (k k)<x:=k> --3> k k *)
val term''' = LazyList.lzhd terms''' (* the resulting agent we want *)
(*val _ = print("term''' =\n" ^ B.toString(term''') ^ "\n")*)
val mtsD = M.matches { agent = makeBR term''' , rule = ruleD }
(*val _ = printMts mtsD*)
val terms'''' = LazyList.lzmap (Re.react term''') mtsD
(*
val _ = case mtD of NONE => print "No matches!\n"
		  | SOME(m) => ( print(M.toString(m))
		    handle e => handler e )
*)
val _ = printRes "term'''" terms''''


(*
open TextIO;

val os = openOut("matches.out")

fun printmatches [] = "Done\n"
  | printmatches (x::xs) = ( output(os, (M.toString x) ^ "\n")
			   ; printmatches xs )

val out_a = printmatches all_a
val out_b = printmatches all_b

val _ = flushOut(os)
val _ = closeOut(os)
*)
