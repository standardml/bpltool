(***************************************************************************
 Ebbe Elsborg, 5/7-2007

 Compile: Do 'make lambda-pure' in 'impl/bpl/src'
 Run: Do './lambda-pure' in 'impl/bpl/src'

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
  app : active(0) , derived (ordered children '1' and '2')
  var : atomic(1)
  def : active(1)

  [x]_X\u{x}  = var_x \t X
! [\x.M]_X    = (/x \t id_1 \t id_X)(lam_x | id_X\u{x})[M]_X\u{x}
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
  A    (app \ id_x)(lam_x || id_1) ==> id_1 || def_x  {0->0,1->1} (Id_Ø,Id_Ø)
  C    var_x || def_x              ==> id_1 || def_x  {0->0,1->0} (Id_Ø,Id_Ø)
  D    (/x \t id_1) def_x          ==> 1              {}          ()

****************************************************************************)

structure BG = BG (structure ErrorHandler = PrintErrorHandler);
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

val info = BG.Info.noinfo
val id_1 = B.Per info (P.id_n 1)
fun s2n s = Name.make s
fun v2n x = Name.make (String.toString x)
fun ion2bg ion = B.Ion info ion

(* SIGNATURE *)
fun lam x = ion2bg (Ion.make {ctrl = C.make("lam", C.Active),
			      free = [x], bound = []})
fun var x = ion2bg (Ion.make {ctrl = C.make("var", C.Atomic),
			      free = [x], bound = []})
fun def x = ion2bg (Ion.make {ctrl = C.make("def", C.Active),
			      free = [x], bound = []})

val appl = ion2bg (Ion.make {ctrl = C.make("appl", C.Active),
			     free = [], bound = []})
val appr = ion2bg (Ion.make {ctrl = C.make("appr", C.Active),
			     free = [], bound = []})
val app = let val a = ion2bg (Ion.make {ctrl = C.make("app", C.Active),
					free = [], bound = []})
	  in S.o (a, (S.`|` (appl, appr))) end

(* example: (\x.xx) k , k is a constant *)
(*
val var_x = var(s2n "x")
val par_xx = S.|| (var_x, var_x)
val id_x = S.idw ["x"]
val app' = S.* (app, id_x)
val lam_x = lam(s2n "x")
val lam_x' = S.`|` (lam_x, id_x)
val lam_xx = S.o (lam_x', S.o (app', par_xx)) (*not composable!*)
*)
val var_y = var(s2n "y")
val par_yy = S.|| (var_y, var_y)
val id_y = S.idw ["y"]
val app' = S.* (app, id_y)
val lam_x = lam(s2n "x")
val lam_x' = S.* (lam_x, id_y)
val x_slash_xy = S.* (S.// ("x", ["x","y"]), id_1)
val close_x = S.* (S./ ("", "x"), id_1)
val join_xy = S.o (close_x, x_slash_xy)
val lam_xx = S.o (join_xy, S.o (lam_x', S.o (app', par_yy)))
val k = S.atomic0 "k"

val term  = S.o (app, S.* (lam_xx, k))

(* aux. functions *)
fun prtSimp name =
 fn bgval => print(name ^ "= " ^ B.toString(B.simplify bgval) ^ "\n")

fun makeBR bgval = Bdnf.regularize (Bdnf.make bgval)

val _ = prtSimp "(Lx.x x) k " term

(* RULES *)
val var_x = var(s2n "x")
val def_x = def(s2n "x")
val barren = S.<->
val id_x = S.idw ["x"]
val set_x = NameSet.insert (s2n "x") NameSet.empty
val iface_0x = Iface.* (Iface.zero, Iface.X set_x)
val instC = Inst.make { I = Iface.m 2 ,
			J = Iface.* (Iface.m 2, iface_0x) ,
			maps = [((0,[]), (0,[])), ((1,[]), (0,[]))] }
val redexA = S.o (S.* (app, id_x), S.|| (lam_x, id_1))
val reactA = S.`|` (id_1, def_x)
val ruleA = R.make' { name = "A" , redex = makeBR redexA , react = reactA }

val redexC = S.|| (var_x, def_x)
val reactC = S.|| (id_1, def_x)
val ruleC = R.make { name = "C" , redex = makeBR redexC , react = reactC,
		     inst = instC }

val redexD = S.o (S.* (S./ ("", "x"), id_1), def_x)
val reactD = barren
val ruleD = R.make' { name = "D" , redex = makeBR redexD , react = reactD }

(* MATCHES *)
(* example:
   (\x.x x) k --1> (x x)<x:=k> --2> (k x)<x:=k> --2> (k k)<x:=k> --3> k k
*)

fun printMts m =
    ( print "Matches:\n"
    ; LazyList.lzprint M.toString m
    ; print "\n" )

val mts = M.matches { agent = makeBR term, rule = ruleA }

val term_parts =
    let val term' = M.unmk (LazyList.lzhd mts)
	val term'_ctx = #context(term')
	val term'_par = #parameter(term')
	fun peel x = (B.toString o B.simplify o Bdnf.unmk) x
    in ["term_ctx= " ^ (peel term'_ctx) ^ "\n",
	"term_par= " ^ (peel term'_par) ^ "\n"]
    end

val _ = printMts mts
val _ = map print term_parts

val terms = LazyList.lzmap (Re.react (makeBR term)) mts
val _ = print "Agents resulting from reactions:\n"
val _ = LazyList.lzprint (B.toString o B.simplify o Bdnf.unmk) terms
val _ = print "\n"

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
