(* README: Do 'make lambda-pure' from impl/bpl/src ! *)

(*open TextIO;*)

structure BG = BG (structure ErrorHandler = PrintErrorHandler);
structure B = BG.BgVal
structure S = BG.Sugar
structure P = BG.Permutation
structure R = BG.Rule
structure Bdnf = BG.BgBDNF
structure M = BG.Match
structure C = BG.Control
structure Name = BG.Name
structure Ion = BG.Ion
structure Wiring = BG.Wiring
structure Link = BG.Link
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
fun s2n s = Name.make s
fun v2n x = Name.make (String.toString x)
fun ion2bg ion = B.Ion info ion

(* signature *)
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
val var_x = var (s2n "x")
val par_xx = S.|| (var_x, var_x)
val id_x = S.idw [s2n "x"]
    (*B.Wir info (Wiring.make' [Link.make {outer = SOME(s2n "x"),
					   inner = Nameset.Set.empty}])*)
val app' = S.* (app, id_x)
val lam_xx = S.o (lam x, S.o (app', par_xx))
val k = S.atomic0 "k"
val term  = S.o (app', S.* lam_xx k)

(*here
val id_1 = B.Per info (P.id_n 1)

val a = B.Ten info [K, L]
val b = B.Ten info [L, K]
val C = B.Pri info [M, id_1]

fun prtSimp name =
 fn bgval => print(name ^ "= " ^ B.toString(B.simplify bgval) ^ "\n")

val _ = prtSimp "a" a
val _ = prtSimp "b" b
val _ = prtSimp "C" C

fun makeBR bgval = Bdnf.regularize (Bdnf.make bgval)

val K' = makeBR K

val r = R.make' { name = "K-to-L" , redex = K' , react = L }

val Coa = makeBR(B.Com info (C,a))
val Cob = makeBR(B.Com info (C,b))

val mt_a = M.matches { agent = Coa, rule = r }
val mt_b = M.matches { agent = Cob, rule = r }

fun printMts m =
    ( print "Matches:\n"
    ; LazyList.lzprint M.toString m
    ; print "\n" )

val b_parts =
    let val b' = M.unmk (LazyList.lzhd mt_b)
	val b'_ctx = #context(b')
	val b'_par = #parameter(b')
	fun peel x = (B.toString o B.simplify o Bdnf.unmk) x
    in ["b_ctx= " ^ (peel b'_ctx) ^ "\n",
	"b_par= " ^ (peel b'_par) ^ "\n"]
    end

val _ = printMts mt_a
(*val _ = printMts mt_b*)
val _ = map print b_parts

val b's = LazyList.lzmap (Re.react Cob) mt_b
val _ = print "Agents resulting from reactions:\n"
val _ = LazyList.lzprint (B.toString o B.simplify o Bdnf.unmk) b's
val _ = print "\n"
*)

(*
val os = openOut("matches.out")

fun printmatches [] = "Done\n"
  | printmatches (x::xs) = ( output(os, (M.toString x) ^ "\n")
			   ; printmatches xs )

val out_a = printmatches all_a
val out_b = printmatches all_b

val _ = flushOut(os)
val _ = closeOut(os)
*)
