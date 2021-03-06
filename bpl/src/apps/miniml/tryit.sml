open TextIO; (* for writing output *)

structure BG = BG (structure ErrorHandler = PrintErrorHandler);
structure B = BG.BgVal
structure S = BG.Sugar
structure P = BG.Permutation
structure R = BG.Rule
structure Bdnf = BG.BgBDNF
structure M = BG.Match
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
			 structure ErrorHandler = BG.ErrorHandler)

val info = BG.Info.noinfo

val K = S.atomic0 "K"
val L = S.atomic0 "L"
val M = S.passive0 "M"

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

val r = R.make' { name = "K-to-L" , redex = K' , react = L, info = info }

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

val b's = LazyList.lzmap (Re.react (*Cob*)) mt_b
val _ = print "Agents resulting from reactions:\n"
val _ = LazyList.lzprint (B.toString o B.simplify o Bdnf.unmk o Bdnf.make) b's
val _ = print "\n"


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
