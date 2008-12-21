(***************************************************************************
 Ebbe Elsborg, 18/12/2008
 ---------------------------------------------------------------------------
 Compile: Do 'make abs-locmod-sim' in 'impl/bpl/src'
 Run: Do './abs-locmod-sim' in 'impl/bpl/src'
 Using the dependencies in impl/bpl/src/apps/miniml/Dependencies
 ---------------------------------------------------------------------------
 An abstract Plato-graphical location model.
 Simulation of a simple predefined scenario.

 C: Real world

 signature world =
   sig % arity is: (binding -> free)
     loc : passive (0 -> 1)
     dev : atomic  (0 -> 1)
   end

 using world

 rule moveup =
   loc_l([0] | loc_l'([1] | dev_d))
     ->
   loc_l([0] | loc_l'([1]) | dev_d)

 rule movedown =
   loc_l([0] | loc_l'([1]) | dev_d)
     ->
   loc_l([0] | loc_l'([1] | dev_d)) 

 state = loc_l(loc_l' | dev_d ...)

 --------------------------------------------------------

 S: Sensor system

  signature sensor =
  sig % arity is: (binding -> free)
  end

 rule observe_update =
   loc_l(dev_d | [0]) || devs(location_l',d | [1])
     ->
   loc_l(dev_d | [0]) || devs(location_l,d  | [1]) * l'/

 rule observe_new =
   ( /d . loc_l(dev_d | [0]) ) || devs([1])
     ->
   /d. ( loc_l(dev_d | [0]) || devs(location_l,d | [1]) )

 rule lose =
   loc_l([0]) || ( /d. devs(location_l',d | [1]) )
     ->
   loc_l([0]) || devs([1]) * l'/

 --------------------------------------------------------

 L: Location model

 signature repr =
   sig % arity is: (binding -> free)
     devs     : passive (0 -> 0)
     location : atomic (0 -> 2) % linked to loc and dev
   end 

 state = devs(location_l1,d1 | ... | location_ln,dn)

 --------------------------------------------------------

 A: Location-aware application

 signature agent =
  sig % arity is: (binding -> free)
    findall  : atomic (0 -> 0)
    whereis  : atomic (0 -> 1) % linked to the device sought
    location : atomic (0 -> 2) % linked to loc and dev
  end

 using agent

 rule findall =
   devs([0]) || findall
     ->
   devs([0]) || [0]

 rule whereis =
   devs(location_l,d | [0]) || whereis_d
     ->
   devs(location_l,d | [0]) || location_l,d

 rule genFindall =
   id_1 -> id_1 | findall

 rule genWhereis =
   id_1 * d/ -> id_1 * whereis_d

 --------------------------------------------------------

 SCENARIO:
 1. A ``whereis'' query for device d is issued.
 2. A move of d occurs in C.
 3. An answer to the query appears in A.
 4. Another ``whereis'' query is issued, this time for device d'.
 5. S discovers that d' occurs in C but not in L and reacts.
 6. An answer to this query appears in A.

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

(* exception handler *)
fun handler exn = (print (BaseErrorHandler.explain' exn) ; print "\n")

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

(* LazyList functions*)
fun lzSize l c =
    let fun lzSize' l' c' =
	    if LazyList.lznull(l') then Int.toString(c')
	    else lzSize' (LazyList.lztl l') (c'+1)
    in if c < 0 then "lzSize: Counter is initially less than zero.\n"
       else lzSize' l c
    end

fun lzLength l = Int.toString(List.length(LazyList.lztolist l))

(* take apart a match; context and parameter *)
fun parts agent matches =
    let val agent' = M.unmk (LazyList.lzhd matches)
	val agent'_ctx = #context(agent')
	val agent'_par = #parameter(agent')
	fun peel x = (B.toString o B.simplify o Bdnf.unmk) x
    in ["agent_ctx= " ^ (peel agent'_ctx) ^ "\n",
	"agent_par= " ^ (peel agent'_par) ^ "\n"] end

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

fun printRes' tname agent =
    ( print("Agent resulting from reaction on " ^ tname ^ ":\n")
    ; print((B.toString o B.simplify) agent)
    ; print "\n" )


(* SIGNATURE *)

(* C *)
fun loc l =
    ion2bg (Ion.make {ctrl = C.make("loc", C.Active, 1, 0),
		      free = [l], bound = []})

fun dev d =
    S.o (ion2bg (Ion.make {ctrl = C.make("dev", C.Atomic, 1, 0),
			   free = [d], bound = []}),
	 barren)

(* S has the empty signature *)

(* L *)
val devs =
    ion2bg (Ion.make {ctrl = C.make("devs", C.Passive, 0, 0),
		      free = [], bound = []})

fun location l d =
    S.o (ion2bg (Ion.make {ctrl = C.make("location", C.Atomic, 2, 0),
			   free = [l,d], bound = []}),
	 barren)

(* A *)
val findall =
    S.o (ion2bg (Ion.make {ctrl = C.make("findall", C.Atomic, 0, 0),
			   free = [], bound = []}),
	 barren)

fun whereis d =
    S.o (ion2bg (Ion.make {ctrl = C.make("whereis", C.Atomic, 1, 0),
			   free = [d], bound = []}),
	 barren)

fun make_plato (c,p,a) = S.|| (c, S.|| (p, a))


(* INITIAL STATE OF THE SYSTEM:
   C: loc_l'(loc_l(dev_d | dev_d')) ||
   L: devs(location_l',d) ||
   A: 1
*)
val loc_l' = loc (s2n "l'")
val loc_l = loc (s2n "l")
val dev_d = dev (s2n "d")
val dev_d' = dev (s2n "d'")
val C = S.o (loc_l', S.o (loc_l, S.`|` (dev_d, dev_d')))
val devs1 = devs
val location_l'd = location (s2n "l'") (s2n "d")
val L = S.o (devs1, location_l'd)
val A = barren
val system0 = make_plato(C,L,A)

val _ = print "\nsystem0\n"
val _ = prtSimp "state: " system0
val _ = printIfaces "system0" (getInner system0) (getOuter system0)

(* aux. function *)
fun makeBR bgval = Bdnf.regularize (Bdnf.make bgval)


(* RULES *)

(* auxiliary definitions *)
val loc_l1 = loc (s2n "l1")
val loc_l2 = loc (s2n "l2")
val dev_d3 = dev (s2n "d3")
val aux1 = S.o (loc_l1, S.`|` (id_1, S.o (loc_l2, S.`|` (id_1, dev_d3))))
val aux2 = S.o (loc_l1, S.`|` (id_1, S.`|` (S.o (loc_l2, id_1), dev_d3)))
val locdev = S.o (loc_l1, S.`|` (dev_d3, id_1))
val location_l1d3 = location (s2n "l1") (s2n "d3")
val location_l2d3 = location (s2n "l2") (s2n "d3")
val id_l1 = S.idw ["l1"]
val close_d3 = S.* (S.-/ "d3", id_1)
val id_l1_tt_close_d3 = S.* (id_l1, close_d3)
val id_l1_tt_close_d3_tt_id_1 = S.* (id_l1, S.* (close_d3, id_1))
val id_l2 = S.idw ["l2"]
val id_l2_tt_close_d3 = S.* (id_l2, close_d3)
val devs_loc_l2d3_id1 = S.o (devs, S.`|` (location_l2d3, id_1))
val devs_loc_l1d3_id1 = S.o (devs, S.`|` (location_l1d3, id_1))
val idle_l1 = S.// ("l1", [])
val idle_l2 = S.// ("l2", [])
val idle_d3 = S.// ("d3", [])
val whereis_d3 = whereis (s2n "d3")

(* C *)
val redexUp = aux1
val reactUp = aux2
val Cmoveup = R.make' { name = "Cmoveup",
			redex = makeBR redexUp,
			react = reactUp,
			info = info }
val redexDown = aux2
val reactDown = aux1
val Cmovedown = R.make' { name = "Cmovedown",
			  redex = makeBR redexDown,
			  react = reactDown,
			  info = info }

(* S *)
val redexObsUpd = S.|| (locdev, devs_loc_l2d3_id1)
val reactObsUpd = S.|| (locdev, S.* (devs_loc_l1d3_id1, idle_l2))
val Sobsupd = R.make' { name = "Sobsupd",
			redex = makeBR redexObsUpd,
			react = reactObsUpd,
			info = info }

val redexObsNew = S.|| (S.o (id_l1_tt_close_d3, locdev), devs)
val reactObsNew = S.o (id_l1_tt_close_d3_tt_id_1,
		       S.|| (locdev, S.`|` (location_l1d3, id_1)))
val Sobsnew = R.make' { name = "Sobsnew",
			redex = makeBR redexObsNew,
			react = reactObsNew,
			info = info }

val redexLose = S.|| (S.o (loc_l1, id_1),
		      S.o (id_l2_tt_close_d3, devs_loc_l2d3_id1))
val reactLose = S.|| (S.o (loc_l1, id_1), S.* (devs, idle_l2))
val Slose = R.make' { name = "Slose",
		      redex = makeBR redexLose,
		      react = reactLose,
		      info = info }

(* L has no rules *)

(* A *)
val redex_innerface_Findall = Iface.m 1
val react_innerface_Findall = Iface.m 2
(* [0 |-> 0, 1 |-> 0] *)
val instFindall = Inst.make { I = redex_innerface_Findall,
			      J = react_innerface_Findall,
			      maps = [((0,[]), (0,[])),
				      ((1,[]), (0,[]))] }
val redexFindall = S.|| (devs, findall)
val reactFindall = S.|| (devs, id_1)
val Afindall = R.make { name = "Afindall",
			 redex = makeBR redexFindall,
			 react = reactFindall,
			 inst = instFindall,
			 info = info }
(*    handle e => (handler e ;
		 R.make' { name = "dummy",
			   redex = makeBR barren,
			   react = barren,
			   info = info })*)

val redexWhereis = S.|| (S.o (devs, S.`|` (location_l1d3, id_1)),
			 whereis_d3)
val reactWhereis = S.|| (S.o (devs, S.`|` (location_l1d3, id_1)),
			 location_l1d3)
val Awhereis = R.make' { name = "Awhereis",
			 redex = makeBR redexWhereis,
			 react = reactWhereis,
			 info = info }

val redexGenFindall = id_1
val reactGenFindall = S.`|` (id_1, findall)
val AgenFindall = R.make' { name = "AgenFindall",
			    redex = makeBR redexGenFindall,
			    react = reactGenFindall,
			    info = info }

val redexGenWhereis = S.* (id_1, idle_d3)
val reactGenWhereis = S.`|` (id_1, whereis_d3)
val AgenWhereis = R.make' { name = "AgenWhereis",
			    redex = makeBR redexGenWhereis,
			    react = reactGenWhereis,
			    info = info }


(* REACTIONS *)

(* Example:
     loc_l'(loc_l(dev_d | dev_d')) || devs(location_l',d) || 1
-1-> loc_l'(loc_l(dev_d | dev_d')) || devs(location_l',d) || whereis_d
-2-> loc_l'(loc_l(dev_d') | dev_d) || devs(location_l',d) || whereis_d
-3-> loc_l'(loc_l(dev_d') | dev_d) || devs(location_l',d) || location_l',d
-4-> loc_l'(loc_l(dev_d') | dev_d) ||
     devs(location_l',d) ||
     (location_l',d | whereis_d')
-5-> loc_l'(loc_l(dev_d') | dev_d) ||
     devs(location_l',d | location_l,d') ||
     (location_l',d | whereis_d')
-6-> loc_l'(loc_l(dev_d') | dev_d) ||
     devs(location_l',d | location_l,d') ||
     (location_l',d | location_l,d')
*)

(* 1: --AgenWhereis-> *)
val BRsystem0 = makeBR system0
val mts0 = M.matches { agent = BRsystem0 , rule = AgenWhereis }
val match0 = LazyList.lznth mts0 3 (* zero-indexed *)
val system1 = Re.react match0 (* returns agent *)
val _ = printRes' "system0" system1

(* 2: --Cmoveup-> *)
val BRsystem1 = makeBR system1
val mts1 = M.matches { agent = BRsystem1 , rule = Cmoveup }
val _ = print("length(mts1) = " ^ (lzLength mts1) ^ "\n")
val _ = printMts mts1

(* 3: --SobsUpd-> *)
(* 4: --AgenWhereis-> *)
(* 5: ---> *)
(* 6: ---> *)

(*
val _ = print(M.toString(match1))
val match1 = LazyList.lznth mts1 ?
val system2 = Re.react match1
val _ = printRes' "system1" system2
*)

(*
val _ = case mtD of NONE => print "No matches!\n"
		  | SOME(m) => ( print(M.toString(m))
		    handle e => handler e )
*)

(* PRINTING FOR TESTING PURPOSES *)

(*
val _ = case mt1 of NONE => print "No matches!\n"
		  | SOME(m) => ( print(M.toString(m))
				 handle e => handler e )

val _ = map print (parts system mts1)
*)

(* print number of matches to stdout 
val _ = ( print "Number of matches: "
        ; print (lzSize mts1 0)
	; print "\n")
*)

(* print to stdout 
val _ = printMts mts1
*)

(* print to file 
open TextIO;

val os = openOut("matches.out")

fun printmatches ms =
    if LazyList.lznull(ms) then "Done\n"
    else ( output(os, (M.toString (LazyList.lzhd ms)) ^ "\n")
	 ; printmatches (LazyList.lztl ms) )

val out_a = printmatches mts1

val _ = flushOut(os)
val _ = closeOut(os)
*)
