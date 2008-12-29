(********************************************************************
 Ebbe Elsborg, 25/12/2008
 --------------------------------------------------------------------
 Compile: Do 'make abs-locmod-sim' in 'impl/bpl/src'
 Run: Do './abs-locmod-sim' in 'impl/bpl/src'
 Using the dependencies in impl/bpl/src/apps/miniml/Dependencies
 --------------------------------------------------------------------
 An abstract Plato-graphical location model.
 Simulation of a simple predefined scenario.

 INVARIANTS:
  - If a node has control 'id' then it contains exactly one node,
    which has control 'iN'. (iN ranges over i0, i1, i2...)
  - If a node has control 'dev' then it contains exactly one node,
    which has control 'iN'.
  - If a node has control 'loc' then it contains at least one node,
    which has control 'id'.
  - If a node has control 'location' then it contains exactly two
    nodes, one which has control 'l' and the other control 'd'.
  - If a node has control 'l' then it contains exactly one node,
    which has control 'iN'.
  - If a node has control 'd' then it contains exactly one node,
    which has control 'iN'.
  - If a node with control 'iN' occurs inside a node with control
    'id', then there does /not/ exist a node with the same control
    'iN' inside a node with control 'dev', and vice versa.

 Negative Application Conditions (NACs):
  - See rules 'observe_new' and 'lose'.

 --------------------------------------------------------------------

 C: Real world

 signature world =
   sig %
     loc : passive (0)
     dev : passive (0)
     id  : passive (0)
     i0,i1,... : atomic
   end

 using world

 rule moveup  = (* sort: C *)
   loc(id([0]) | [1] | loc(id([2]) | [3] | dev([4])))
     ->
   loc(id([0]) | [1] | loc(id([2]) | [3]) | dev([4]))

 rule movedown = (* sort: C *)
   loc(id([0]) | [1] | loc(id([2]) | [3]) | dev([4]))
     ->
   loc(id([0]) | [1] | loc(id([2]) | [3] | dev([4])))

 state = loc(id(n) | loc(...) | dev(id(m)) | ...)

 --------------------------------------------------------------------

 S: Sensor system

  signature sensor =
  sig end

 (* rule schema, one for each n in i0,01,i2... *)
 rule observe_update = (* sort: C,L *)
   loc(id([0]) | [1] | dev(n)) ||
   devs(location(l([2]) | d(n)) | [3])
     ->
   loc(id([0]) | [1] | dev(n)) ||
   devs(location(l([0]) | d(n)) | [3])

 (* need a NAC to ensure that the iN node in [2] is not in [3] *)
 rule observe_new = (* sort: C,L *)
   loc(id([0]) | [1] | dev([2])) ||
   devs([3])
     ->
   loc(id([0]) | [1] | dev([2])) ||
   devs([3] | location(l([0]) | d([2])))

 (* need a NAC to ensure that the iN node in [3] is not in [0] *)
 rule lose = (* sort: C,L *)
   loc([0]) || devs([1] | location(l([2]) | d([3])))
     ->
   loc([0]) || devs([1])

 --------------------------------------------------------------------

 L: Location model

 signature repr =
   sig
     devs     : passive (0)
     location : passive (0)
     l: passive (0)
     d: passive(0)
   end

 state = devs(location(l(n),d(m)) | ...)

 --------------------------------------------------------------------

 A: Location-aware application

 signature agent =
  sig
    findall  : atomic
    whereis  : passive (0)
    i0,i1,i2... : atomic
    id : passive (0)
    location : passive (0)
    l: passive(0)
    d: passive(0)
  end

 using agent

 rule findall = (* sort: L,A *)
   devs([0]) || findall
     ->
   devs([0]) || [0]

 rule whereis = (* sort: L,A *)
   devs(location(l([0]) | d(id([1])) | [2])) || whereis([1])
     ->
   devs(location(l([0]) | d(id([1])) | [2]))
   || location(l([0]) | d(id([1])))

 rule genFindall = (* sort: A *)
   [0] -> [0] | findall

 (* rule schema, a rule for each n in i0,01,i2... *)
 rule genWhereis = (* sort: A *)
   [0] -> [0] | whereis(n)

 --------------------------------------------------------------------

 INITIAL STATE OF THE SYSTEM:
   C: loc(id(i1) | loc(id(i2) | dev(i3) | dev(i4))) ||
   L: devs(location(l(i2) | d(i3))) ||
   A: 1

 --------------------------------------------------------------------

 SCENARIO:
 1. A ``whereis'' query for device i3 is issued.
 2. A move of device i3 occurs in C.
 3. An answer to the query appears in A.
 4. Another ``whereis'' query is issued, this time for device i4.
 5. S discovers that device i4 occurs in C but not in L and reacts.
 6. An answer to this query appears in A.

 EXTENDED:
 7. A ``findall'' query is issued.
 8. An answer to this query appears in A.
 9. S updates L with info that device i3 is in location i1.
10. Device i3 moves back into location l2 in C.

********************************************************************)

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

(* make look nicer *)
val _ = Flags.setBoolFlag "/kernel/ast/bgterm/ppids" false
val _ = Flags.setBoolFlag "/kernel/ast/bgterm/ppabs" false
val _ = Flags.setBoolFlag "/kernel/ast/bgterm/pp0abs" false
val _ = Flags.setBoolFlag "/kernel/ast/bgterm/pptenaspar" true
val _ = Flags.setBoolFlag "/kernel/ast/bgterm/ppmeraspri" true
val _ = Flags.setBoolFlag "/kernel/ast/bgval/pp-simplify" true

(* useful constants *)
val info = BG.Info.noinfo
val barren = S.<->
val id_1 = B.Per info (P.id_n 1)
val site = id_1 (*`[]`*)

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
       ; print ">") end

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

fun printRes' tname rname agent =
    ( print("Agent resulting from reaction with "
	    ^ rname ^ " on " ^ tname ^ ":\n")
    ; print((B.toString o B.simplify) agent)
    ; print "\n" )


(* SIGNATURE *)

(* C *)
fun i n = S.atomic0 ("i" ^ n)
val id = S.passive0 "id"
val loc = S.passive0 "loc"
val dev = S.passive0 "dev"

(* S has the empty signature *)

(* L *)
val l = S.passive0 "l"
val d = S.passive0 "d"
val devs = S.passive0 "devs"
val location = S.passive0 "location"

(* A *)
val findall = S.atomic0 "findall"
val whereis = S.passive0 "whereis"

(* A shares 'fun i' and 'val id' with C. *)
(* A shares 'val l', 'val d', and 'val location' with L. *)

(* Plato-graphical systems *)
fun make_plato (c,p,a) = S.|| (c, S.|| (p, a))

(* auxiliary definitions *)
val loc' = S.o (loc, S.`|` (id, site))
fun loc'' n = S.o (loc, S.`|` (S.o (id, i(n)), site))
fun dev' n = S.o (dev, i(n))
fun devs' h = S.o (S.passive0 "devs", h)
val locdev = S.o (loc, S.`|` (id, S.`|` (site, dev)))
fun locdev' n = S.o (loc, S.`|` (id, S.`|` (site, dev' n)))
val location' = S.o (location, S.`|` (l, d))
fun location'' lid did =
    S.o (location, S.`|` (S.o (l, i(lid)), S.o (d, i(did))))
fun location''' did =
    S.o (location, S.`|` (l, S.o (d, i(did))))
fun whereis' c = S.o (S.passive0 "whereis", c)

(* initial state of the system *)
val loc1 = loc'' "1"
val loc2 = loc'' "2"
val dev3 = dev' "3"
val dev4 = dev' "4"
val C = S.o (loc1, S.o (loc2, S.`|` (dev3, dev4)))
val location_l2d3 = location'' "2" "3"
val L = S.o (devs, location_l2d3)
val A = barren
val system0 = make_plato(C,L,A)

val _ = print "\nsystem0\n"
val _ = prtSimp "state: " system0
val _ = printIfaces "system0" (getInner system0) (getOuter system0)

(* aux. function *)
fun makeBR bgval = Bdnf.regularize (Bdnf.make bgval)


(* RULES *)

(* C *)

val aux1 = S.o (loc, S.`|` (id, S.`|`(site, S.o (loc, S.`|`(id, S.`|` (site, S.`|` (dev, dev)))))))
val aux2 = S.o (loc, S.`|` (id, S.`|`(site, S.`|` (S.o (loc, S.`|`(id, S.`|` (site, dev))), dev))))
val redex_innerface_move = Iface.m 6
val react_innerface_move = Iface.m 6
val instMove = Inst.make { I = redex_innerface_move,
			   J = react_innerface_move,
			   maps = [((0,[]), (0,[])),
				   ((1,[]), (1,[])),
				   ((2,[]), (2,[])),
				   ((3,[]), (3,[])),
				   ((4,[]), (5,[])),
				   ((5,[]), (4,[]))] }

val redexUp = aux1
val reactUp = aux2
val Cmoveup = R.make { name = "Cmoveup",
		       redex = makeBR redexUp,
		       react = reactUp,
		       inst = instMove,
		       info = info }

val redexDown = aux2
val reactDown = aux1
val Cmovedown = R.make { name = "Cmovedown",
			 redex = makeBR redexDown,
			 react = reactDown,
			 inst = instMove,
			 info = info }

(* S *)
(* rule schema, one for each n in N *)
val devs_dn = S.o (devs, S.`|` (location''' "n", site))
val redexObsUpd = S.|| (locdev' "n", devs_dn)
val reactObsUpd = redexObsUpd
val redex_innerface_upd = Iface.m 4
val react_innerface_upd = Iface.m 4
val instUpd = Inst.make { I = redex_innerface_upd,
			  J = react_innerface_upd,
			  maps = [((0,[]), (0,[])),
				  ((1,[]), (1,[])),
				  ((2,[]), (0,[])),
				  ((3,[]), (3,[]))] }
val Sobsupd = R.make { name = "Sobsupd",
		       redex = makeBR redexObsUpd,
		       react = reactObsUpd,
		       inst = instUpd,
		       info = info }

(* need a NAC to ensure that iN of dev is not in devs *)
val redexObsNew = S.|| (locdev, devs)
val reactObsNew = S.|| (locdev, devs' (S.`|` (site, location')))
val redex_innerface_new = Iface.m 4
val react_innerface_new = Iface.m 6
val instNew = Inst.make { I = redex_innerface_new,
			  J = react_innerface_new,
			  maps = [((0,[]), (0,[])),
				  ((1,[]), (1,[])),
				  ((2,[]), (2,[])),
				  ((3,[]), (3,[])),
				  ((4,[]), (0,[])),
				  ((5,[]), (2,[]))] }
val Sobsnew = R.make { name = "Sobsnew",
		       redex = makeBR redexObsNew,
		       react = reactObsNew,
		       inst = instNew,
		       info = info }

(* need a NAC to ensure that site in dev is not in site of loc *)
val redexLose = S.|| (loc, devs' (S.`|` (site, location')))
val reactLose = S.|| (loc, devs)
val Slose = R.make' { name = "Slose",
		      redex = makeBR redexLose,
		      react = reactLose,
		      info = info }

(* L has no rules *)

(* A *)
val redex_innerface_findall = Iface.m 1
val react_innerface_findall = Iface.m 2
val instFindall = Inst.make { I = redex_innerface_findall,
			      J = react_innerface_findall,
			      maps = [((0,[]), (0,[])),
				      ((1,[]), (0,[]))] }
val redexFindall = S.|| (devs, findall)
val reactFindall = S.|| (devs, site)
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

(* rule schema, a rule for each n in N, we need two concrete now *)
val redexWhereis3 = S.|| (devs' (S.`|` (location''' "3", site)),
			 whereis'(i("3")))
val reactWhereis3 = S.|| (devs' (S.`|` (location''' "3", site)),
			 location''' "3")
val redexWhereis4 = S.|| (devs' (S.`|` (location''' "4", site)),
			 whereis'(i("4")))
val reactWhereis4 = S.|| (devs' (S.`|` (location''' "4", site)),
			 location''' "4")
val redex_innerface_whereis = Iface.m 2
val react_innerface_whereis = Iface.m 3
val instWhereis = Inst.make { I = redex_innerface_whereis,
			      J = react_innerface_whereis,
			      maps = [((0,[]), (0,[])),
				      ((1,[]), (1,[])),
				      ((2,[]), (0,[]))] }
val Awhereis3 = R.make { name = "Awhereis3",
			 redex = makeBR redexWhereis3,
			 react = reactWhereis3,
			 inst = instWhereis,
			 info = info }
val Awhereis4 = R.make { name = "Awhereis4",
			 redex = makeBR redexWhereis4,
			 react = reactWhereis4,
			 inst = instWhereis,
			 info = info }

val redexGenFindall = site
val reactGenFindall = S.`|` (site, findall)
val AgenFindall = R.make' { name = "AgenFindall",
			    redex = makeBR redexGenFindall,
			    react = reactGenFindall,
			    info = info }

(* rule schema, a rule for each n in N, we need two concrete now *)
val redexGenWhereis3 = site
val reactGenWhereis3 = S.`|` (site, whereis'(i("3")))
val AgenWhereis3 = R.make' { name = "AgenWhereis3",
			     redex = makeBR redexGenWhereis3,
			     react = reactGenWhereis3,
			     info = info }
val redexGenWhereis4 = site
val reactGenWhereis4 = S.`|` (site, whereis'(i("4")))
val AgenWhereis4 = R.make' { name = "AgenWhereis4",
			     redex = makeBR redexGenWhereis4,
			     react = reactGenWhereis4,
			     info = info }

(* REACTIONS *)

(* Scenario:
 loc(id(i1) | loc(id(i2) | dev(i3) | dev(i4))) ||
 devs(location(l(i2) | d(i3))) ||
 1
   --1:Agenwhereis3->
 loc(id(i1) | loc(id(i2) | dev(i3) | dev(i4))) ||
 devs(location(l(i2) | d(i3))) ||
 whereis(i3)
   --2:Cmoveup->
 loc(id(i1) | dev(i3) | loc(id(i2) | dev(i4))) ||
 devs(location(l(i2) | d(i3))) ||
 whereis(i3)
   --3:Awhereis3->
 loc(id(i1) | dev(i3) | loc(id(i2) | dev(i4))) ||
 devs(location(l(i2) | d(i3))) ||
 location(l(i2) | d(i3))
   --4:AgenWhereis4->
 loc(id(i1) | dev(i3) | loc(id(i2) | dev(i4))) ||
 devs(location(l(i2) | d(i3))) ||
 (location(l(i2) | d(i3)) | whereis(i4))
   --5:Sobsnew->
 loc(id(i1) | dev(i3) | loc(id(i2) | dev(i4))) ||
 devs(location(l(i2) | d(i3)) | location(l(i1) | d(i4))) ||
 (location(l(i2) | d(i3)) | whereis(i4))
   --6:Awhereis4->
 loc(id(i1) | dev(i3) | loc(id(i2) | dev(i4))) ||
 devs(location(l(i2) | d(i3)) | location(l(i1) | d(i4))) ||
 (location(l(i2) | d(i3)) | location(l(i1) | d(i4)))

 (7. A ``findall'' query is issued.)
 (8. An answer to this query appears in A.)
*)

(* 1: --AgenWhereis-> *)
val BRsystem0 = makeBR system0
val mts0 = M.matches { agent = BRsystem0 , rule = AgenWhereis3 }
val match0 = LazyList.lznth mts0 4 (* zero-indexed *)
val system1 = Re.react match0 (* return agent *)
val _ = print "\n"
val _ = printRes' "system0" "AgenWhereis" system1

(* 2: --Cmoveup-> *)
val BRsystem1 = makeBR system1
val mts1 = M.matches { agent = BRsystem1 , rule = Cmoveup }
val match1 = LazyList.lznth mts1 0
val system2 = Re.react match1
val _ = print "\n"
val _ = printRes' "system1" "Cmoveup" system2

(* 3: --Awhereis-> *)
val BRsystem2 = makeBR system2
val mts2 = M.matches { agent = BRsystem2 , rule = Awhereis3 }
val match2 = LazyList.lznth mts2 0
val system3 = Re.react match2
val _ = print "\n"
val _ = printRes' "system2" "Awhereis" system3

(* 4: --AgenWhereis-> *)
val BRsystem3 = makeBR system3
val mts3 = M.matches { agent = BRsystem3 , rule = AgenWhereis4 }
val match3 = LazyList.lznth mts3 5
val system4 = Re.react match3
val _ = print "\n"
val _ = printRes' "system3" "AgenWhereis" system4

(* 5: --Sobsnew-> *)
val BRsystem4 = makeBR system4
val mts4 = M.matches { agent = BRsystem4 , rule = Sobsnew }
val _ = print("length(mts4) = " ^ (lzLength mts4) ^ "\n")
val _ = printMts mts4
(*
val match4 = LazyList.lznth mts4 0
val system5 = Re.react match4
val _ = print "\n"
val _ = printRes' "system4" "Sobsnew" system5
*)
(*
(* 6: --Awhereis-> *)
val BRsystem5 = makeBR system5
val mts5 = M.matches { agent = BRsystem5 , rule = Awhereis4 }
val _ = print("length(mts5) = " ^ (lzLength mts5) ^ "\n")
val _ = printMts mts5

val match5 = LazyList.lznth mts5 0
val system6 = Re.react match5
val _ = print "\n"
val _ = printRes' "system5" "Sobsnew" system6
val _ = print "\n"
*)

(* 7: --AgenFindall-> *)
(* 8: --AFindall-> *)

(*
 7. A ``findall'' query is issued.
 8. An answer to this query appears in A.
 9. S updates L with info that device i3 is in location i1.
10. Device i3 moves back into location l2 in C.
*)

(* bgval2svg *)
(*val _ = outputsvgdoc "system1.svg" system1*)
(*val _ = outputsvgdoc_v*)

(* bgbdnf2svg 
val _ = outputsvgdoc_b
*)

(* 3: --Awhereis-> 
val BRsystem2 = makeBR system2
val mts2 = M.matches { agent = BRsystem2 , rule = Awhereis }
val _ = print("length(mts2) = " ^ (lzLength mts2) ^ "\n")
val _ = printMts mts2
*)
(*
val match2 = LazyList.lzhd mts2
val system3 = Re.react match2
val _ = printRes' "system2" "Awhereis" system3
*)

(* 4: --AgenWhereis-> *)
(* 5: --Sobsupd-> *)

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
        ; print (lzLength mts1)
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

val out_a = printmatches mts0

val _ = flushOut(os)
val _ = closeOut(os)
*)
