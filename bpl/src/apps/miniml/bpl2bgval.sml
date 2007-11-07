(* Copyright (c) 2006  The BPL Group at the IT University of Copenhagen
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

(*
 Mapping from BPL abstract syntax tree to BgVal. 
 Implements bpl/bplproject/doc/projects/contextawareness/plato/bpl-bnf.tex

 make apps/miniml/bpl2bgval.uo
*)

(*open TextIO;*)

structure BG = BG (structure ErrorHandler = PrintErrorHandler);
structure B = BG.BgVal
structure BgTerm = BG.BgTerm
structure Sugar = BG.Sugar
structure Rule = BG.Rule
structure Control = BG.Control
structure Ion = BG.Ion
structure Name = BG.Name
structure Wiring  = BG.Wiring
structure NameSet = BG.NameSet
structure Link = BG.Link
structure LinkSet = BG.LinkSet
structure Origin = Origin
structure Instantiation = BG.Instantiation
structure Rule = BG.Rule

(*
structure P = BG.Permutation
structure Bdnf    = BG.BgBDNF
structure M       = BG.Match
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
*)

(***** BNF *****)
type id = string
type ctrlid = string
type nat = int
datatype siteId = Num of nat
		| Var of id
datatype wire = Global of id * id
	      | Local of id * id
	      | IdleG of id
	      | IdleL of id
type wires = wire list
type names = id list
datatype namelist = Namelist of names
type ports = names
datatype bigraph = Wir of wires
		 | Par of bigraph * bigraph
		 | Pri of bigraph * bigraph
		 | Com of bigraph * bigraph
		 | Emb of bigraph * bigraph
		 | Ten of bigraph * bigraph
		 | Ctrl of ctrlid * ports * ports
		 | Clo of names * bigraph
		 | Abs of names * bigraph
		 | Site of siteId * namelist
		 | Id of id
		 | Empty
datatype ctrlkind = Active
		  | Passive
		  | Atomic
datatype ctrldef = Cdef of ctrlid * ctrlkind * nat * nat
type ctrldefs = ctrldef list
datatype dec = Rule of id * bigraph * bigraph
	     | Value of id * bigraph
type decs = dec list
type signatur = ctrldefs
datatype prog = Prog of signatur * decs

type bgval = B.bgval

type sitemap = (nat * siteId) list

val barren = Sugar.<-> (* barren root *)
val info = BG.Info.noinfo

(* OPERATORS *)
infixr || (* parallel product *)
infixr pp (* prime product *)
infixr tt (* tensor product *)
infixr oo (* composition *)
fun (b1:bgval) || (b2:bgval) = Sugar.|| (b1,b2)
fun (b1:bgval) pp (b2:bgval) = Sugar.`|` (b1,b2)
fun (b1:bgval) tt (b2:bgval) = Sugar.* (b1,b2)
fun (b1:bgval) oo (b2:bgval) = Sugar.o (b1,b2)

(***** AUXILIARY FUNCTIONS *****)
(* return list of sites of a bigraph *)
fun getSites (b:bigraph) =
    case b of Wir(w) => []
	    | Par(b1,b2) => (getSites b1) @ (getSites b2)
	    | Pri(b1,b2) => (getSites b1) @ (getSites b2)
	    | Com(b1,b2) => (getSites b1) @ (getSites b2)
	    | Emb(b1,b2) => (getSites b1) @ (getSites b2)
	    | Ten(b1,b2) => (getSites b1) @ (getSites b2)
	    | Ctrl(i,b,f) => []
	    | Clo(n,b) => getSites b
	    | Abs(n,b) => getSites b
	    | Site(i,l) => [Site(i,l)]
	    | Id(i) => []
	    | Empty => []

(* return list of sites ids (Nats) of a bigraph *)
fun getSiteNums (b:bigraph) =
    case b of Wir(w) => []
	    | Par(b1,b2) => (getSiteNums b1) @ (getSiteNums b2)
	    | Pri(b1,b2) => (getSiteNums b1) @ (getSiteNums b2)
	    | Com(b1,b2) => (getSiteNums b1) @ (getSiteNums b2)
	    | Emb(b1,b2) => (getSiteNums b1) @ (getSiteNums b2)
	    | Ten(b1,b2) => (getSiteNums b1) @ (getSiteNums b2)
	    | Ctrl(i,b,f) => []
	    | Clo(n,b) => getSiteNums b
	    | Abs(n,b) => getSiteNums b
	    | Site(i,l) => [i] (* assumes that sites are Nats already *)
	    | Id(i) => []
	    | Empty => []


(* partition an smap into two smaps according to pivot p *)
fun partSmap [] acc p = (acc,[])
  | partSmap ((n,i)::m) acc p =
    if p=n then (rev acc, (n,i)::m) else partSmap m ((n,i) :: acc) p

(* lookup first occurence of id and return corresponding key (Nat) *)
fun lookupFst [] id = NONE
  | lookupFst ((n,i)::m) id = if i=id then SOME(n) else lookupFst m id

(* lookup first occurence of n2 and return corresponding n1 *)
fun lookupSnd [] id = NONE
  | lookupSnd ((n1,n2)::m) id = if id=n1 then SOME(n2) else lookupSnd m id

fun nat2int n = n

fun nat2str n = Int.toString(nat2int(n))

(* calculate 'maps', i.e. a 'map list', for instantiation *)
fun calcMaps (b1:bigraph) (b2:bigraph) pivot smap =
    let val (smap1,smap2) = partSmap smap [] pivot
	(* for each site in b2, find target site in b1 if it exists *)
	val auxList = List.map (fn (n,i) => let val x = lookupFst smap1 i
					    in (n,x) end)
			       smap2
	fun err n = "calcMaps: Site " ^ nat2str(n) ^
		    " has no LHS counterpart"
	(* peel off options from second components of generated auxlist *)
	fun peel assocOptList =
	    case assocOptList
	     of [] => []
	      | ((n,x)::m) => case x of NONE => raise Fail(err n)
				      | SOME(i) => (n,i) :: peel m
	val pldList = peel auxList
	(* generate 'maps'; (int * name list) * (int * name list) list *)
	val sites1 = getSites b1
	val sites2 = getSites b2
	fun makeMaps slist1 slist2 alist =
	    List.map
	    ( fn Site(n,l) =>
		 let val b1siteId = lookupSnd auxList n
		     val b1site =
			 case b1siteId
			  of SOME(m) =>
			     ( case lookupFst sites1 b1siteId
				of SOME(s) => s
				 | NONE => raise Fail(err n) )
			   | NONE => raise Fail(err n)
		     val b2site = Site(n,l)
		 in (b2site, b1site) end )
	    slist2
	val maps = makeMaps sites1 sites2 auxlist
    in maps end

fun s2n s = Name.make s

fun v2n x = s2n (String.toString x)

fun mkWir1 ovar =
    let val link = Link.make {outer = SOME(v2n ovar),
			      inner = NameSet.empty}
	val linkset = LinkSet.insert link LinkSet.empty
	val wiring = Wiring.make linkset
    in B.Wir info wiring end

fun mkWir2 ovar ivar =
    let val link = Link.make
		       {outer = SOME(v2n ovar),
			inner = NameSet.insert (v2n ivar) NameSet.empty}
	val linkset = LinkSet.insert link LinkSet.empty
	val wiring = Wiring.make linkset
    in B.Wir info wiring end

fun lookupKind cid signa =
    let fun loop [] = NONE
	  | loop ((i,k,b,f)::p) = if cid = i then SOME k else loop p
    in loop signa end

fun lookupBgval id idmap =
    let fun loop [] = NONE
          | loop ((i,b)::p) = if id = i then SOME b else loop p
    in loop idmap end

fun nm2nmSet n = NameSet.insert n NameSet.empty

fun s2nmSet s = (nm2nmSet o s2n) s

fun strList2nmSetList l = List.map (nm2nmSet o s2n) l

fun lastCnt [] = 0
  | lastCnt ((k,v)::m) = #1(List.hd(rev m))

fun ctrlNotInSig cid =
    raise Fail("Control does not exist in signature: " ^ cid ^ "\n")

fun abs p = B.Abs info p

fun rmDubs [] = []
  | rmDubs (x::xs) =
    if List.exists (fn y => y = x) xs then rmDubs xs else x :: rmDubs xs


(* transform a dec/big into a bgval *)
fun big2bgval (ast:bigraph) signa =
    (* HERE insert ids...?! *)
    case ast
     of Wir(w) =>
	let fun w2bgval wire =
		case wire
		 of Global(out,inn) => mkWir2 out inn
		  | Local(out,inn) => abs (s2nmSet out, mkWir2 out inn)
		  | IdleG(x) => mkWir1 x
		  | IdleL(x) => abs (s2nmSet x, mkWir1 x)
	    val wires = List.map w2bgval w
	    val bgval = B.Par info wires
		handle NotParallelisable =>
		       raise Fail("big2bgval: Inner names not disjoint")
	in bgval end
      | Par(b1,b2) =>
	let val b1' = big2bgval b1 signa
	    val b2' = big2bgval b2 signa
	in b1' || b2' end
      | Pri(b1,b2) =>
	let val b1' = big2bgval b1 signa
	    val b2' = big2bgval b2 signa
	in b1' pp b2' end
      | Com(b1,b2) =>
	let val b1' = big2bgval b1 signa
	    val b2' = big2bgval b2 signa
	in b1' oo b2' end
      | Emb(b1,b2) =>
	let val b1' = big2bgval b1 signa
	    val b2' = big2bgval b2 signa
	in b1' oo b2' end
      | Ten(b1,b2) =>
	let val b1' = big2bgval b1 signa
	    val b2' = big2bgval b2 signa
	in b1' tt b2' end
      | Ctrl(cid,bports,fports) =>
	let val boundnames = strList2nmSetList bports
	    val freenames = List.map s2n fports
	    val bSz = List.length bports
	    val fSz = List.length fports
	in case lookupKind cid signa
	    of SOME(k) =>
	       let val ion = Ion.make {ctrl = Control.make(cid,k,bSz,fSz),
				       free = freenames,
				       bound = boundnames}
		   val bgval = B.Ion info ion
	       in bgval end
	     | NONE => ctrlNotInSig cid
	end
      | Clo(nms,b) =>
	let val bgval1 = Sugar.-//nms(*(List.map s2n nms)*)
	    val (bgval2, smap') = big2bgval b signa maps
	in bgval1 oo bgval2 end
      | Abs(nms,b) =>
	let val nmSetList = strList2nmSetList (rmDubs nms)
	    val union = fn (nmset,acc) => NameSet.union nmset acc
	    val nmSet = List.foldr union NameSet.empty nmSetList
	    val b' = big2bgval b signa
	    val abst = abs(nmSet, b')
	in abst end
      (* | Conc(nms,b) => not used *)
      | Site(i,nmList) =>
	let val id1 = Sugar.merge 1
	    val nms = case nmList of Namelist(sl) => sl
	    val idw = Sugar.idw nms(*(List.map s2n nms)*)
	    val bgval = id1 tt idw
	in bgval end
      | Id(i) => raise Fail("big2bgval: " ^ "Unbound identifier: " ^ i)
      | Empty => barren

(*
fun big2bgval ast signa (maps:idmap*sitemap) =
    let val imap = #1(maps)
	val smap = #2(maps) (* to be updated in this function *)
	val cnt = lastCnt smap
    in case ast
	of Wir(w) =>
	   let fun w2bgval wire =
		   case wire
		    of Global(out,inn) =>  mkWir2 out inn
		     | Local(out,inn) => abs (s2nmSet out, mkWir2 out inn)
		     | IdleG(x) => mkWir1 x
		     | IdleL(x) => abs (s2nmSet x, mkWir1 x)
	       val wires = List.map w2bgval w
	       val bgval = B.Par info wires
		   handle NotParallelisable =>
			  raise Fail("big2bgval: Inner names not disjoint")
	   in (bgval, smap) end
	 | Par(b1,b2) =>
	   let val (b1',smap') = big2bgval b1 signa maps
	       val (b2',smap'') = big2bgval b2 signa (imap,smap')
	   in (b1' || b2', smap'') end
	 | Pri(b1,b2) =>
	   let val (b1',smap') = big2bgval b1 signa maps
	       val (b2',smap'') = big2bgval b2 signa (imap,smap')
	   in (b1' pp b2', smap'') end
	 | Com(b1,b2) =>
	   let val (b1',smap') = big2bgval b1 signa maps
	       val (b2',smap'') = big2bgval b2 signa (imap,smap')
	   in (b1' oo b2', smap'') end
	 | Emb(b1,b2) =>
	   let val (b1',smap') = big2bgval b1 signa maps
	       val (b2',smap'') = big2bgval b2 signa (imap,smap')
	   in (b1' oo b2', smap'') end
	 | Ten(b1,b2) =>
	   let val (b1',smap') = big2bgval b1 signa maps
	       val (b2',smap'') = big2bgval b2 signa (imap,smap')
	   in (b1' tt b2', smap'') end
	 | Ctrl(cid,bports,fports) =>
	   let val boundnames = strList2nmSetList bports
	       val freenames = List.map s2n fports
	       val bSz = List.length bports
	       val fSz = List.length fports
	   in case lookupKind cid signa
	       of SOME(k) =>
		  let val ion = Ion.make {ctrl = Control.make(cid,k,bSz,fSz),
					  free = freenames,
					  bound = boundnames}
		      val bgval = B.Ion info ion
		  in (bgval, smap) end
		| NONE => ctrlNotInSig cid
	   end
	 | Clo(nms,b) =>
	   let val bgval1 = Sugar.-//nms(*(List.map s2n nms)*)
	       val (bgval2, smap') = big2bgval b signa maps
	   in (bgval1 oo bgval2, smap') end
	 | Abs(nms,b) =>
	   let val nmSetList = strList2nmSetList (rmDubs nms)
	       val union = fn (nmset,acc) => NameSet.union nmset acc
	       val nmSet = List.foldr union NameSet.empty nmSetList
	       val (b',smap') = big2bgval b signa maps
	       val abst = abs(nmSet, b')
	   in (abst, smap') end
	 (* | Conc(nms,b) => not used *)
	 | Site(i,nmList) =>
	   let val id1 = Sugar.merge 1
	       val nms = case nmList of Namelist(sl) => sl
	       val idw = Sugar.idw nms(*(List.map s2n nms)*)
	       val bgval = id1 tt idw
	   in (bgval, (cnt+1,i) :: smap) end
	 | Id(i) =>
	   ( case lookupBgval i imap
	      of SOME(b) => (b, smap)
	       | NONE => raise Fail("big2bgval: " ^
				    "Unbound identifier: " ^ i)
	   )
	 | Empty => (barren, smap)
    end
*)

(* In: numbered declist, out: main bgval and list of rules (bgval pairs) *)
fun decs2bgvals decls mainVal rules signa smap =
    case decls
     of [] => (mainVal, rules)
      | (d::ds) =>
	( case d
	   of Value(i,b) =>
	      let val b' = big2bgval b signa
		  val mainBgval = b'
	      in decs2bgval ds mainBgval rules signa smap end
	    | Rule(i,b1,b2) =>
	      let val pivot = ((nat2int o List.last o getSiteNums) b1) + 1
		  val maplist = calcMaps b1 b2 pivot smap
		  val b1' = big2bgval b1 signa
		  val b2' = big2bgval b2 signa
		  val ins = Instantiation.make { I = b1'.innerface,
						 J = b2'.innerface,
						 maps = maplist }
		  val rule = Rule.make { info = Origin.unknown_origin,
					 inst = ins,
					 name = i,
					 react = b1',
					 redex = b2'}
		  val rules' = rule :: rules
	      in decs2bgval ds mainVal rules' signa smap end
	)

(*
fun dec2bgval decls vals rules signa (maps:idmap*sitemap) =
    let val imap = #1(maps) (* for rolling bgval list into main bgval *)
	val smap = #2(maps) (* for making instantiations *)
    in case decls
	of [] => (vals, rules, maps)
	 | (d::ds) =>
	   ( case d
	      of Rule(i,b1,b2) =>
		 let val (b1',smap') = big2bgval b1 signa maps
		     val (b2',smap'') = big2bgval b2 signa (imap,smap')
		     val pivot = ((site2int o List.last o getSites) b1) + 1
		     val ins = calcInst b1' b2' pivot (imap,smap'')
		     val rule = Rule.make { info = Origin.unknown_origin,
					    inst = ins,
					    name = i,
					    react = b1',
					    redex = b2'}
		     val rules' = rule :: rules
		 in dec2bgval ds vals rules' signa (imap,smap'') end
	       | Value(i,b) =>
		 let val (b',smap') = big2bgval b signa maps
		     val imap' = (i,b') :: imap (* update imap *)
		     val vals' = b' :: vals
		 in dec2bgval ds vals' rules signa (imap',smap') end )
    end
*)

(* substitute a bigraph b1 into a bigraph b2, recursively *)
fun sub b1 b2 =
    case b2 of Wir(w) => Wir(w)
	    | Par(b,b') => Par(sub b1 b, sub b1 b')
	    | Pri(b,b') => Pri(sub b1 b, sub b1 b')
	    | Com(b,b') => Com(sub b1 b, sub b1 b')
	    | Emb(b,b') => Emb(sub b1 b, sub b1 b')
	    | Ten(b,b') => Ten(sub b1 b, sub b1 b')
	    | Ctrl(i,b,f) => Ctrl(i,b,f)
	    | Clo(n,b) => Clo(n, sub b1 b)
	    | Abs(n,b) => Abs(n, sub b1 b)
	    | Site(i,l) => Site(i,l)
	    | Id(i) => b1 (* assumes that b1 is already rolled *)
	    | Empty => Empty

(* substitute a bigraph b into every following element in a bigraph list *)
fun subList [] = []
  | subList (b::bs) =
    let val newlist = 
	    List.map
		(fn b' =>
		    case b' of Value(i,big) => Value(i, sub b b')
			     | Rule(i,b1,b2) => Rule(sub b b1, sub b b2))
		bs
    in b :: newlist end

(* roll a declist (vals/rules) *)
fun roll [] = []
  | roll (d::ds) =
    if ds = [] then d
    else let val newlist = subList (d::ds)
	     val hd = List.hd newlist
	     val tl = List.tl newlist
	 in hd :: roll tl end

(* delete all except the main val (and rules) from a declist *)
fun delAuxVals [] = []
  | delAuxVals (d::ds) =
    case d of Value(i,b) =>
	      let val moreVals =
		      List.find
			  (fn d => case d of Value(i,b) => true
					   | Rule(i,b1,b2) => false)
			  ds
	      in if moreVals
		 then delAuxVals ds (* main val is last by invariant *)
		 else d::ds (* found the main val, terminate *)
	      end
	    | Rule(i,b1,b2) => d :: delAuxVals ds

(* compute next unused Nat identifier *)
fun nextNat smap =
    let val last = List.last smap
	val nat = #1(last)
    in nat2int(nat) + 1 end

(* replace siteIds in a bigraph by contiguous Nats *)
fun traverse b smap =
    case b
     of Wir(w) => (Wir(w), smap) (* done *)
      | Par(b1,b2) =>
	let val (b1',smap') = traverse b1 smap
	    val (b2',smap'') = traverse b2 smap'
	in (Par(b1',b2'), smap'') end
      | Pri(b1,b2) =>
	let val (b1',smap') = traverse b1 smap
	    val (b2',smap'') = traverse b2 smap'
	in (Pri(b1',b2'), smap'') end
      | Com(b1,b2) =>
	let val (b1',smap') = traverse b1 smap
	    val (b2',smap'') = traverse b2 smap'
	in (Com(b1',b2'), smap'') end
      | Emb(b1,b2) =>
	let val (b1',smap') = traverse b1 smap
	    val (b2',smap'') = traverse b2 smap'
	in (Emb(b1',b2'), smap'') end
      | Ten(b1,b2) =>
	let val (b1',smap') = traverse b1 smap
	    val (b2',smap'') = traverse b2 smap'
	in (Ten(b1',b2'), smap'') end
      | Ctrl(i,b,f) => (Ctrl(i,b,f), smap) (* done *)
      | Clo(n,b) => Clo(n, traverse b smap)
      | Abs(n,b) => Abs(n, traverse b smap)
      | Site(i,l) => let val next = nextNat smap
		     in (Site(next,l), (next,i)::smap) end
      | Id(i) => let val id = case i of Var(v) => v2s v
				      | Num(n) => Int.toString n
		 in raise Fail("traverse: Bigraph with an id " ^ id) end
      | Empty => (Empty, smap) (* done *)

(* take declist, return declist of decs with Nat-sites and an updated smap *)
fun numberSites [] smap = ([],smap)
  | numberSites (d::ds) =
    case d
     of Value(i,b) =>
	let val (b',smap') = traverse b smap
	in Value(i,b') :: numberSites ds smap' end
      | Rule(i,b1,b2) =>
	let val (b1',smap') = traverse b1 smap
	    val (b2',smap'') = traverse b2 smap'
	in Rule(i,b1',b2') :: numberSites ds smap'' end

(* toplevel *)
fun prog2bgval ast =
    case ast
     of Prog(signa,declist) =>
	let val rolledList = roll declist
	    val rolledList' = delAuxVals rolledList
	    val smap = []
	    val (nmbrdList,smap') = numberSites rolledList' smap
	    val mainVal = []
	    val rules = []
	    val (b,r) = decs2bgvals nmbrdList mainVal rules signa smap'
	    val mainBgval = b
	    val rules' = r
	in (signa, mainBgval, rules') end
      | _ => raise Fail("prog2bgval: Malformed program")

(* old code
fun prog2bgval ast =
    case ast
     of Prog(signa,declist) =>
	let val (vals,rules,maps) = dec2bgval declist [] [] signa ([],[])
	    (* vars of last val	have been substituted for on the fly *)
	    val mainBgval = getLast vals
	in (signa, mainBgval, rules) end
      | _ => raise Fail("Malformed program")
*)
