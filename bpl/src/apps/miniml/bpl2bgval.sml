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
 Ebbe Elsborg, November 9 2007

 Mapping from BPL abstract syntax tree to BgVal. 
 Implements bpl/bplproject/doc/projects/contextawareness/plato/bpl-bnf.tex

 Algorithm: todo explanation

 Compile: cd <src-dir of BPL-root>; make bpl2bgval
*)

open TextIO;

structure Bpl2bgval = struct

structure BG = BG (structure ErrorHandler = PrintErrorHandler);
structure B = BG.BgVal
structure BgTerm = BG.BgTerm
structure Sugar = BG.Sugar
structure Rule = BG.Rule
structure Control = BG.Control
structure Ion = BG.Ion
structure Name = BG.Name
structure NameSet = BG.NameSet
structure Wiring  = BG.Wiring
structure Link = BG.Link
structure LinkSet = BG.LinkSet
structure Origin = Origin
structure Instantiation = BG.Instantiation
structure Rule = BG.Rule
structure BgBdnf = BG.BgBDNF
structure Interface = BG.Interface

(***** BNF *****)
type id = string (* HERE: Use a 'name' instead? *)
type ctrlid = string (* HERE: Use a 'name' instead? *)
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
		 | Ion of ctrlid * ports * ports
		 | Clo of names * bigraph
		 | Abs of names * bigraph
		 | Conc of names * bigraph (* new, 29/11-07 *)
		 | Site of siteId * namelist
		 | Id of id
		 | Empty
type ctrlkind = Control.kind (* Active | Passive | Atomic *)
datatype ctrldef = Cdef of ctrlid * ctrlkind * nat * nat
type ctrldefs = ctrldef list
datatype dec = Rule of id * bigraph * bigraph
	     | Value of id * bigraph
type decs = dec list
type signatur = ctrldefs
datatype prog = Prog of signatur * decs

(* aux. types and vals *)
type bgval = B.bgval
type sitemap = nat * siteId
type sitemaps = sitemap list
type natmap = nat * nat
type natmaps = natmap list
type map = Instantiation.map
type maps = map list
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

(* PRINTING *)

(* shorthand functions *)
fun s2n s = Name.make s
fun n2s n = Name.unmk n
fun v2n x = Name.make (String.toString x)
fun ion2bg ion = B.Ion info ion

(* interface functions *)
fun getInner b = let val (b,inn,out) = B.unmk b in inn end
fun getOuter b = let val (b,inn,out) = B.unmk b in out end
fun isGround b = let val (bgterm,inner,outer) = B.unmk b
                 in Interface.eq (inner, Interface.zero) end

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
    let val i_parts = Interface.unmk i
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

(* print 'name = bgval' *)
fun prtSimp name bgval =
    print(name ^ "= " ^ B.toString(B.simplify bgval) ^ "\n")

(* print sitemaps *)
fun printSmaps (smaps:sitemaps) =
    List.map (fn x =>
		 let val prtnat = Int.toString(#1(x))
		     val prtid = case #2(x)
				  of Num(n) => Int.toString n
				   | Var(i) => i
		 in print("(" ^ prtnat ^ "," ^ prtid ^ ") ") end
	     )
	     smaps

(* print a map : (int * namelist) * (int * namelist) *)
fun printMap (m:map) =
    let val (i1,l1) = #1(m)
	val (i2,l2) = #2(m)
	fun strList2str l =
	    case l of [] => ""
		    | (n::ns) => Name.unmk(n) ^ (strList2str ns)
	val (i1',l1') = (Int.toString i1, strList2str l1)
	val (i2',l2') = (Int.toString i2, strList2str l2)
    in print("((" ^ i1' ^ "," ^ l1' ^ ")" ^ "," ^
	     "(" ^ i2' ^ "," ^ l2' ^ ")) ")
    end

(* print maps : ( (int * namelist) * (int * namelist) ) list *)
fun printMaps m = List.map printMap m

(* print nmap, i.e. a (nat*nat) list *)
fun printNmap m =
    let val n1 = Int.toString(#1(m))
	val n2 = Int.toString(#2(m))
    in print("(" ^ n1 ^ "," ^ n2 ^ ") ") end

(* print nmaps, i.e. an nmap list, i.e. a (nat*nat) list *)
fun printNmaps [] = []
  | printNmaps (m::ms) = printNmap m :: printNmaps ms

(***** AUXILIARY FUNCTIONS *****)

(* return list of sites of a bigraph *)
fun getSites (b:bigraph) =
    case b of Wir(w) => []
	    | Par(b1,b2) => (getSites b1) @ (getSites b2)
	    | Pri(b1,b2) => (getSites b1) @ (getSites b2)
	    | Com(b1,b2) => (getSites b1) @ (getSites b2)
	    | Emb(b1,b2) => (getSites b1) @ (getSites b2)
	    | Ten(b1,b2) => (getSites b1) @ (getSites b2)
	    | Ion(i,b,f) => [] (* reached atomic control *)
	    | Clo(n,b) => getSites b
	    | Abs(n,b) => getSites b
	    | Conc(n,b) => getSites b
	    | Site(i,l) => [Site(i,l)]
	    | Id(i) => []
	    | Empty => []

(* return list of sites ids (nats) of a bigraph *)
(*
fun getSiteNums (b:bigraph) =
    case b of Wir(w) => []
	    | Par(b1,b2) => (getSiteNums b1) @ (getSiteNums b2)
	    | Pri(b1,b2) => (getSiteNums b1) @ (getSiteNums b2)
	    | Com(b1,b2) => (getSiteNums b1) @ (getSiteNums b2)
	    | Emb(b1,b2) => (getSiteNums b1) @ (getSiteNums b2)
	    | Ten(b1,b2) => (getSiteNums b1) @ (getSiteNums b2)
	    | Ion(i,b,f) => [] (* reached atomic control *)
	    | Clo(n,b) => getSiteNums b
	    | Abs(n,b) => getSiteNums b
	    | Conc(n,b) => getSiteNums b
	    | Site(i,l) =>
	      ( case i (* assume that sites are nats already *)
		 of Num(n) => [n]
		  | _ => raise Fail("getSiteNums: Site with non-nat id\n") )
	    | Id(i) => []
	    | Empty => []
*)
(* lookup first occurence of id and return corresponding key (Site) *)
fun lookupSite [] id = NONE
  | lookupSite (s::m) id =
    case s
     of Site(Num(n),l) => if id=n then SOME(s) else lookupSite m id
      | _ => raise Fail("lookupSite: Unnumbered site\n")

(* lookup first occurence of id and return corresponding key (nat) *)
fun lookupSiteId [] id = NONE
  | lookupSiteId ((n,i)::m) id =
    if id=i then SOME(n) else lookupSiteId m id

(* lookup first occurence of n1 and return corresponding key (nat) *)
fun lookupSnd [] id = NONE
  | lookupSnd ((n1,n2)::m) id = if id=n1 then SOME(n2) else lookupSnd m id

(* convert strings and vars into names *)
fun s2n s = Name.make s
fun v2n x = s2n (String.toString x)

(* convert names and strings to name sets *)
fun nm2nmSet n = NameSet.insert n NameSet.empty
fun s2nmSet s = (nm2nmSet o s2n) s

(* string list to name set list *)
fun strList2nmSetList l = List.map (nm2nmSet o s2n) l

(* make (wiring of) idle outer name *)
fun mkWir1 ovar =
    let val link = Link.make {outer = SOME(v2n ovar),
			      inner = NameSet.empty}
	val linkset = LinkSet.insert link LinkSet.empty
	val wiring = Wiring.make linkset
    in B.Wir info wiring end

(* make wiring from inner an name to an outer name *)
fun mkWir2 ovar ivar =
    let val link = Link.make
		       { outer = SOME(v2n ovar),
			 inner = nm2nmSet (v2n ivar) }
	val linkset = LinkSet.insert link LinkSet.empty
	val wiring = Wiring.make linkset
    in B.Wir info wiring end

(* lookup a control in a signature *)
fun lookupCtrl cid signa =
    let fun loop [] = NONE
	  | loop ((i,k,b,f)::m) = if cid = i then SOME (i,k,b,f) else loop m
    in loop signa end

(* find the last key (nat/int) of an assoc. list *)
fun lastCnt [] = 0
  | lastCnt ((k,v)::m) = #1(List.hd(List.rev((k,v)::m)))

(* raise an error message that a particular control is not in the sig. *)
fun ctrlNotInSig cid =
    raise Fail("Control does not exist in signature: " ^ cid ^ "\n")

(* remove dublets from a list *)
fun rmDubs [] = []
  | rmDubs (x::xs) =
    if List.exists (fn y => y = x) xs then rmDubs xs else x :: rmDubs xs

(* for each name in a set, convert it to the string it originated from *)
(*
fun nmSet2sList set =
    List.map
	(fn n => Name.ekam n) (* recover original string by 'ekam' *)
	(NameSet.list set)
*)
(* partition an smap into two smaps *)
(*
fun partSmap [] acc = (acc,[])
  | partSmap ((n,i)::m) acc p =
    if p=n then (rev acc, (n,i)::m)
    else partSmap m ((n,i) :: acc) p
*)

(* compare two lists of namesets for equality *)
(*
fun cmprNSlists [] [] = true
  | cmprNSlists (n::ns) [] = false
  | cmprNSlists [] (n::ns) = false
  | cmprNSlists (n::ns) (n'::ns') =
    if NameSet.eq n n' then cmprNSlists ns ns' else false
*)
(* check composability of two interfaces; 'inner o outer' *)
(*
fun chkIcomp inner outer = 
    let val _ = print "chkIcomp called...\n"
	val i_glob = Interface.glob inner
	val i_loc = Interface.loc inner
	val i_width = Interface.width inner
	val _ = print("i_width: " ^ Int.toString i_width ^ "\n")
	val o_glob = Interface.glob outer
	val o_loc = Interface.loc outer
	val o_width = Interface.width outer
	val _ = print("o_width: " ^ Int.toString o_width ^ "\n")
	val _ = printIfaces " " inner outer
    in if i_width = o_width
       then if NameSet.eq i_glob o_glob
	    then if cmprNSlists i_loc o_loc
		 then true
		 else raise Fail("chkIcomp: Com/Emb " ^
				 "has local name mismatch\n")
	    else raise Fail("chkIcomp: Com/Emb " ^
			    "has global name mismatch\n")
       else raise Fail("chkIcomp: Com/Emb has width mismatch\n")
    end
*)
(* some error functions used by makeMaps in calcMaps *)
fun err1 n = "makeMaps: Site " ^ Int.toString(n) ^
	     " has no LHS counterpart\n"
fun err2 n = "makeMaps: Shouldn't happen... " ^ Int.toString(n) ^ "\n"
val err3 = "makeMaps: Unnumbered site\n"

(* peel off options from second components of generated auxList *)
(*
fun peel (assocOptList:(nat*nat option) list) =
    case assocOptList
     of [] => []
      | ((n,x)::m) => ( case x
			 of NONE => raise Fail(err1 n)
			  | SOME(n') => (n,n') :: peel m )
*)
(* peel off Namelist constructor *)
fun peelNamelist l =
    case l of Namelist(nms) => List.map (fn s => s2n s) nms

(* calculate 'maps', i.e. a 'map list', for instantiation *)
(*
fun calcMaps (b1:bigraph) (b2:bigraph) (smaps:sitemaps) =
    let val _ = print "calcMaps called...\n"
	(* for each site in b2, find target site in b1 if it exists *)
	val (smap1,smap2) = ...
	val _ = print "smap1 = "
	val _ = printSmaps smap1
	val _ = print "\n"
	val _ = print "smap2 = "
	val _ = printSmaps smap2
	val _ = print "\n"
	val auxList = (*:(nat*nat option) list*)
	    List.map
		(fn s => let val natkey = #1(s)
			     val site_id = #2(s)
			     val opt = lookupSiteId smap1 site_id
			 in (natkey,opt) end)
		smap2
	val pldList = (*:(nat*nat) list*) peel auxList
	(* generate 'maps'; (int * name list) * (int * name list) list *)
	val sites1 = getSites b1
	val sites2 = getSites b2
*)
fun makeMaps slist1 slist2 (nmaps:natmaps) =
    List.map
	( fn s =>
	     ( case s
		of Site(Num(n),l) =>
		   let val b1siteId = lookupSnd nmaps n
		       val b1site =
			   case b1siteId
			    of SOME(n') => (* exists LHS site id *)
			       ( case lookupSite slist1 n'
				  of SOME(s) => s (* id |-> site *)
				   | NONE => raise Fail(err2 n') )
			     | NONE => raise Fail(err1 n)
		       val (n1,l1) =
			   case b1site
			    of Site(Num(n'),l') =>
			       (n', peelNamelist l')
			     | _ => raise Fail(err3)
		       val (n2,l2) = (n, peelNamelist l)
		   in ((n2,l2), (n1,l1)) end
		 | _ => raise Fail(err3) )
	)
	slist2
(*
	val maps = makeMaps sites1 sites2 pldList
    in maps end
*)
(* compose two bigraphs checking their interfaces *)
(*
fun compBigs b1 b2 =
    let val b1' = big2bgval b1 signa
	val b2' = big2bgval b2 signa
	(* requires:
	 - width(dom(b1')) = width(cod(b2'))
	 - glob(dom(b1')) \subseteq glob(cod(b2'))
	 *)
	val loc_diff = 

	val composable = chkIcomp (B.innerface b1') (B.outerface b2')
	(* we only get here if the 'composable' flag is true *)	

	val b2'o_glob = (Interface.glob o B.outerface) b2'
	val id_b2glob = (Sugar.idw o nmSet2sList) b2'o_glob(*s2n*)
    in (b1' tt id_b2glob) oo b2' end (* just add the names,  *)
*)
(***** TRANSLATION *****)

(* short-hand funs *)
fun abs (nameset,prime) = B.Abs info (nameset,prime)
fun con nameset = B.Con info nameset
fun par bgvallist = B.Par info bgvallist
fun com (b1,b2) = B.Com' info (b1,b2)
fun conc (nameset,b) = (con nameset) oo b

(* transform a bigraph into a bgval *)
fun big2bgval (ast:bigraph) signa =
    (* HERE...insert also id_n ? *)
    case ast
     of Wir(w) =>
	let val _ = print "big2bgval: Wir...\n"
	    fun w2bgval wire =
		case wire
		 of Global(out,inn) => mkWir2 out inn
		  | Local(out,inn) =>
		    (* inn must be local to obey the scope rule *)
		    let val glob_inn = (con o nm2nmSet o s2n) inn
			val wir_perm = (mkWir2 out inn) tt Sugar.idp(1)
			val comp_wir = wir_perm oo glob_inn
			val loc2loc = abs (s2nmSet out, comp_wir)
		    in loc2loc end
		  | IdleG(x) => mkWir1 x
		  | IdleL(x) => abs (s2nmSet x, (mkWir1 x) tt Sugar.idp(1))
	    val wires = List.map w2bgval w
	    val bgval = par wires
	in bgval before printIfaces "Wir"
				    (B.innerface bgval)
				    (B.outerface bgval)
	end
      | Par(b1,b2) =>
	let val b1' = big2bgval b1 signa
	    val b2' = big2bgval b2 signa
	in b1' || b2' end
      | Pri(b1,b2) =>
	let val _ = print "big2bgval: Pri...\n"
	    val b1' = big2bgval b1 signa
	    val b2' = big2bgval b2 signa
	in (b1' pp b2') before printIfaces "Pri"
					   (B.innerface(b1' pp b2'))
					   (B.outerface(b1' pp b2'))
	end
      | Com(b1,b2) =>
	let val _ = print "big2bgval: Com...\n"
	    val b1' = big2bgval b1 signa
	    val b2' = big2bgval b2 signa
	in com(b1',b2') before printIfaces "Emb"
					   (B.innerface(com(b1',b2')))
					   (B.outerface(com(b1',b2')))
	end (* (b1' || id) o b2' *)
      | Emb(b1,b2) =>
	let val _ = print "big2bgval: Emb...\n"
	    val b1' = big2bgval b1 signa
	    val b2' = big2bgval b2 signa
	(* (b1' || id) o b2', requires:
	 - width(dom(b1')) = width(cod(b2'))
	 - glob(dom(b1')) \subseteq glob(cod(b2'))
	 *)
	in com(b1',b2') before printIfaces "Emb"
					   (B.innerface(com(b1',b2')))
					   (B.outerface(com(b1',b2')))
	end
      | Ten(b1,b2) =>
	let val _ = print "big2bgval: Ten...\n"
	    val b1' = big2bgval b1 signa
	    val b2' = big2bgval b2 signa
	in (b1' tt b2') before printIfaces "Ten"
					 (B.innerface(b1' tt b2'))
					 (B.outerface(b1' tt b2'))
	end
      | Ion(cid,bports,fports) =>
        let val _ = print "big2bgval: Ion...\n"
	    val boundnames = strList2nmSetList bports
	    val freenames = List.map s2n fports
	    val bSz = List.length bports
	    val fSz = List.length fports
	in case lookupCtrl cid signa
	    of SOME((i,k,b,f)) =>
	       if bSz = b andalso fSz = f
	       then let val ion =
			    Ion.make {ctrl = Control.make(cid,k,bSz,fSz),
				      free = freenames,
				      bound = boundnames}
			val bgval = B.Ion info ion
		    in if k = Control.Atomic
		       then if bSz = 0
			    then (bgval oo barren) (* atomic control *)
				 before printIfaces
					    "Ctrl"
					    (B.innerface(bgval oo barren))
					    (B.outerface(bgval oo barren))
			    else raise Fail("big2bgval: Atomic control "
					    ^ i ^ " with bound names\n")
		       else let val _ = printIfaces
					    "Ion"
					    (B.innerface bgval)
					    (B.outerface bgval)
			    in bgval end
		    end
	       else raise Fail("big2bgval: Control " ^ i ^ 
			       " used in non-accordance with signature\n")
	     | NONE => ctrlNotInSig cid
	end
      | Clo(nms,b) =>
	let val bgval1 = Sugar.-//nms(*(List.map s2n nms)*)
	    val bgval2 = big2bgval b signa
	in bgval1 oo bgval2 end
      | Abs(nms,b) =>
	let val _ = print "big2bgval: Abs...\n"
	    val nmSetList = strList2nmSetList (rmDubs nms)
	    val union = fn (nmset,acc) => NameSet.union nmset acc
	    val nmSet = List.foldr union NameSet.empty nmSetList
	    val b' = big2bgval b signa
	    val abst = abs (nmSet, b')
	    val _ = printIfaces "Abs" (B.innerface abst) (B.outerface abst)
	in abst end
      | Conc(nms,b) =>
	let val _ = print "big2bgval: Conc...\n"
	    val nmSetList = strList2nmSetList (rmDubs nms)
	    val union = fn (nmset,acc) => NameSet.union nmset acc
	    val nmSet = List.foldr union NameSet.empty nmSetList
	    val b' = big2bgval b signa
	    val conc = conc (nmSet, b')
	    val _ = printIfaces "Conc" (B.innerface conc) (B.outerface conc)
	in conc end
      | Site(i,l) =>
	let val _ = print "big2bgval: Site...\n"
	    (* (/X tt id_1) o `[X]` *)
	    val X' = case l of Namelist(sl) => rmDubs sl
	    val X = NameSet.fromList(List.map s2n X')
	    val site = (*(Sugar.-//X') oo*) abs(X,con X)
	    val _ = printIfaces "Site" (B.innerface site) (B.outerface site)
	in site end
      | Id(i) => raise Fail("big2bgval: Unbound identifier: " ^ i ^ "\n")
      | Empty => let val _ = print "big2bgval: Empty...\n"
		 in barren end

(* take a rule and a natmap and produce a rule of bgvals with inst *)
fun rule2bgval (p:dec*natmaps) signa =
    case p
     of (Rule(i,b1,b2), nmaps) =>
	let val b1sites = getSites b1
	    val b2sites = getSites b2
	    val maplist = makeMaps b1sites b2sites nmaps
	    val b1' = big2bgval b1 signa
	    val b2' = big2bgval b2 signa
	    val b2'' = (BgBdnf.regularize o BgBdnf.make) b2'
	    val ins = Instantiation.make { I = B.innerface b1',
					   J = B.innerface b2',
					   maps = maplist }
	in Rule.make { info = Origin.unknown_origin,
		       inst = ins,
		       name = i,
		       react = b1',
		       redex = b2''}
	end
      | _ => raise Fail("rule2bgval: Encountered a Value\n")

(* translate rules with natmaps into rules of bgvals with insts *)
fun rules2bgvals (rules_natmaps:(dec*natmaps) list) signa =
    case rules_natmaps
     of [] => []
      | (p::ps) => rule2bgval p signa :: rules2bgvals ps signa

(* take numbered declist, output main bgval and rules list (bgval pairs) *)
(*
fun decs2bgvals decls mainVal rules signa smap =
    case decls
     of [] => (mainVal, rules)
      | (d::ds) =>
	( case d
	   of Value(i,b) =>
	      let val _ = print "decs2bgvals...value branch\n"
		  val mainBgval = big2bgval b signa
	      in decs2bgvals ds mainBgval rules signa smap end
	    | Rule(i,b1,b2) =>
	      let val _ = print "decs2bgvals...rule branch\n"
		  (*
		  val b1sites = getSiteNums b1
		  val _ = print("length(b1sites) = " ^ Int.toString(List.length b1sites) ^ "\n")
	      
		  val pivot = if List.length(b1sites) > 0
			      then (List.last b1sites) + 1
			      else 0 (* yields empty maplist *)
		   *)
		  val maplist = calcMaps b1 b2 (*pivot*)smap1 smap2
		  val _ = print "length(maplist): "
		  val _ = print(Int.toString(List.length maplist) ^ "\n")
		  val _ = printMaps maplist
		  val _ = print "\n"
		  val b1' = big2bgval b1 signa
		  val b2' = big2bgval b2 signa
		  val b2'' = (BgBdnf.regularize o BgBdnf.make) b2'
		  val _ = print("b1': " ^ 
				(Interface.toString o B.innerface) b1'
				^ " -> ...\n")
		  val _ = print("b2': " ^ 
				(Interface.toString o B.innerface) b2'
				^ " -> ...\n")
		  val ins = Instantiation.make { I = B.innerface b1',
						 J = B.innerface b2',
						 maps = maplist }
		      handle Instantiation.InvalidSiteNumber
				 (map,int,iface) => raise Fail "yikes\n"
(*			     ( printMap map
			     ; print(Int.toString(int))
			     ; printIface iface
			     ; Instantiation.make' { I = B.innerface b1',
						    J = B.innerface b2'})
*)
		  val rule = Rule.make { info = Origin.unknown_origin,
					 inst = ins,
					 name = i,
					 react = b1',
					 redex = b2''}
		  val rules' = rule :: rules
	      in decs2bgvals ds mainVal rules' signa smap1 smap2 end
	)
*)
(* substitute a bigraph b1 into a bigraph b2, recursively *)
fun sub (i1,b1) b2 =
    case b2 of Wir(w) => Wir(w)
	    | Par(b,b') => Par(sub (i1,b1) b, sub (i1,b1) b')
	    | Pri(b,b') => Pri(sub (i1,b1) b, sub (i1,b1) b')
	    | Com(b,b') => Com(sub (i1,b1) b, sub (i1,b1) b')
	    | Emb(b,b') => Emb(sub (i1,b1) b, sub (i1,b1) b')
	    | Ten(b,b') => Ten(sub (i1,b1) b, sub (i1,b1) b')
	    | Ion(i,b,f) => Ion(i,b,f) (* reached atomic control *)
	    | Clo(n,b) => Clo(n, sub (i1,b1) b)
	    | Abs(n,b) => Abs(n, sub (i1,b1) b)
	    | Conc(n,b) => Conc(n, sub (i1,b1) b)
	    | Site(i,l) => Site(i,l)
	    | Id(i) =>
	      if i = i1 then b1 (* assumes that b1 is already rolled *)
	      else Id(i)
	    | Empty => Empty

(* substitute a bigraph b into every following element in a decl list *)
fun subList [] = []
  | subList (d::ds) =
    case d
     of Rule(i,b1,b2) => d :: subList ds (* rules not subst. into decls *)
      | Value(i,b) =>
	let val newlist = 
		List.map
		    (fn d' =>
			case d'
			 of Value(i',b') => Value(i', sub (i,b) b')
			  | Rule(i',b1',b2')
			    => Rule(i', sub (i,b) b1', sub (i,b) b2'))
		    ds
	in d :: newlist end

(* roll a declist (vals/rules) *)
fun roll [] = []
  | roll (d::ds) =
    if ds = [] then [d] (* nothing to do for last element *)
    else let val newlist = subList (d::ds)
	     val head = List.hd newlist
	     val tail = List.tl newlist
	 in head :: roll tail end

(* delete all except the main val (and rules) from a declist *)
fun delAuxVals [] = []
  | delAuxVals (d::ds) =
    case d of Value(i,b) =>
	      let val moreVals =
		      List.exists
			  (fn d => case d of Value(i,b) => true
					   | Rule(i,b1,b2) => false)
			  ds
	      in if moreVals
		 then delAuxVals ds (* cont., main val is last by invar. *)
		 else d::ds (* found the main val, terminate *)
	      end
	    | Rule(i,b1,b2) => d :: delAuxVals ds

(* compute next unused nat site-identifier of sitemaps *)
fun nextNat (smaps:sitemaps) =
    case smaps
     of [] => 0
      | (x::xs) => #1(List.last smaps) + 1

(* compute next unused nat site-identifier of natmaps *)
fun nextNat' (nmaps:natmaps) =
    case nmaps
     of [] => 0
      | (x::xs) => #1(List.last nmaps) + 1

(* replace siteIds in redex by contiguous nats, create smaps for reactum *)
fun traverse big (smaps:sitemaps) =
    case big
     of Wir(w) => (Wir(w), smaps) (* done *)
      | Par(b1,b2) =>
	let val (b1',smaps') = traverse b1 smaps
	    val (b2',smaps'') = traverse b2 smaps'
	in (Par(b1',b2'), smaps'') end
      | Pri(b1,b2) =>
	let val (b1',smaps') = traverse b1 smaps
	    val (b2',smaps'') = traverse b2 smaps'
	in (Pri(b1',b2'), smaps'') end
      | Com(b1,b2) =>
	let val (b1',smaps') = traverse b1 smaps
	    val (b2',smaps'') = traverse b2 smaps'
	in (Com(b1',b2'), smaps'') end
      | Emb(b1,b2) =>
	let val (b1',smaps') = traverse b1 smaps
	    val (b2',smaps'') = traverse b2 smaps'
	in (Emb(b1',b2'), smaps'') end
      | Ten(b1,b2) =>
	let val (b1',smaps') = traverse b1 smaps
	    val (b2',smaps'') = traverse b2 smaps'
	in (Ten(b1',b2'), smaps'') end
      | Ion(i,b,f) => (Ion(i,b,f), smaps) (* done, reached atomic control *)
      | Clo(n,b) =>
	let val (b',smaps') = traverse b smaps
	in (Clo(n,b'), smaps') end
      | Abs(n,b) =>
	let val (b',smaps') = traverse b smaps
	in (Abs(n,b'), smaps') end
      | Conc(n,b) =>
	let val (b',smaps') = traverse b smaps
	in (Conc(n,b'), smaps') end
      | Site(i,l) =>
	let val next = nextNat smaps
	in (Site(Num(next),l), smaps@[(next,i)]) end
      | Id(i) => raise Fail("traverse: Bigraph with an id " ^ i ^ "\n")
      | Empty => (Empty, smaps) (* done *)

(* error value used by traverse' *)
val err_rhs = "traverse': RHS site id without corresponding LHS id"

(* replace siteIds in react. by contig. nats wrt. smaps, create natmaps *)
fun traverse' big (smaps:sitemaps) (nmaps:natmaps) =
    case big
     of Wir(w) => (Wir(w), nmaps) (* done *)
      | Par(b1,b2) =>
	let val (b1',nmaps') = traverse' b1 smaps nmaps
	    val (b2',nmaps'') = traverse' b2 smaps nmaps'
	in (Par(b1',b2'), nmaps'') end
      | Pri(b1,b2) =>
	let val (b1',nmaps') = traverse' b1 smaps nmaps
	    val (b2',nmaps'') = traverse' b2 smaps nmaps'
	in (Pri(b1',b2'), nmaps'') end
      | Com(b1,b2) =>
	let val (b1',nmaps') = traverse' b1 smaps nmaps
	    val (b2',nmaps'') = traverse' b2 smaps nmaps'
	in (Com(b1',b2'), nmaps'') end
      | Emb(b1,b2) =>
	let val (b1',nmaps') = traverse' b1 smaps nmaps
	    val (b2',nmaps'') = traverse' b2 smaps nmaps'
	in (Emb(b1',b2'), nmaps'') end
      | Ten(b1,b2) =>
	let val (b1',nmaps') = traverse' b1 smaps nmaps
	    val (b2',nmaps'') = traverse' b2 smaps nmaps'
	in (Ten(b1',b2'), nmaps'') end
      | Ion(i,b,f) => (Ion(i,b,f), nmaps) (* done, reached atomic control *)
      | Clo(n,b) =>
	let val (b',nmaps') = traverse' b smaps nmaps
	in (Clo(n,b'), nmaps') end
      | Abs(n,b) =>
	let val (b',nmaps') = traverse' b smaps nmaps
	in (Abs(n,b'), nmaps') end
      | Conc(n,b) =>
	let val (b',nmaps') = traverse' b smaps nmaps
	in (Conc(n,b'), nmaps') end
      | Site(i,l) =>
	let val lhsSiteNum =
		case lookupSiteId smaps i
		 of NONE => raise Fail(err_rhs)
		  | SOME(n) => n
			       before print("lhsSiteNum = " ^
					    Int.toString(n) ^ "\n")
	    val next = nextNat' nmaps
	    val _ = print "nmaps = "
	    val _ = printNmaps nmaps
	    val _ = print "\n"
	    val _ = print "nmaps' = "
	    val _ = printNmaps(nmaps@[(next,lhsSiteNum)])
	    val _ = print "\n"
	in (Site(Num(next),l), nmaps@[(next,lhsSiteNum)]) end
      | Id(i) => raise Fail("traverse': Bigraph with an id " ^ i ^ "\n")
      | Empty => (Empty, nmaps) (* done *)

(* number the sites of a rule and generate a natmap *)
fun numberSites rule =
    case rule
     of Rule(i,b1,b2) =>
	let val _ = print "numberSites called...\n"
	    val smaps = []
	    val nmaps = []
	    val (b1',smaps') = traverse b1 smaps
	    val _ = print "b1' sites = "
	    val _ = List.map
			(fn n => print(Int.toString(n)))
			(List.map (fn Site(Num(n),l) => n) (getSites b1'))
	    val _ = print "\n"
	    val _ = print "smaps' = "
	    val _ = printSmaps smaps'
	    val _ = print "\n"
	    val (b2',nmaps') = traverse' b2 smaps' nmaps
	    val _ = print "b2' sites = "
	    val _ = List.map
			(fn n => print(Int.toString(n)))
			(List.map (fn Site(Num(n),l) => n) (getSites b2'))
	    val _ = print "\n"
	    val _ = print "nmaps = "
	    val _ = printNmaps nmaps'
	    val _ = print "\n"
	in (Rule(i,b1',b2'),nmaps') end

(* take ctrldef list and peel off Cdef constructor from the 4-tuples *)
fun peelCdef [] = []
  | peelCdef (c::cs) =
    case c of Cdef(cid,ck,bp,fp) => (cid,ck,bp,fp) :: peelCdef cs

(* toplevel *)
fun prog2bgval ast =
    case ast
     of Prog(signa,declist) =>
	let val rolledList = roll declist
	    val rolledList' = delAuxVals rolledList
	    val (mainVal,rules) =
		List.partition
		    (fn d => case d of Value(i,b) => true
				     | _ => false)
		    rolledList'
(*
	    val _ = print("length(mainVal) = " ^
			  Int.toString(List.length(mainVal)) ^ "\n")
	    val _ = print("length(rules) = " ^
			  Int.toString(List.length(rules)) ^ "\n")
*)
	    (* number sites in rules and generate natmaps: (nat*nat) list *)
	    val rules_nmaps = List.map numberSites rules
	    val signa' = peelCdef signa
	    val mainVal' = case mainVal
			    of [Value(i,b)] => b (* singleton by invar. *)
	    (* make bgvals *)
	    val mainBgval = big2bgval mainVal' signa'
	    val rules' = rules2bgvals rules_nmaps signa'
	in (signa', mainBgval, rules') end

end (* structure Bpl2bgval *)
