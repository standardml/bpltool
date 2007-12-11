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
 Author: Ebbe Elsborg (elsborg@itu.dk)

 Mapping from BPL abstract syntax tree to BgVal. 
 Implements bpl/bplproject/doc/projects/contextawareness/plato/bpl-bnf.tex

 Algorithm.
   Input: A list of decls (Values and Rules) and a signature.
   Output: A signature, a main bgval (the state of the BRS), and
	   a set of rules of bgvals with instantiations.
   1. Roll the Values into one main Value -- the last one found.
   2. Delete the used Values.
   3. Rules are on form (identifier,bigraph1,bigraph2). The site
      identifiers (in the bigraphs) can be either a variable (string) or a
      number (nat). For each LHS bigraph b1, number the sites from 0,
      depth-first, and create a 'sitemap' from "old id" to "new id".
      Pass this sitemap to RHS bigraph b2 and insert nat identifiers acc.
      to this sitemap, and also return a 'natmap' from b2 sites to b1
      sites, which is used to generate an instantiation later.
   4. Make the main Value into a bgval by calling the function 'big2bgval'.
   5. Make the Rules into rules of the BPL Rule functor.

 Comments:
   - There is at least one Value in the input, and exactly one main bgval,
     i.e there is just one "state" of the system (BRS).
   - Ions are used to represent both ions, molecules, and atoms. Atoms are
     just special cases and are constructed by "filling the hole" using
     Emb/Com in the AST.
   - Sites go from local names to local names (and have widths 1) because
     they are usually used in rules, where they are typically bound in
     redeces.

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
		 | Ion of ctrlid * ports * ports
		 | Clo of names * bigraph
		 | Abs of names * bigraph
		 | Conc of names * bigraph
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

(* CONVERSION FUNCTIONS *)

(* convert strings and vars into names, and names back into strings *)
fun s2n s = Name.make s
fun v2n x = s2n (String.toString x)
fun n2s n = Name.unmk n

(* convert names and strings to name sets *)
fun nm2nmSet n = NameSet.insert n NameSet.empty
fun s2nmSet s = (nm2nmSet o s2n) s

(* PRINTING *)

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
fun printNmap (m:natmap) =
    let val n1 = Int.toString(#1(m))
	val n2 = Int.toString(#2(m))
    in print("(" ^ n1 ^ "," ^ n2 ^ ") ") end

(* print nmaps, i.e. an nmap list, i.e. a (nat*nat) list *)
fun printNmaps [] = []
  | printNmaps (m::ms) = printNmap m :: printNmaps ms

(***** AUXILIARY FUNCTIONS *****)

(* return list of sites of a bigraph *)
fun getSites b =
    case b of Wir(w) => []
	    | Par(b1,b2) => (getSites b1) @ (getSites b2)
	    | Pri(b1,b2) => (getSites b1) @ (getSites b2)
	    | Com(b1,b2) => (getSites b1) @ (getSites b2)
	    | Emb(b1,b2) => (getSites b1) @ (getSites b2)
	    | Ten(b1,b2) => (getSites b1) @ (getSites b2)
	    | Ion(i,b,f) => [] (* reached atom *)
	    | Clo(n,b) => getSites b
	    | Abs(n,b) => getSites b
	    | Conc(n,b) => getSites b
	    | Site(i,l) => [Site(i,l)]
	    | Id(i) => []
	    | Empty => []

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

(* raise an error message that a particular control is not in the sig. *)
fun ctrlNotInSig cid =
    raise Fail("Control does not exist in signature: " ^ cid ^ "\n")

(* remove dublets from a list *)
fun rmDubs [] = []
  | rmDubs (x::xs) =
    if List.exists (fn y => y = x) xs then rmDubs xs else x :: rmDubs xs

(* peel off Namelist constructor *)
fun peelNamelist l =
    case l of Namelist(nms) => List.map (fn s => s2n s) nms

(* in: nat-ID-site lists and natmaps, out: relate Sites *)
fun makeMaps slist1 slist2 (nmaps:natmaps) =
    let fun err1 n = "makeMaps: Site " ^ Int.toString(n) ^
		     " has no LHS counterpart\n"
	fun err2 n = "makeMaps: Shouldn't happen... "
		     ^ Int.toString(n) ^ "\n"
	val err3 = "makeMaps: Unnumbered site\n"
    in List.map
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
    end

(***** TRANSLATION *****)

(* short-hand funs used by big2bgval *)
fun abs (nameset,prime) = B.Abs info (nameset,prime)
fun con nameset = B.Con info nameset
fun par bgvallist = B.Par info bgvallist
fun com (b1,b2) = B.Com' info (b1,b2)
fun conc (nameset,b) = (con nameset) oo b

(* transform a bigraph -- abstract syntax tree -- into a bgval *)
fun big2bgval ast signa =
    (* translate into bgval, and wire through surplus global names,
       but what about local names? *)
    case ast
     of Wir(w) =>
	let fun w2bgval wire =
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
	in bgval end
      | Par(b1,b2) =>
	let val b1' = big2bgval b1 signa
	    val b2' = big2bgval b2 signa
	in b1' || b2' end
      | Pri(b1,b2) =>
	let val b1' = big2bgval b1 signa
	    val b2' = big2bgval b2 signa
	in (b1' pp b2')	end
      | Com(b1,b2) =>
	let val b1' = big2bgval b1 signa
	    val b2' = big2bgval b2 signa
	(* (b1' || id) o b2', requires:
	 - width(dom(b1')) = width(cod(b2'))
	 - glob(dom(b1')) \subseteq glob(cod(b2'))
	 *)
	in com(b1',b2')	end
      | Emb(b1,b2) =>
	let val b1' = big2bgval b1 signa
	    val b2' = big2bgval b2 signa
	(* (b1' || id) o b2', requires:
	 - width(dom(b1')) = width(cod(b2'))
	 - glob(dom(b1')) \subseteq glob(cod(b2'))
	 *)
	in com(b1',b2')	end
      | Ten(b1,b2) =>
	let val b1' = big2bgval b1 signa
	    val b2' = big2bgval b2 signa
	in (b1' tt b2')	end
      | Ion(cid,bports,fports) =>
        let val boundnames = strList2nmSetList bports
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
			    then (bgval oo barren) (* make an atom *)
			    else raise Fail("big2bgval: Atom "
					    ^ i ^ " with bound names\n")
		       else bgval
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
	let val nmSetList = strList2nmSetList (rmDubs nms)
	    val union = fn (nmset,acc) => NameSet.union nmset acc
	    val nmSet = List.foldr union NameSet.empty nmSetList
	    val b' = big2bgval b signa
	    val abst = abs (nmSet, b')
	in abst end
      | Conc(nms,b) =>
	let val nmSetList = strList2nmSetList (rmDubs nms)
	    val union = fn (nmset,acc) => NameSet.union nmset acc
	    val nmSet = List.foldr union NameSet.empty nmSetList
	    val b' = big2bgval b signa
	    val conc = conc (nmSet, b')
	in conc end
      | Site(i,l) => (* ({X}) o `[X]`, i.e. id_<1,({X}),X> *)
	let val X' = case l of Namelist(sl) => rmDubs sl
	    val X = NameSet.fromList(List.map s2n X')
	    val site = abs(X,con X)
	in site end
      | Id(i) => raise Fail("big2bgval: Unbound identifier: " ^ i ^ "\n")
      | Empty => barren

(* take a rule with natmaps and produce a rule of bgvals with inst. *)
fun rule2bgval p signa =
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
fun rules2bgvals rules_nmaps signa =
    case rules_nmaps
     of [] => []
      | (p::ps) => rule2bgval p signa :: rules2bgvals ps signa

(* substitute a bigraph b1 into a bigraph b2, recursively *)
fun sub (i1,b1) b2 =
    case b2 of Wir(w) => Wir(w)
	    | Par(b,b') => Par(sub (i1,b1) b, sub (i1,b1) b')
	    | Pri(b,b') => Pri(sub (i1,b1) b, sub (i1,b1) b')
	    | Com(b,b') => Com(sub (i1,b1) b, sub (i1,b1) b')
	    | Emb(b,b') => Emb(sub (i1,b1) b, sub (i1,b1) b')
	    | Ten(b,b') => Ten(sub (i1,b1) b, sub (i1,b1) b')
	    | Ion(i,b,f) => Ion(i,b,f) (* reached atom *)
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
fun nextNat smaps =
    case smaps
     of [] => 0
      | (x::xs) => #1(List.last smaps) + 1

(* replace siteIds in redex by contiguous nats, create smaps for reactum *)
fun traverse big smaps =
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
      | Ion(i,b,f) => (Ion(i,b,f), smaps) (* reached atom *)
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

(* compute next unused nat site-identifier of natmaps *)
fun nextNat' nmaps =
    case nmaps
     of [] => 0
      | (x::xs) => #1(List.last nmaps) + 1

(* replace siteIds in react. by contig. nats wrt. smaps, create natmaps *)
fun traverse' big smaps nmaps =
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
      | Ion(i,b,f) => (Ion(i,b,f), nmaps) (* reached atom *)
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
	    val next = nextNat' nmaps
	in (Site(Num(next),l), nmaps@[(next,lhsSiteNum)]) end
      | Id(i) => raise Fail("traverse': Bigraph with an id " ^ i ^ "\n")
      | Empty => (Empty, nmaps) (* done *)

(* number the sites of a rule and generate natmaps *)
fun numberSites rule =
    case rule
     of Rule(i,b1,b2) =>
	let val smaps = []
	    val nmaps = []
	    val (b1',smaps') = traverse b1 smaps
	    val (b2',nmaps') = traverse' b2 smaps' nmaps
	in (Rule(i,b1',b2'),nmaps') end

(* take ctrldef list and peel off Cdef constructor from the 4-tuples *)
fun peelCdef [] = []
  | peelCdef (c::cs) =
    case c of Cdef(cid,ck,bp,fp) => (cid,ck,bp,fp) :: peelCdef cs

(* toplevel *)
fun prog2bgval ast =
    case ast
     of Prog(signa,declist) =>
	if List.exists (fn d => case d
				 of Value(i,b) => true
				  | _ => false)
		       declist
	then
	    let val rolledList = roll declist
		val rolledList' = delAuxVals rolledList
		val (mainVal,rules) =
		    List.partition
			(fn d => case d of Value(i,b) => true
					 | _ => false)
			rolledList'
		(* number sites in rules and generate natmaps *)
		val rules_nmaps = List.map numberSites rules
		val signa' = peelCdef signa
		val mainVal' =
		    case mainVal
		     of [Value(i,b)] => b (* singleton by invar. *)
		(* make bgvals *)
		val mainBgval = big2bgval mainVal' signa'
		val rules' = rules2bgvals rules_nmaps signa'
	    in (signa', mainBgval, rules') end
	else raise Fail("prog2bgval: There are no Values\n")

end (* structure Bpl2bgval *)
