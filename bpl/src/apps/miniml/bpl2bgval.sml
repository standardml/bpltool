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
*)

open TextIO;

structure BG = BG (structure ErrorHandler = PrintErrorHandler);
structure B = BG.BgVal
structure Sugar = BG.Sugar
structure Rule = BG.Rule
structure Control = BG.Control
structure Ion = BG.Ion
structure Name = BG.Name
structure Wiring  = BG.Wiring
structure NameSet = BG.NameSet
structure LinkSet = BG.LinkSet

(*
structure P = BG.Permutation
structure Link    = BG.Link
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
type namelist = Namelist of names
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
type inst = (nat * nat) list (* 21/8-07*)
type rule = bigraph * bigraph * inst (* 21/8-07 *)
datatype ctrlkind = Active
		  | Passive
		  | Atomic
datatype ctrldef = Cdef of ctrlid * ctrlkind * nat * nat
datatype ctrldefs = ctrldef list
datatype dec = Rule of id * rule
	     | Value of id * bigraph
datatype decs = dec list
type signatur = ctrldefs
datatype prog = Prog of signatur * decs

val barren = Sugar.<-> (* barren root *)
val info = BG.Info.noinfo

(* OPERATORS *)
infixr || (* parallel product *)
infixr pp (* prime product *)
infixr tt (* tensor product *)
infixr oo (* composition *)
fun (b1:bgval) || (b2:bgval) = Sugar.|| (b1,b2)
fun (b1:bgval) pp (b2:bgval) = Sugar.'|' (b1,b2)
fun (b1:bgval) tt (b2:bgval) = Sugar.* (b1,b2)
fun (b1:bgval) oo (b2:bgval) = Sugar.o (b1,b2)

(***** AUXILIARY FUNCTIONS *****)
(*
fun getSites1 b numbered =
    case b of Wir => []
	    | Par(b1,b2) => (getSites b1) @ (getSites b2)
	    | Pri(b1,b2) => (getSites b1) @ (getSites b2)
	    | Com(b1,b2) => (getSites b1) @ (getSites b2)
	    | Emb(b1,b2) => (getSites b1) @ (getSites b2)
	    | Ten(b1,b2) => (getSites b1) @ (getSites b2)
	    | Ctrl(i) => []
	    | CtrlB(c,p) => []
	    | CtrlF(c,p) => []
	    | CtrlBF(c,p1,p2) => []
	    | Clo(n,b) => getSites b
	    | Abs(n,b) => getSites b
	    | Site(n) => if numbered then [n] else []
	    | Sitep(n,l) => if numbered then [n] else []
	    | Nsite(s) => if numbered then [] else [s]
	    | Nsitep(s,l) => if numbered then [] else [s]
	    | id => []
	    | Empty => []

fun makeInst s1 s2 =
    let val s1nums = #1(s1)
	val s1nams = #2(s1)
	val s2nums = #1(s2)
	val s2nams = #2(s2)
    in todo
    end

fun calcInst b1 b2 =
    let val s1 = (getSites b1 true, getSites b1 false)
	val s2 = (getSites b2 true, getSites b2 false)
    in makeInst s1 s2 end
*)

fun s2n s = Name.make s

fun v2n x = Name.make (String.toString x)

fun mkWir1 ovar =
    Wiring.make(LinkSet.insert
		    LinkSet.empty
		    (Link.make {outer = SOME(v2n ovar),
				inner = NameSet.empty}))

fun mkWir2 ovar ivar =
    let val link = Link.make
		       {outer = SOME(v2n out),
			inner = NameSet.insert empty (v2n inn)}
	val linkset = NameSet.insert NameSet.empty link
    in Wiring.make linkset end

fun nms2nmSet n = List.map (fn x => NameSet.insert NameSet.empty x)
			   (nms2nmList n)

fun lookupKind cid signa =
    let fun loop [] = NONE
	  | loop ((i,k,b,f)::p) = if cid = i then SOME k else loop p
    in loop signa end

fun lookupBigraph id idmap =
    let fun loop [] = NONE
          | loop ((i,b)::p) = if id = i then SOME b else loop p
    in loop idmap end

fun ctrlNotInSig cid = raise Fail("Control does not exist in signature: " ^ cid ^ "\n")

type idmap = (id * bigraph) list
type sitemap = (nat * siteId) list

(***** TRANSLATION *****)
fun big2bgval ast (maps:idmap*sitemap) signa =
    let val imap = #1(maps)
	val smap = #2(maps)
    (* hmm...need to calculate and insert implicit id wirings *)
    in case ast
	of Wir(w) =>
	   (* hmmm...need to localise some names here? *)
	   ( case w of Global(out,inn) => mkWir2 out inn
		     | Local(out,inn) => mkWir2 out inn
		     | IdleG(x) => mkWir1 x
		     | IdleL(x) => mkWir1 x )
	 | Par(b1,b2) => (big2bgval b1 maps signa)
			     || (big2bgval b2 maps signa)
	 | Pri(b1,b2) => (big2bgval b1 maps signa)
			     pp (big2bgval b2 maps signa)
	 | Com(b1,b2) => (big2bgval b1 maps signa)
			     oo (big2bgval b2 maps signa)
	 | Emb(b1,b2) => (big2bgval b1 maps signa)
			     oo (big2bgval b2 maps signa)
	 | Ten(b1,b2) => (big2bgval b1 maps signa)
			     tt (big2bgval b2 maps signa)
	 | Ctrl(cid,bports,fports) =>
	   let val boundnames = List.map (nm2nmSet o s2n) bports
	       val freenames = List.map s2n fports
	   in case lookupKind cid signa
	       of SOME(k) => Ion.make {ctrl = Control.make(cid,k),
				       free = freenames,
				       bound = boundnames}
		| NONE => ctrlNotInSig cid
	   end
	 | Clo(nms,b) => (S.-//(List.map s2n nms)) 
			     oo (big2bgval b maps signa)
	 | Abs(nms,b) => B.Abs info (List.map (nm2nmSet o s2n) nms,
				     big2bgval b maps signa)
	 | Site(i,nms) => todo (*(nextNat,i) :: smap) ... *)
	 | Id(i) =>
	   ( case getBigraph i imap
	      of SOME(b) => big2bgval b maps signa
	       | NONE => raise Fail("Unbound identifier: " ^ i ^ "\n") )
	 | Empty => barren

(* fix this... *)
fun dec2bgval ast signa =
    case ast of Decs(d1,d2) => (bpl2bgval d1 signa)
			       @ (bpl2bgval d2 signa)
(*
      | Rule(i,r) =>
	let val lhs = makeBR(#1(r))
	    val rhs = #2(r)
	    val ins = calcInst lhs rhs
	in Rule.make { name = i, redex = lhs, react = rhs, inst = ins } end
      | Value(i,b) => let val b' = big2bgval b ((i,b)::imap) smap
		      in b' ((i,b')::imap) smap end
*)
      | Cdefs(cd,cds) => (bpl2bgval cd maps signa)
			 :: (bpl2bgval cds maps signa)
      | Cdef(cid,ck,n1,n2) => Ion.make {ctrl = Control.make(cid,ck),
					free = map v2n n1,
					bound = map v2n n2}
(*
      | Active => ...
      | ... => ...
*)

(* toplevel *)
fun prog2bgval ast =
    case ast of Prog(s,d) => (s, dec2bgval d s)
	      | _ => raise Fail("Malformed program")
