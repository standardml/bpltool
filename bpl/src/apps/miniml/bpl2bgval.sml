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

datatype id = Id of string
datatype ctrlid = CtrlId of string
datatype namedsite = NamedSite of string
datatype nat = Nat of int
datatype wire = Global of id * id
	      | Local of id * id
	      | IdleG of id
	      | IdleL of id
datatype wiring = wire
		| Wires of wire * wiring
datatype names = id
	       | Names of id * names
datatype namelist = Namelist of names
datatype ports = Ports of names
datatype bigraph = Wir of wiring
		 | Par of bigraph * bigraph
		 | Pri of bigraph * bigraph
		 | Com of bigraph * bigraph
		 | Emb of bigraph * bigraph
		 | Ten of bigraph * bigraph
		 | Ctrl of ctrlid
		 | CtrlB of ctrlid * ports
		 | CtrlF of ctrlid * ports
		 | CtrlBF of ctrlid * ports * ports
		 | Clo of names * bigraph
		 | Abs of names * bigraph
		 | Site of nat
		 | Sitep of nat * namelist
		 | Nsite of namedsite
		 | Nsitep of namedsite * namelist
		 | id
		 | Empty
datatype rule = Rule of bigraph * bigraph
datatype ctrlkind = Active
		  | Passive
		  | Atomic
datatype ctrldef = Cdef of ctrlid * ctrlkind * nat * nat
datatype ctrldefs = ctrldef
		  | Cdefs of ctrldef * ctrldefs
datatype dec = Decs of dec * dec
	     | Rule of id * rule
	     | Value of id * bigraph
datatype signatur = Sig of ctrldefs
datatype prog = Prog of signatur * dec

val barren = <-> (* barren root *)
val info = BG.Info.noinfo

(* operators *)
infixr || (* parallel product *)
infixr pp (* prime product *)
infixr tt (* tensor product *)
infixr oo (* composition *)
fun (b1:bgval) || (b2:bgval) = B.Par info [b1,b2]
fun (b1:bgval) pp (b2:bgval) = B.Pri info [b1,b2]
fun (b1:bgval) tt (b2:bgval) = B.Ten info [b1,b2]
fun (b1:bgval) oo (b2:bgval) = B.Com info (b1, b2)

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

(* aux. functions *)
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

fun cds2cdList cdefs =
    case cdefs of Cdef(cid,ck,bp,fp) => [Cdef(cid,ck,bp,fp)]
		| Cdefs(cd,cds) => cd :: cds2cdList cds

fun nms2nmList names =
    case names of ID(s) => [Id(s)]
		| Names(id,nms) => id :: nms2nmList nms

fun nms2nmSet n = List.map (fn x => NameSet.insert NameSet.empty x)
			   (nms2nmList n)

fun lookupKind cid signa =
    let fun loop [] = NONE
	  | loop ((i,k,b,f)::p) = if cid = i then SOME k else loop p
    in loop signa end

fun getBigraph id imap =
    let fun loop [] = NONE
          | loop ((i,b)::p) = if id = i then SOME b else loop p
    in loop imap end

fun big2bgval ast maps signa =
    case ast
     of Wir(w) =>
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
      | Ctrl(cid) =>
	( case lookupKind cid signa
	   of SOME(k) => Sugar.k cid
	    | NONE => raise Error )
      | CtrlB(cid,Ports(b)) =>
	let val boundnames = nms2nmSet b
	in case lookupKind cid signa
	    of SOME(k) => Ion.make {ctrl = Control.make(cid,k),
				    free = [],
				    bound = boundnames}
	     | NONE => raise Error
	end
      | CtrlF(cid,Ports(f)) =>
	( case lookupKind cid signa
	   of SOME(k) => Ion.make {ctrl = Control.make(cid,k),
				   free = nms2nmlist f,
				   bound = []}
	    | NONE => raise Error )
      | CtrlBF(cid,Ports(b),Ports(f)) =>
	let val boundnames = List.map nm2nmSet (nms2nmlist b)
	in case lookupKind cid signa
	    of SOME(k) => Ion.make {ctrl = Control.make(cid,k),
				    free = nms2nmlist f,
				    bound = boundnames}
	     | NONE => raise Error
	end
      | Clo(n,b) => (S.-//(nms2nmlist n))
			oo (big2bgval b maps signa)
      | Abs(n,b) => B.Abs info (nms2nmSet n, big2bgval b maps signa)
      | Site(num) => todo
      | Sitep(num,namelist) => todo
      | Nsite(namedsite) => todo
      | Nsitep(namedsite,namelist) => todo
      | Id(i) => case getBigraph i (#1(maps))
		  of SOME(b) => big2bgval b maps signa
		   | NONE => raise Error
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
    case ast of Prog(Sig(s),d) => let val signa = cds2cdList s
				  in (signa, dec2bgval d signa) end
