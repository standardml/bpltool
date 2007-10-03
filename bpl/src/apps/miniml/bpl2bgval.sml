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

(*open TextIO;*)

structure BG = BG (structure ErrorHandler = PrintErrorHandler);
structure B = BG.BgVal
structure Sugar = BG.Sugar
structure Rule = BG.Rule
structure Control = BG.Control
structure Ion = BG.Ion
structure Name = BG.Name
structure Wiring  = BG.Wiring
structure NameSet = BG.NameSet
structure Link = BG.Link
structure LinkSet = BG.LinkSet

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
type inst = (nat * nat) list (* 21/8-07*)
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

type idmap = (id * bgval) list
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
	    | Site(i,l) => [i] (* assumes that sites are Nats already *)
	    | Id(i) => []
	    | Empty => []

fun partSmap [] acc x = (acc,[])
  | partSmap ((n,i)::m) acc x =
    if x=n then (rev acc, (n,i)::m) else partSmap m ((n,i) :: acc) x

fun lookupFst [] id = NONE
  | lookupFst ((n,i)::m) id = if i=id then SOME(n) else lookupFst m id

(* hmm...this function is fishy *)
fun site2int s =
    case s of Num(n) => n
	    | Var(v) => raise Fail("site2int: " ^ v ^ "is not a Nat\n")

fun nat2string n = Int.toString(n)

fun calcInst (b1:bgval) (b2:bgval) pivot (maps:idmap*sitemap) =
    let val smap = #2(maps)
	val (smap1,smap2) = partSmap smap [] pivot
	val inst = List.map (fn (n,i) => let val x = lookupFst smap1 i
					 in (n,x) end) smap2
	fun err n = "calcInst: Site " ^ nat2string(n) ^
		    " has no LHS counterpart\n"
	fun peel assocOptList =
	    case assocOptList
	     of [] => []
	      | ((n,x)::m) => case x of NONE => raise Fail(err n)
				      | SOME(i) => (n,i) :: peel m
    in peel inst end

fun s2n s = Name.make s

fun v2n x = s2n (String.toString x)

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

fun lookupBgval id idmap =
    let fun loop [] = NONE
          | loop ((i,b)::p) = if id = i then SOME b else loop p
    in loop idmap end

fun nList2nSet l = List.map (nm2nmSet o s2n) l

fun lastCnt [] = 0
  | lastCnt ((k,v)::m) = #1(List.hd(rev m))

fun ctrlNotInSig cid = raise Fail("Control does not exist in signature: " ^ cid ^ "\n")

(***** TRANSLATION : ast -> bgval *****)
fun big2bgval ast signa (maps:idmap*sitemap) =
    let val imap = #1(maps)
	val smap = #2(maps) (* to be updated in this function *)
	val cnt = lastCnt smap
	val returnmap = ref smap
    in let val bgval =
	       case ast
		of Wir(w) =>
		   (* todo: do these cover all cases? *)
		   ( case w
		      of Global(out,inn) => mkWir2 out inn
		       | Local(out,inn) =>
			 B.Abs info ((nm2nmSet o s2n) out, mkWir2 out inn)
		       | IdleG(x) => mkWir1 x
		       | IdleL(x) => B.Abs info ((nm2nmSet o s2n) x, mkWir1 x)
		   )
		 | Par(b1,b2) =>
		   let val (b1',smap') = big2bgval b1 signa maps
		       val (b2',smap'') = big2bgval b2 signa (imap,smap')
		   in returnmap := smap'' ; b1' || b2' end
		 | Pri(b1,b2) =>
		   let val (b1',smap') = big2bgval b1 signa maps
		       val (b2',smap'') = big2bgval b2 signa (imap,smap')
		   in returnmap := smap'' ; b1' pp b2' end
		 | Com(b1,b2) =>
		   let val (b1',smap') = big2bgval b1 signa maps
		       val (b2',smap'') = big2bgval b2 signa (imap,smap')
		   in returnmap := smap'' ; b1' oo b2' end
		 | Emb(b1,b2) =>
		   let val (b1',smap') = big2bgval b1 signa maps
		       val (b2',smap'') = big2bgval b2 signa (imap,smap')
		   in returnmap := smap'' ; b1' oo b2' end
		 | Ten(b1,b2) =>
		   let val (b1',smap') = big2bgval b1 signa maps
		       val (b2',smap'') = big2bgval b2 signa (imap,smap')
		   in returnmap := smap'' ; b1' tt b2' end
		 | Ctrl(cid,bports,fports) =>
		   let val boundnames = nList2nSet bports
		       val freenames = List.map s2n fports
		   in case lookupKind cid signa
		       of SOME(k) => Ion.make {ctrl = Control.make(cid,k),
					       free = freenames,
					       bound = boundnames}
			| NONE => ctrlNotInSig cid
		   end
		 | Clo(nms,b) => (Sugar.-//(List.map s2n nms)) 
				     oo (big2bgval b signa maps)
		 | Abs(nms,b) => B.Abs info (nList2nSet nms,
					     big2bgval b signa maps)
		 (* | Conc(nms,b) => not used *)
		 | Site(i,nms) =>
		   ( returnmap := (cnt+1,i) :: (!returnmap)
		   ; S.id_1 tt (S.idw nms) )
		 | Id(i) =>
		   ( case lookupBgval i imap
		      of SOME(b) => b
		       | NONE => raise Fail("Unbound identifier: " ^ i ^ "\n")
		   )
		 | Empty => barren
       in (bgval, !returnmap) end (* return bgval and new sitemap *)
    end

fun dec2bgval decls vals rules signa (maps:imap*smap) =
    let val imap = #1(maps) (* for rolling bgval list into main bgval *)
	val smap = #2(maps) (* for making instantiations *)
    in case decls
	of [] => (vals, rules, maps)
	 | (d::ds) =>
	   ( case d
	      of Rule(i,b1,b2) =>
		 let val (b1',smap') = big2bgval b1 signa maps
		     val (b2',smap'') = big2bgval b2 signa (imap,smap')
		     val pivot = site2int(List.hd(List.rev(getSites b1))) + 1
		     val ins = calcInst b1' b2' pivot (imap,smap'')
		     val rule = Rule.make { name = i, redex = b1',
					    react = b2', inst = ins}
		     val rules' = rule :: rules
		 in dec2bgval ds vals rules' signa (imap,smap'') end
	       | Value(i,b) =>
		 let val (b',smap') = big2bgval b signa maps
		     val imap' = (i,b') :: imap (* update imap *)
		     val vals' = b' :: vals
		 in dec2bgval ds vals' rules signa (imap',smap') end )
    end

(* toplevel *)
fun prog2bgval ast =
    case ast
     of Prog(signa,declist) =>
	let val (vals,rules,maps) = dec2bgval declist [] [] signa ([],[])
	    (* vars of last val	have been substituted for on the fly *)
	    val mainBgval = List.hd(rev vals)
	in (signa, mainBgval, rules) end
      | _ => raise Fail("Malformed program")
