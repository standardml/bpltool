(* Copyright (c) 2007  The BPL Group at the IT University of Copenhagen
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

(** 
 * Abstract data type for an alternative variant of bigraph terms,
 * and functions to prettyprint this abstract datatype.
 *
 * Henning Niss, Ebbe Elsborg et al.
 *
 * @version $LastChangedRevision$
 *)
functor BPLTerm (structure Control : CONTROL) :> BPLTERM
where type kind = Control.kind =
struct

	type id = string
  datatype siteid = SiteNum of int | SiteName of id

  datatype wire
  = GRen of id * id
  | GInt of id
	| LRen of id * id
	| LInt of id

  datatype bigraph
  = Wir of wire list
  | Par of bigraph * bigraph
  | Pri of bigraph * bigraph
  | Com of bigraph * bigraph
  | Ten of bigraph * bigraph
  | Ion of id * id list * id list
  | Clo of id list * bigraph
  | Abs of id list * bigraph
  | Con of id list * bigraph 
  | Sit of siteid * id list
  | Ref of id
  | Bar

  open Control

  type sign = (id * kind * int * int) list
  type sigdef = id * sign

  datatype dec
  = Rul of id * (bigraph * bigraph)
	| Val of id * bigraph
  
  type prog = sigdef list * id * dec list

  open Pretty
  infixr 5 ^+
  infix  4 +^
  infixr 4 ++

  fun ppSiteId (SiteNum i)  = ppInt i
    | ppSiteId (SiteName n) = ppString n

  fun ppBigraph b =
	  case b of
          (* special-case a few combinations *)
            Com(Ion ion, b) => break(0,1)(ppIon ion,bracket "(#)" (ppBigraph b))
          (* we now return to our regular program *)
	  | Wir w => ppWiring w
	  | Par(b,b') => ppOp "||" b b'
	  | Pri(b,b') => ppOp "|" b b'
	  | Com(b,b') => ppOp "o" b b'
	  | Ten(b,b') => ppOp "*" b b'
	  | Ion ion => ppIon ion
	  | Clo(xs, b) => ppBinary("/" ^+ ppEdges xs, ".", ppBigraph b)
	  | Abs(ys, b) => (bracket "{#}" (ppEdges ys)) ++ (ppBigraph b)
	  | Con(ys, b) => ("'" ^+ ppEdges ys +^ "'") ++ (ppBigraph b)
	  | Sit(i, []) => "[" ^+ ppSiteId i +^ "]"
	  | Sit(i, ports) => ("[" ^+ ppSiteId i +^ "]") ++ ppPorts ports
	  | Ref i => ppString i
	  | Bar => ppString "1"
  and ppOp ope b b' = ppBinary(ppBigraph b, ope, ppBigraph b')
  and ppIon (c, [], []) = ppString c 
    | ppIon (c, fs, bs)
    = ppString c
	    ++ (bracket "<#>" (ppEdges fs ++ (bracket "(#)" (ppEdges bs))))
  and ppEdges es = clist ",#" ppString es
  and ppPorts ps = bracket "<#>" (ppEdges ps)
  and ppWire w =
	  case w of
	    GRen(i,i') => ppBinary(ppString i, "/", ppString i')
	  | LRen(i,i') => ppBinary(ppLocal i, "/", ppLocal i')
	  | GInt i => ppString i +^ "/"
	  | LInt i => ppLocal i +^ "/"
  and ppLocal id = bracket "{#}" (ppString id)
  and ppWiring ws = clist ",#" ppWire ws

  fun pp (sign, id, decs) =
	  let
	    val sign' = makelist(true,"")(map (forcemulti o ppSignature) sign)
	    val decs' = makelist(true,"")(map (forcemulti o ppDec) decs)
	  in
	    makelist
	      (true, "")
	        (map forcemulti [sign', "using " ^+ ppString id, decs'])
	  end
  and ppDec dec =
	  case dec of
	    Rul (id, (lhs, rhs)) =>
	      ppString "rule" ++ (ppString id +^ " =") ++ (ppOp "->" lhs rhs)
	  | Val (id, bigraph) => 
	      ppString "val" ++ (ppString id +^ " =") ++ ppBigraph bigraph
  and ppSignature (id, ctrldefs) =
  	close(1,"end")
	    (("signature " ^+ ppString id +^ " =") ++ ppString "sig"
	     ++ (makelist(true,"") (map (forcemulti o ppCtrlDef) ctrldefs)))
  and ppCtrlDef (id, kind, b, f) =
	  (id ^ " : ") ^+ ppKind kind ++ (ppBinary(ppInt b, "->", ppInt f))
  and ppKind Atomic  = ppString "atomic"
    | ppKind Passive = ppString "passive"
    | ppKind Active  = ppString "active"
end (* structure BPLTerm *)
