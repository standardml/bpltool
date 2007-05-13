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

structure BPLTerm :> BPL_TERM = struct

    type ctrlid = string
    type namedsite = string
    type id = string

    datatype wire = GRen of id * id
		  | GIdle of id
		  | LRen of id * id
		  | LIdle of id
    type wiring = wire list

    type edge = id
		
    type ports = edge list
		 
    datatype bigraph = Wir of wiring
		     | Par of bigraph * bigraph
		     | Pri of bigraph * bigraph
		     | Com of bigraph * bigraph
		     | Ten of bigraph * bigraph
                     | Ion of ctrlid * (*free:*)edge list * (*binding:*)edge list
		     | Clo of edge list * bigraph
		     | Abs of edge list * bigraph
		     | Site of int * ports option
		     | Nsite of namedsite * ports option
		     | Id of id
		     | EmptyPri
                     | Empty
    datatype rule = Rule of bigraph * bigraph
    datatype ctrlkind = Active
		      | Passive
		      | Atomic
    datatype ctrldef = Ctrl of ctrlid * ctrlkind * int * int
    datatype sign = Sig of ctrldef list
    datatype sigdef = SigDef of id * sign
    datatype dec = Seq of dec * dec
		 | RuleDec of id * rule
		 | ValDec of id * bigraph
    datatype prog = Prog of sigdef list * id * dec

    fun flatten0 dec acc =
	case dec of
	    Seq(d1, d2) => flatten0 d2 (flatten0 d1 acc)
	  | _ => dec :: acc
    fun flatten dec = rev(flatten0 dec [])

    open Pretty
    infixr 5 ^+
    infix 4 +^
    infixr 4 ++

    fun ppBigraph b =
	case b of
	    Wir w => ppWiring w
	  | Par(b,b') => ppOp "||" b b'
	  | Pri(b,b') => ppOp "|" b b'
	  | Com(b,b') => ppOp "o" b b'
	  | Ten(b,b') => ppOp "*" b b'
	  | Ion(c, [], []) => ppString c
	  | Ion(c, fs, bs) => 
	       ppString c
	       ++ (bracket "<#>" (ppEdges fs ++ (bracket "(#)" (ppEdges bs))))
	  | Clo(es, b) => ppBinary("/" ^+ ppEdges es, ".", ppBigraph b)
	  | Abs(es, b) => (bracket "{#}" (ppEdges es)) ++ (ppBigraph b)
	  | Site(i, NONE) => "[" ^+ ppInt i +^ "]"
	  | Nsite(ns, NONE) => ppString ns
	  | Id i => ppString i
	  | EmptyPri => ppString "1"
	  | Empty => ppString "()"
    and ppOp ope b b' = ppBinary(ppBigraph b, ope, ppBigraph b')
    and ppEdges es = clist ",#" ppString es
    and ppPorts ps = bracket "<#>" (ppEdges ps)
    and ppWire w =
	case w of
	    GRen(i,i') => ppBinary(ppString i, "/", ppString i')
	  | LRen(i,i') => ppBinary(ppLocal i, "/", ppLocal i')
	  | GIdle i => ppString i +^ "/"
	  | LIdle i => ppLocal i +^ "/"
    and ppLocal id = bracket "{#}" (ppString id)
    and ppWiring ws = clist ",#" ppWire ws

    fun pp (Prog(sign, id, dec)) =
	let val sign' = empty
	    val decs = flatten dec
	    val decs' = makelist(true,"")(map (forcemulti o ppDec) decs)
	in  makelist (true, "") (map forcemulti
              [sign', "using " ^+ ppString id, decs']
            )
	end
    and ppDec dec =
	case dec of
	    Seq _ => Util.abort 767832
	  | RuleDec(id, Rule(lhs, rhs)) =>
	      ppString "rule" ++ (ppString id +^ " =") ++ (ppOp "->" lhs rhs)
	  | ValDec(id, bigraph) => 
	      ppString "val" ++ (ppString id +^ " =") ++ ppBigraph bigraph
end (* structure BPLTerm *)
