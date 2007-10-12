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

(** Abstract data type for an alternative variant of bigraph terms.
 *
 *
 * TODO: UPDATE THIS OUT-OF-DATE-DOCUMENTATION:
 *
 * The terms are not necessarily well-formed:
 * scope rule may be violated in abstractions, names may clash in
 * interfaces of tensor product operands, bigraph widths may be
 * incompatible in compositions, etc.
 * <p>
 * Each constructor takes an info argument that can contain contextual
 * information (e.g., source file location for the term).
 * @version $LastChangedRevision: 930 $
 *)
signature BPL_TERM = sig
	(** Control identifier type *)
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
                     | Ion of ctrlid * (*binding:*)edge list * (*free:*)edge list
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

    val ppBigraph : bigraph Pretty.pp
    val pp        :    prog Pretty.pp

end (* signature BPL_TERM *)
