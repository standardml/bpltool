(* Copyright (c) 2008  The BPL Group at the IT University of Copenhagen
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
 * Mapping from BPL abstract syntax tree to BgVal.
 * Implements bpl/bplproject/doc/projects/contextawareness/plato/bpl-bnf.tex
 * <p>
 * Input: A list of decls (Values and Rules) and a signature.          <br />
 * Output: A signature, a main bgval (the state of the BRS), and
 *   a set of rules of bgvals with instantiations.
 *
 * @author: Ebbe Elsborg (elsborg@itu.dk) et al.
 * @version $LastChangedRevision$
 *)
signature BPL2BGVAL =
sig
  (** Node control kind. *)
  eqtype kind
  (** Bigraph signature type. *)
  type sign = (string * kind * int * int) list
  (** BPL Rule or BPL value declaration type. *)
  type dec
  (** Bigraph value type. *)
  type bgval
  (** Bigraph rule type. *)
  type rule
  (** Inteface type. *)
  type interface
  (** Main function, converting a BPL program into an agent and
   * a bigraphical reactive system.
   *)
  val prog2bgval : sign * dec list -> bgval * rule list
  (** Print two interfaces. *)
  val printIfaces : string -> interface -> interface -> unit
end