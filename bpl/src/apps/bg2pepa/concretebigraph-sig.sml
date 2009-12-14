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

(** Abstract data type for modelling concrete bigraphs.
 * @version $LastChangedRevision: 2717 $
 *)
signature CONCRETE_BIGRAPH =
sig
  type nodeset
  type 'a nodemap
  type edgeset
  type nameset
  type 'a aspectmap
  type control
  type controlset
  type 'a controlmap
  type childset
(*  type controlmap
  type parentmap
  type linkmap
*)
  type node
  type place
  type aspect
  type value
  type change
  type edit_script

  type interface

  type absbg
(*
  type placegraph
  type linkgraph
*)
  type translation

  (** The concrete bigraph data type. *)
  type conbg

(*
  (** Construct a concrete bigraph. *)
  val make : nodeset -> edgeset -> controlmap -> parentmap -> linkmap ->
             conbg
*)
  (** Signal that a node is part of a non-tree. *)
  exception NotTree of node
  (** Construct a concrete bigraph from a set of aspects. *)
(*   val make' : aspect list -> conbg *)
  val make' : (aspect * value) list -> conbg
(*
  (** Deconstruct a concrete bigraph. *)
  val unmk : conbg ->
             nodeset * edgeset * controlmap * parentmap * linkmap
*)
  (** Deconstruct a concrete bigraph into its aspects. *)
(*   val unmk' : conbg -> aspect list *)
  val unmk' : conbg -> (aspect * value) list

  val unmk'' : conbg -> value aspectmap

  (** Return the nodes of a concrete bigraph. *)
  val nodes : conbg -> nodeset

  (** Return the edges of a concrete bigraph. *)
  val edges : conbg -> edgeset

  (** Return the inner names of a concrete bigraph. *)
  val innernames : conbg -> nameset

  (** Return the outer names of a concrete bigraph. *)
  val outernames : conbg -> nameset

  (** Return the controls of a concrete bigraph. *)
  val controls : conbg -> controlset

  (** Return the ctrl function of a concrete bigraph. *)
  val ctrl : conbg -> control nodemap

  (** Return the inverse of the ctrl function of a concrete bigraph. *)
  val ctrl_inv : conbg -> nodeset controlmap

  (** Return the children of a place in a concrete bigraph. *)
  val children : conbg -> place -> childset

  (** Return the idle names of a concrete bigraph. *)
  val idle_names : conbg -> nameset

  (** Return the abstract bigraph that a concrete bigraph is an
   * instance of. *)
  val abstract : conbg -> absbg

  (** Return a concrete instance from an abstract bigraph. *)
  val concretize : absbg -> conbg

  (** FIXME *)
  val support : conbg -> {nodes : nodeset, edges : edgeset}

  (** FIXME *)
  val width : conbg -> int

  (** Translate the support according the given translation. *)
  val translate : translation -> conbg -> conbg

  (** Apply a set of changes to a concrete bigraph. *)
  val app : change list -> conbg -> conbg

  (** Return an edit script for the editing distance between b1 and
   * b2. *)
  val edit_script : conbg -> conbg -> edit_script

(*
  (** Return the underlying place graph of a concrete bigraph. *)
  val placegraph : conbg -> placegraph
  (** Return the underlying link graph of a concrete bigraph. *)
  val linkgraph : conbg -> linkgraph
*)
end
