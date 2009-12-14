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

(** Abstract data type for modelling edges.
 * @version $LastChangedRevision: 2717 $
 *)
functor Edge(structure ErrorHandler : ERRORHANDLER
               where type ppstream    = PrettyPrint.ppstream
                 and type break_style = PrettyPrint.break_style
                 and type origin      = Origin.origin
        ) :> EDGE =
struct
  (* It seems that the Name'-functor implements most of what we need,
   * so we will just use that for now... *)
  structure Name = Name'(structure ErrorHandler = ErrorHandler)

  type edge = Name.name

  val == = Name.==

  val lt = Name.<

  val compare = Name.compare

  val op < = lt

  (** Construct a node. *)
  val make = Name.make

  (**  Deconstruct a node. *)
  val unmk = Name.ekam

  structure Order : ORDERING =
  struct
    type T = edge
    fun lt e1 e2 = e1 < e2
  end

  structure EdgeSet = Rbset(type t = edge val compare = compare)
end
