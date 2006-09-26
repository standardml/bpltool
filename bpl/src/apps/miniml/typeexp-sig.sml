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

(** Type expressions used during type inference.
 * @version $LastChangedRevision$
 * Modified: $Date: 2006/06/04 20:13:27 $ by: $Author: hniss $
 *)

signature TYPEEXP = sig

    type typename = string
    type typevar
    type typeexp

    val pp : typeexp Pretty.pp
    val toString : typeexp -> string

    val fresh : 'a -> typeexp
    val freshVar : string -> typeexp

    val intty : 'a -> typeexp
    val boolty : 'a -> typeexp
    val stringty : 'a -> typeexp
    val unitty : 'a -> typeexp
    val refty : typeexp -> typeexp
    val arrowty : typeexp * typeexp -> typeexp
    val tuplety : typeexp list -> typeexp
    val tyconty : typename * typeexp list -> typeexp

    type map
    val mkMap : (string * typeexp) list -> map
    val fromAST : map -> MiniML.tyexp -> typeexp

    exception Unify of string
    val unify : typeexp * typeexp -> unit

    (* views of type expressions *)
    datatype view =
	     Var
	   | Tuple of int
	   | Const of typename
	   | Arrow
    val view : typeexp -> view

    (* type schemes *)
    val pushLevel : unit -> unit
    val popLevel  : unit -> unit
    val generalize : typeexp -> unit
    val instance   : typeexp -> typeexp

end (* signature TYPEEXP *)
