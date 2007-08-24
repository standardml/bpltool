(* Copyright (c) 2007  Henning Niss, IT University of Copenhagen
 *
 * BAM is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 *
 * BAM is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with BAM; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
 * USA
 *)

signature PARTIALMATCH = sig

    type t

    (* convenience shorthands *)
    type term = int option Term.t
    type rule = int option Rule.t

    val pmatch : term Stack.t * term * term vector * int Rbset.set -> t

    val LHS : t -> term Stack.t
    val RHS : t -> term
    val parameter : t -> term vector
    val indices : t -> int Rbset.set

    val compare : t * t -> order

    val initParam : rule -> term vector
    val param : int -> t -> term
    val plug : int * term -> t -> t
    val popLHS : t -> term * t

    val toplevel : t -> bool
    val returnable : t -> bool
    val returnableHole : t -> int option
    val reactable : t -> bool

    val pp : t Pretty.pp

end (* signature PARTIALMATCH *)
