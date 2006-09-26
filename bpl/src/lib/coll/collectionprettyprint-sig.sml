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

(** Prettyprinter signature for monomorphic collections
 * @version $LastChangedRevision$
 *)
signature COLLECTIONPRETTYPRINT =
sig
  type collection
  type ppstream
  (** Prettyprint a given collection.
   * @params indent S
   * @param indent  Indentation to use at each block level.
   * @param S       The collection to print.
   *)
  val pp : int -> ppstream -> collection -> unit
  (** Prettyprint a given collection with specific surrounding braces.
   * @params indent leftb rightb S
   * @param indent  Indentation to use at each block level.
   * @param leftb   Left brace.
   * @param rightb  Right brace.
   * @param S       The collection to print.
   *)
  val ppbr : int -> string -> string -> ppstream -> collection -> unit
end
