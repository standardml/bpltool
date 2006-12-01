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

(** The origin of some data (e.g. a file and a position in that file).
 * @version $LastChangedRevision: 121 $
 *)
 
signature ORIGIN =
sig
  type ppstream

  type origin

  val unknown_origin : origin

  datatype pos = NOPOS | POS of (int * int) * (int * int)

  datatype origin_dt =
    UNKNOWN_ORIGIN
  | FILE_ORIGIN of string * pos

  val mk_file_origin : string -> pos -> origin

  val unmk_origin : origin -> origin_dt

  val pp : int -> ppstream -> origin -> unit
end
