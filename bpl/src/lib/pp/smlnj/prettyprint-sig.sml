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

(** Prettyprinter signature from SML/NJ
 * @version $Revision: 1.2 $
 *)
signature PRETTYPRINT =
sig
  type ppstream
  type ppconsumer
  datatype break_style  = CONSISTENT  | INCONSISTENT
  exception PP_FAIL of string
  val mk_ppstream : ppconsumer -> ppstream
  val dest_ppstream : ppstream -> ppconsumer
  val add_break : ppstream -> (int * int) -> unit
  val add_newline : ppstream -> unit
  val add_string : ppstream -> string -> unit
  val begin_block : ppstream -> break_style -> int -> unit
  val end_block : ppstream -> unit
  val clear_ppstream : ppstream -> unit
  val flush_ppstream : ppstream -> unit
  val with_pp : ppconsumer -> (ppstream -> unit) -> unit
  val pp_to_string : int -> (ppstream -> 'a -> unit) -> 'a -> string
end
