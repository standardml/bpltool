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

(** Translation from MiniML to bigraphs.
 * @version $Revision: 1.11 $
 * Modified: $Date: 2006/09/04 09:31:14 $ by: $Author: hniss $
 *)

signature BGGEN =
sig
  type bpl
  type ppstream

  datatype 'a result = Some of 'a | None of exn
  val toBG : (int*int) MiniML.prog' -> bpl result
  val pp : int -> ppstream -> bpl -> unit

(*
  val normalize : bpl -> bpl' result
  val pp' : int -> ppstream -> bpl' -> unit
  val explain : string (*prompt*) -> ppstream -> exn -> unit
*)
end
