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

(** Main BG module.
 * @version $LastChangedRevision$
 *)
signature BG =
sig

  include BG_ADT

  (** Read a BG expression from a file print it, and return it as BDNF. *)
  val usefile : string -> B bgbdnf
  (** Read a BG expression from a file, return it as BDNF, explain
   * any errors on stdOut.
   *)
  val usefile' : string -> B bgbdnf
  (** Read a BG expression from a file, return it as BDNF. *)
  val usefile'' : string -> B bgbdnf
  (** Read a BG expression from a file, return it as a bgval, explain
   * any errors on stdOut.
   *)
  val bgvalUsefile' : string -> bgval
  (** Read a BG expression from a file, return it as a bgval. *)
  val bgvalUsefile'' : string -> bgval
  (** Prettyprinter for bigraphs. *)
  val pp : ppstream -> 'class bgbdnf -> unit
  (** Return string representation of a normal form bigraph. *)
  val toString : 'class bgbdnf -> string
  (** Return string representation of a bigraph value. *)
  val bgvalToString : bgval -> string

  structure BgTermParser : PARSER
end
