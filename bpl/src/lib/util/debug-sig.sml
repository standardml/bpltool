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

(** Module with debugging utilities.
 * @version $LastChangedRevision: 121 $
 *)
 
signature DEBUG =
sig
  (** The debug level type. *)
  type debug_level

  (** No debugging information should be output. *)
  val LVL_USER   : debug_level
  (** Little debug information should be output. *)
  val LVL_LOW    : debug_level
  (** Some debug information should be output. *)
  val LVL_MEDIUM : debug_level
  (** All debug information should be output. *)
  val LVL_HIGH   : debug_level

  (** Apply the given function to the given data
      if the debug level is high enough. *)
  val debug : debug_level -> ('a -> unit) -> 'a -> unit

  (** Returns true if the current debug level 
   * contains <code>debug_level</code>.
   *)
  val debug_enabled : debug_level -> bool
end
