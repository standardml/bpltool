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

(** Module for printing friendly error messages, based on exceptions.
 * @version $LastChangedRevision$
 *)
signature BGERRORHANDLER = 
sig
(** BDNF type. *)
type 'class bgbdnf
(** Bigraph value type. *)
type bgval
(** Bigraph term type. *)
type bgterm
(** Interface type. *)
type interface
(** Display a friendly error message on stdout. 
 * Raises exception  e if the exception is unknown.
 * @params e
 * @param e  The exception that was thrown when the error was detected.
 *)
val explain : exn -> unit
(** Return a friendly error message. 
 * Raises exception  e if the exception is unknown.
 * @params e
 * @param e  The exception that was thrown when the error was detected.
 *)
val explain' : exn -> string
end
