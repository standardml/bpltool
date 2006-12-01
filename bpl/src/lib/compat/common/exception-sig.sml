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

(** Module with exception related utility functions.
 *)
signature EXCEPTION = sig
  (** The type of history events. *)
  type event

  (** Convert the event to a string. *)
  val event2string : event -> string

  (** Return a list of events for the exception in reverse
   *  chronological order (hd occurred last).
   *
   *  Events are
   *  (a) the place where the exception was raised initially,
   *  (b) the location of an errorhandler through which the exception
   *      has passed, or
   *  (c) the location of a reraise of the exception.
   *
   *  The availability of exception history is dependent on the
   *  compiler/runtime system - this function should return the
   *  empty list if no history is available.
   *)
  val history : exn -> event list

end (* signature EXCEPTION *)
