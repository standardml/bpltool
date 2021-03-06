(* Copyright (c) 2007  The BPL Group at the IT University of Copenhagen
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

(** Module with signal handling utility functions.
 * @version $LastChangedRevision$
 *)

signature SIGNALHANDLER =
sig
  (** Executes a function f with side effects, aborting it 
   * if an INT signal is delivered.  Before executing f,
   * resumeInterrupts() is called.
   * @return A boolean indicating whether the function was aborted.
   *)
  val interrupted : (unit -> unit) -> bool
  (** Inhibit delivery of INT signals.  Any INT signals will be
   * ignored and will not re-appear after a call to acceptInterrupts.
   *)
  val ignoreInterrupts : unit -> unit
  (** Allow delivery of INT signals *)
  val acceptInterrupts : unit -> unit
  (** Defer delivery of interrupts till later. *)
  val deferInterrupts : unit -> unit
  (** Resume delivery of interrupts, including any deferred. *)
  val resumeInterrupts : unit -> unit
end
