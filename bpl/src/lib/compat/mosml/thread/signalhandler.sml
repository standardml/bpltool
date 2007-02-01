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

(** Module with dummy signal handling utility functions for Moscow ML.
 * @version $LastChangedRevision: 393 $
 *)

structure SignalHandler : SIGNALHANDLER =
struct
  open TextIO
  val warningGiven = ref false
  fun warn ()
    = (if !warningGiven then () else
  	     (output (stdErr, "lib/compat/mosml/thread/signalhandler.sml:\
  	     	\ Warning: signal handling not implemented faithfully for Moscow ML!\n");
  	     	flushOut stdErr);
  	   warningGiven := true)

  fun interrupted f	= ((f (); false) handle Interrupt => true)
  prim_val catch_interrupt : bool -> unit = 1 "sys_catch_break"
  fun blockInterrupts () = catch_interrupt false
  fun unblockInterrupts () = catch_interrupt true
end

