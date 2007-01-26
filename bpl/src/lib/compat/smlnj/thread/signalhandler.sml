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

(** Module with signal handling utility functions for SML/NJ.
 * @version $LastChangedRevision: 393 $
 *)

structure SignalHandler : SIGNALHANDLER =
struct
  open Signals
  open SMLofNJ.Cont
  fun interrupted f =
    let
      val aborted = ref false
    in
      callcc (fn k =>
							 let
							   val orighandler = inqHandler sigINT
							 in
							   setHandler (sigINT, 
								       HANDLER (fn _ => (aborted := true; k)));
							   unmaskSignals (MASK [sigINT]);
							   f ();
							   setHandler (sigINT, orighandler); ()
							 end);
      !aborted
    end
  fun blockInterrupts () = maskSignals (MASK [sigINT])
  fun unblockInterrupts () = unmaskSignals (MASK [sigINT])
end

