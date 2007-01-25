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

(** Module with signal handling utility functions for MLton.
 * @version $LastChangedRevision: 393 $
 *)

structure SignalHandler : SIGNALHANDLER =
struct
  open MLton.Thread
  open MLton.Signal
  open Posix
  fun interrupted f =
      let
        fun workrunner t =
	  let
	    val workthread 
	      = new (fn () =>
			(f ();
			 switch (fn _ => prepare (t, false))))
	    val INThandler
	      = Handler.handler (fn rt => prepare (t, true)) 
	  in
	    setHandler (Posix.Signal.int, INThandler);
	    Mask.unblock (Mask.some [Signal.int]);
	    prepare (workthread, ())
	  end
      in
	switch workrunner
      end
  fun blockInterrupts ()
    = Mask.block (Mask.some [Signal.int])
  fun unblockInterrupts ()
    = Mask.unblock (Mask.some [Signal.int])
end
