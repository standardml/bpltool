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

signature TIMINGS = sig

    (** A flag controlling whether to do timings or not.
      * The flag can also be controlled through the
      * flag <code>/misc/timings</code> (see 
      * <a href="SigFLAGS.html">FLAGS</a>).
      *)
    val do_timings : bool ref
    (** Create a new timer.
      * @params name desc
      * @param name name of timer
      * @param desc a short desriptive text for the timer
      *)
    val add : {name:string,desc:string} -> (unit -> 'a) -> 'a
    (** Generate a list of timing results.
      * Only include timers mentioned in the argument.
      * @params timers
      * @param timers list of timers to include
      *)
    val list : string list (* timer names to include *) -> string list
    (** Generate a list of timing results.
      * Includes all timers.
      *)
    val listAll : unit -> string list

end (* structure Timings *)

