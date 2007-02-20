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

(** Timer structure matching the TIMER signature from the
 * new SML Basis Library, but based on the old SML Basis
 * Library Timer structure.
 * @version  $LastChangedRevision$
 *)

structure Timer :> TIMER = struct
    open Timer

    fun checkCPUTimes tmr = (* not completely exact, but anyway *)
	let val {sys,usr,gc} = checkCPUTimer tmr
	in  { nongc={usr=Time.-(usr,gc),sys=sys}
            , gc={usr=gc,sys=Time.zeroTime}
            }
        end

    fun checkCPUTimer tmr = (* directly off the Basis spec *)
	let val {nongc, gc} = checkCPUTimes tmr
        in { usr = Time.+(#usr nongc, #usr gc)
           , sys = Time.+(#sys nongc, #sys gc)
	   }
	end

    fun checkGCTime tmr = #usr(#gc(checkCPUTimes tmr))

end (* structure TIMER *)
