(* Copyright (c) 2007  Henning Niss, IT University of Copenhagen
 *
 * BAM is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 *
 * BAM is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with BAM; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
 * USA
 *)

structure BAM = struct

    fun rewrite rules term =
	let val mkc = fn _ => NONE
	    val term' = Process.map mkc term
	    val rules' = Rbset.map (Rule.map mkc, Rule.compare) rules
	    val init = BAMState.initialState rules' term'
	    val PMinit = PartialMatchSet.init rules'
	    fun loop state =
		case BAMState.step PMinit state of
		    NONE => ()
		  | SOME state => loop state
	in  loop init
	end

end (* structure BAM *)
