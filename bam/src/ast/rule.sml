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

structure Rule :> RULE = struct
    exception NotWellFormed
    type 'ctrlinfo t = 'ctrlinfo Term.t * 'ctrlinfo Term.t
    fun rule (lhs, rhs) = 
	let val lhsHoles = Term.holeIndices lhs
	    val rhsHoles = Term.holeIndices rhs
	in  if Rbset.isSubset(rhsHoles, lhsHoles) then (lhs, rhs)
	    else raise NotWellFormed
	end
    val LHS : 'ctrlinfo t -> 'ctrlinfo Term.t = #1
    val RHS : 'ctrlinfo t -> 'ctrlinfo Term.t = #2
    fun holeIndices r = Term.holeIndices (LHS r)
    fun maxHoleIndex r = Term.maxHoleIndex (LHS r)
    fun map f (lhs, rhs) = (Term.map f lhs, Term.map f rhs)
    val compare = Util.pairCmp (Term.compare, Term.compare)
    fun pp (lhs, rhs) = Pretty.ppBinary(Term.pp lhs, "-->", Term.pp rhs)
end (* structure Rule *)
