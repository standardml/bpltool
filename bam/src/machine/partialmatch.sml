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

structure IntSet = struct
    open Rbset
    type t = int set
    val empty = empty Int.compare
    val singleton = singleton String.compare
end

structure PartialMatch : PARTIALMATCH = struct

    (* convenience shorthands *)
    type term = int option Term.t
    type rule = int option Rule.t

    type t = term Stack.t * term * term vector * IntSet.t

    fun pmatch (lhs,rhs,parameter,indices) = (lhs, rhs, parameter, indices)
    val LHS : t -> term Stack.t = #1
    val RHS : t -> term = #2
    val parameter : t -> term vector = #3
    val indices : t -> IntSet.t = #4

    fun compare (pm1, pm2) = 
	case Stack.compare Term.compare (LHS pm1, LHS pm2) of
	    EQUAL => (case Term.compare (RHS pm1, RHS pm2) of
			  EQUAL => (case Util.vectorCmp Term.compare (parameter pm1, parameter pm2) of
					EQUAL => IntSet.compare(indices pm1, indices pm2)
				      | ord => ord)
			| ord => ord)
	  | ord => ord

    exception UnknownParameter of int

    fun initParam rule =
	Vector.tabulate(1 + Rule.maxHoleIndex rule, fn i => Term.Hole i)
    fun param j pm = 
	Vector.sub(parameter pm, j) 
	handle Subscript => raise UnknownParameter j
    fun plug (j, p) (L, R, D, I) = 
	let val D' = Vector.tabulate(Vector.length D, fn i => if i=j then p else Vector.sub(D, i) handle Subscript => raise UnknownParameter i)
	in  (L, R, D', I)
	end 

    fun popLHS (L, R, D, I) =
	if Stack.isEmpty L then raise Fail("popLHS: LHS is empty")
	else let val (G, L) = Stack.pop L
	     in  (G, (L, R, D, I))
	     end

    fun toplevel (L, R, D, I) =
	if Stack.isEmpty L then false
	else Stack.isEmpty(#2(Stack.pop L))

    fun returnable0 (L, R, D, I) =
	if Stack.isEmpty L then NONE
	else let val (G, L') = Stack.pop L
	     in  if Stack.isEmpty L' then NONE
		 else case Term.view G of
			  Term.VNil => SOME(Term.VNil)
			| Term.VHole i => SOME(Term.VHole i)
			| _ => NONE
	     end
    fun returnable pm =
	case returnable0 pm of
	    SOME(Term.VNil) => true
	  | _ => false
    fun returnableHole pm =
	case returnable0 pm of
	    SOME(Term.VHole i) => SOME i
	  | _ => NONE

    fun reactable (L, R, D, I) =
	if Stack.isEmpty L then false
	else let val (G, L) = Stack.pop L 
	     in  case Term.view G of
		     Term.VNil => Stack.isEmpty L
		   | _ => false
	     end
		    
    open Pretty
    fun pp (lhs, rhs, params, indices) =
	bracket "(#)"
		(clist "#, " (fn t => t)
	               [ ppBinary(Stack.pp "»" Term.pp lhs, "->", Term.pp rhs)
                       , "#" ^+ bracket "[#]" (Util.ppVector Term.pp params)
	 	       , Util.ppSet ppInt indices
                       ]
		)

end (* structure PartialMatch *)

structure PartialMatchSet (*: sig
			       sig
				   include Rbset
       type t = PartialMatch.t set
       val empty : t
       val singleton : PartialMatch.t -> t
       val init : rule Rbset.set -> t
end *)= struct
    open Rbset
    type t = PartialMatch.t set
    val empty = empty PartialMatch.compare
    val singleton = singleton PartialMatch.compare
    val mapPartial = fn f => mapPartial (f, PartialMatch.compare)

    fun init rules = 
	let fun f rule = 
		PartialMatch.pmatch (Stack.push(Rule.LHS rule) Stack.empty, 
				     Rule.RHS rule, PartialMatch.initParam rule, IntSet.empty)
	in  Rbset.map (f, PartialMatch.compare) rules
	end

    fun minus (PM, I) = 
	let fun f (pm, acc) =
		if IntSet.isEmpty(IntSet.intersection(PartialMatch.indices pm,I))
		then add(acc,pm)
		else acc
	in  foldl f empty PM
	end
end (* structure PartialMatchSet *)
