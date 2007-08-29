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

structure BAMState :> sig
    type t
    type term = int option Process.t
    type rule = int option Rule.t
    val step : PartialMatchSet.t -> t -> t option
    val initialState : rule Rbset.set -> term -> t
end = struct

    (* convenience *)
    structure S = PartialMatchSet 
    structure P = PartialMatch
    structure T = Process
    structure C = Control

    type term = int option Process.t
    type rule = int option Rule.t
		  
    type elem = S.t * term * term * term * term
    type stack = (elem * (int option Control.t)) Stack.t

    type t = stack * int

    local open Pretty
    in
    fun ppElem (PM, q, q', p, p') =
	Util.ppTuple
           (Util.ppSet P.pp PM :: (List.map (Process.pp' Control.pp) [q,q',p,p']))

    fun pp (S: stack, i) = 
	let val S' = Stack.take 3 S
	in  bracket "<#>" 
               (  (ilist " »»# " (ppElem o #1) S' +^ ",") 
               ++ (ppInt i))
	end
    end

    fun termminus (p, I) =
	let fun f p = if T.exists (fn SOME idx => IntSet.member(I,idx)
				    | NONE => false) p 
		      then NONE else SOME p
	    val ps = List.mapPartial f (T.toplevels p)
	in  List.foldl T.Par T.Nil ps
	end

    fun ifPred p pm = if p pm then SOME pm else NONE
    fun ifLHSMatch pat pm =
	let val (G, pm) = P.popLHS pm
	in  case T.match pat G of
		NONE => NONE
	      | SOME match => SOME(match, pm)
	end
    fun matchStep PMinit (PM, q, q', (K,p), p') K0 (S:stack) i =
	let val PMnew = if Control.isActive K then PMinit else S.empty
	    val PMtop = S.mapPartial (ifPred P.toplevel) PM
			
	    val PM' =
		let val pat = T.PPar(T.PPrefix(K,T.PVar "G'"), 
				     T.PVar("G''"))
		    fun f (match, pm) = 
			let val G' = valOf(T.lookup match "G'")
			    val G'' = valOf(T.lookup match "G''")
			    val LHS' = Stack.push G' (Stack.push G'' (P.LHS pm))
			in  P.pmatch(LHS', P.RHS pm, P.parameter pm, 
				     IntSet.add(P.indices pm, i))
			end
		in  S.mapPartial (Option.map f o ifLHSMatch pat) PM
		end handle Option => raise Fail("PM'")
					   
	    val PM'' =
		let val pat = T.PPar(T.PHoled "j", T.PVar("G'"))
		    fun f (match, pm) = 
			let val G' = valOf(T.lookup match "G'")
			    val j = valOf(T.lookupHole match "j")
			    val LHS' = Stack.push G' (P.LHS pm)
			    val pm' = P.pmatch(LHS', P.RHS pm, P.parameter pm,
				       IntSet.add(P.indices pm, i))
			in  P.plug(j,T.plug1(j,T.Prefix(K,p))(P.param j pm)) pm'
			end 
		in  S.mapPartial (Option.map f o ifLHSMatch pat) PM
		end handle Option.Option => raise Fail("PM''")
	    val traverse = (S.union(PM', PMnew), T.Nil, T.Nil, p, T.Nil)
	    val sidestep = (S.union(PM'',PMtop), q, q', p', T.Nil)
	in  (Stack.push (traverse,C.ctrl(C.name K, SOME i, C.activity K))
			(Stack.push (sidestep,K0) S), i+1)
	end
	
    fun returnStep PMinit ((PM1, q1, q1', _, _),K1) ((PM2, q2, q2', p, _),K2) 
		   (S:stack) i =
	let val PM1returns = 
		S.mapPartial (Option.map (#2 o P.popLHS) o ifPred P.returnable) PM1
	    val PM1holes =
		let fun f pm = case P.returnableHole pm of
				   SOME j => SOME(P.plug (j, T.plug1 (j, T.Nil) (P.param j pm)) (#2(P.popLHS pm)))
				 | NONE => NONE
		in  S.mapPartial f PM1
		end
	    val PM1' = S.union(PM1returns, PM1holes)
	    val (q, q') = (* if we've seen any reactions we have to
			     fuse the reactions and worklist on return *)
		case T.view q1' of
		    T.VNil => (T.Par(q2, T.Prefix(K1, q1)), q2')
		  | _      => (q2, T.Par(q2', T.Prefix(K1, T.Par(q1, q1'))))
	    val return = (S.union(PM2, PM1'), q, q', p, T.Nil)
	in  (Stack.push (return,K2) S, i)
	end
	
    fun worklistStep PMinit (PM, q, q', _, _) K0 (S:stack) i =
	case T.view q' of
	    T.VNil => (S, i)  (* FIXME: Do I want this? *)
	  | _ =>
	    let val restart = (PM, q, T.Nil, q', T.Nil)
	    in  (Stack.push (restart,K0) S, i+1)
	    end

    fun reactStep PMinit (pm, PM') (_, q, q', p, p') K0 (S:stack) i =
	let val (P,D,I) = (P.RHS pm, P.parameter pm, P.indices pm)
	    val res = T.plug D P
	    val react = (S.minus(PM', I),
			 termminus(q, I), T.Par(res, termminus(q', I)),
			 p, p')
	in  (Stack.push (react,K0) S, i)
	end

    fun findReaction0 (PM0 : S.t) (pm0 : P.t) =
	if P.reactable pm0 then
	    let val PM' = S.delete(PM0, pm0)
		val PM'reactable = S.mapPartial (ifPred P.reactable) PM'
		fun f pm =
		    if IntSet.isEmpty(IntSet.intersection(P.indices pm0, P.indices pm))
		    then true
		    else let val max = valOf(IntSet.max(P.indices pm))
			 in  case IntSet.find (fn j => j >= max) (P.indices pm0) of
				 SOME _ => true
			       | NONE => false
			 end handle Option.Option => raise Fail("findReaction")
		val PM'overlapping = S.mapPartial (ifPred f) PM'reactable
	    in  S.isEmpty PM'overlapping
	    end
	else
	    false
    fun findReaction (PM, q, q', p, p') = 
	case S.find (findReaction0 PM) PM of
	    SOME pm => SOME(pm, S.delete(PM, pm))
	  | NONE => NONE

    val reactStep = fn x => (print("(REACT)"); reactStep x)
    val worklistStep = fn x => (print("(WLIST)"); worklistStep x)
    val returnStep = fn x => (print("(RETURN)"); returnStep x)
    val matchStep = fn x => (print("(MATCH)"); matchStep x)

    fun step0 PMinit (pm as (PM, q, q', p, p') : elem, K0, S) i = 
	case (T.view p, T.view p') of 
            (T.VPar(p1,p2), _) => (* structural *) 
	    (Stack.push ((PM, q, q', p1, T.Par(p2, p')),K0) S, i)
	  | (T.VNil, T.VPar _) => (* structural *)
            (Stack.push ((PM, q, q', p', T.Nil),K0) S, i)
	  | (T.VNil, T.VPrefix _) => (* structural *)
            (Stack.push ((PM, q, q', p', T.Nil),K0) S, i)
          | (T.VPrefix(K, p), _) => (* match *)
	    matchStep PMinit (PM, q, q', (K,p), p') K0 S i
	  | (T.VNil, T.VNil) => (* return or worklist *)
	    if not(Stack.isEmpty S) then (* return *)
		let val ((pm',K'), S') = Stack.pop S
		in  returnStep PMinit (pm,K0) (pm',K') S' i
		end
            else (* worklist *)
		worklistStep PMinit pm K0 S i
	  | (T.VHole i, _) => raise Fail("step0: p is not ground")
	  | (_, T.VHole i) => raise Fail("step0: p' is not ground")
    fun step1 PMinit ((pm, K), S:stack) i =
	case findReaction pm of
	    SOME reaction => reactStep PMinit reaction pm K S i
	  | NONE => step0 PMinit (pm, K, S) i
		    
    fun step2 PMinit (S:stack, i) =
	if Stack.isEmpty S then NONE
	else SOME(step1 PMinit (Stack.pop S) i)
	     
    fun step PMinit (S, i) =
	let open Pretty
	    val _ = print "--> "
	    val st' = step2 PMinit (S,i)
	    fun pr (S,i) = 
		ppPrint ("  " ^+ pp(S,i)) (plainOutput("(*","*)")) 
			TextIO.stdOut
	    val _ = ( print "\n" ; Option.app pr st' )
	in  st'
	end

    fun initialState rules term : t =
	(Stack.push((S.init rules, T.Nil, T.Nil, term, T.Nil),C.ctrl("_top_",NONE,C.ACTIVE)) Stack.empty, 0)
end (* structure BAM State *)

