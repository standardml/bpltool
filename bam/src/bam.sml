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

    (* convenience shorthands *)
    type term = int option Term.t
    type rule = int option Rule.t

    structure IntSet = struct
       open Rbset
       type t = int set
       val empty = empty Int.compare
       val singleton = singleton String.compare
    end

    structure PartialMatch : sig
       type t

       val pmatch : term Stack.t * term * term vector * IntSet.t -> t

       val LHS : t -> term Stack.t
       val RHS : t -> term
       val parameter : t -> term vector
       val indices : t -> IntSet.t

       val compare : t * t -> order

       val initParam : rule -> term vector
       val param : int -> t -> term
       val plug : int * term -> t -> t
       val popLHS : t -> term * t

       val toplevel : t -> bool
       val returnable : t -> bool
       val returnableHole : t -> int option
       val reactable : t -> bool

       val pp : t Pretty.pp
    end = struct
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
                 (*, ppString "..."*)
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

    structure BAMState : sig
       type t
       val step : PartialMatchSet.t -> t -> t option
       val initialState : rule Rbset.set -> term -> t
    end = struct

       (* convenience *)
       structure S = PartialMatchSet 
       structure P = PartialMatch
       structure T = Term
       structure C = Control

       type elem = S.t * term * term * term * term
       type stack = (elem * (int option Control.t)) Stack.t

       type t = stack * int

       fun ppElem (PM, q, q', p, p') =
	   Pretty.bracket "(#)"
	     (Pretty.clist "#, " (fn t => t)
	        (Util.ppSet P.pp PM :: (List.map (Term.pp' Control.pp) [q,q',p,p'])))

       fun pp (S: stack, i) = 
	   let val S' = Stack.take 3 S
	   in  Pretty.bracket "(#)" (Pretty.ppBinary(Pretty.ilist "»»#" (ppElem o #1) S', ", ", Pretty.ppInt i))
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
			       val pm' = (LHS', P.RHS pm, P.parameter pm,
					  IntSet.add(P.indices pm, i))
			   in  P.plug(j,T.plug1(j,T.Prefix(K,p))(P.param j pm)) pm'
			   end
		   in  S.mapPartial (Option.map f o ifLHSMatch pat) PM
		   end 
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
				      SOME j => (print("plugging " ^ Int.toString j); SOME(P.plug (j, T.plug1 (j, T.Nil) (P.param j pm)) (#2(P.popLHS pm))))
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

       fun reactStep PMinit ((_, P, D, I), PM') (_, q, q', p, p') K0 (S:stack) i =
	   let val res = T.plug D P
	       val react = (S.minus(PM', I),
			    termminus(q, I), T.Par(res, termminus(q', I)),
			    p, p')
	   in  (Stack.push (react,K0) S, i)
	   end

       fun findReaction0 PM0 pm0 =
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
			    end
		   val PM'overlapping = S.mapPartial (ifPred f) PM'reactable
	       in  S.isEmpty PM'overlapping
	       end
	   else
	       false
       fun findReaction (PM, q, q', p, p') =
	   case S.find (findReaction0 PM) PM of
	       SOME pm => SOME(pm, S.delete(PM, pm))
	     | NONE => NONE

       val reactStep = fn x => (print("REACT"); reactStep x)
       val worklistStep = fn x => (print("WLIST"); worklistStep x)
       val returnStep = fn x => (print("RETURN"); returnStep x)
       val matchStep = fn x => (print("MATCH"); matchStep x)

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

       fun step PMinit (S:stack, i) =
	   if Stack.isEmpty S then NONE
	   else SOME(step1 PMinit (Stack.pop S) i)

       val step = fn PMinit => fn (S, i) =>
 	   let val st' = step PMinit (S,i)
	       fun pr (S,i) = ( print "\n  " ; print(Pretty.ppToString(pp(S,i))) )
	       val _ = ( pr (S,i) ; print "\n-->" ; Option.app pr st'; print "\n\n")
	   in  st'
	   end 
(*
	   case step PMinit (S,i) of
	       SOME (S', i') =>
	          ( print "\n"
		  ; print(Pretty.ppToString(pp (S,i)))
		  ; print"\n"
		  ; print(Pretty.ppToString(pp (S',i')))
		  ; print"\n"
		  before
		    SOME (S',i')
                  )
	     | NONE => 
	          ( print "\n"
		  ; print(Pretty.ppToString(pp (S,i)))
		  ; print"\n"
                  before 
                    NONE
                  )
*)
       fun initialState rules term : t =
	   (Stack.push((S.init rules, T.Nil, T.Nil, term, T.Nil),C.ctrl("_top_",NONE,C.ACTIVE)) Stack.empty, 0)
    end (* structure BAM State *)

    fun rewrite rules term =
	let val mkc = fn _ => NONE
	    val term' = Term.map mkc term
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

fun run file =
    let val (rules, agent) = MiniBPLParser.parseFile file
	val rules' = Rbset.addList(Rbset.empty Rule.compare, rules)
    in  BAM.rewrite rules' agent
    end

val _ = case CommandLine.arguments() of
	    [file] => run file
	  | _ => raise Fail("Expected a single argument")
