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

structure Term :> TERM = struct

    datatype 'a t =
	     TPar of 'a t list
           | TPrefix of 'a Control.t * 'a t
           | TNil
	   | THole of int

    fun compare (p1, p2) =
	case (p1, p2) of
	    (TNil, TNil) => EQUAL
	  | (TNil, _)    => LESS
	  | (THole _, TNil) => GREATER
	  | (THole i1, THole i2) => Int.compare(i1,i2)
	  | (THole _, _) => LESS
	  | (TPrefix _, TNil) => GREATER
	  | (TPrefix _, THole _) => GREATER
	  | (TPrefix(C1,p1'), TPrefix(C2,p2')) => 
	       (case String.compare(Control.name C1, Control.name C2) of
		    EQUAL => compare (p1', p2')
		  | order => order)
	  | (TPrefix _, _) => LESS
	  | (TPar p1s, TPar p2s) => Util.listCmp compare (p1s,p2s)
	  | (TPar _, _) => GREATER

    fun existsi P p =
	case p of
	    TPar ps => List.exists (existsi P) ps
	  | TPrefix(C, p) => if Control.predi P C then true else existsi P p
	  | TNil => false
	  | THole _ => false
    fun exists P = existsi (fn (n,i,a) => P i)

    val Nil = TNil
    val Hole = THole
    val Prefix = TPrefix
    fun Par (p1, p2) = 
	case toplevels p1 @ toplevels p2 of
	    [] => TNil
	  | [p] => p
	  | ps => TPar ps
    and toplevels p =
	case p of
	    TPar ps => ps
	  | TNil => [] (* Nil should be elided from TPar lists *)
	  | _ => [p]

    fun map f p = 
	case p of
	    TNil => TNil
	  | THole i => THole i
	  | TPrefix(C, p) => TPrefix(Control.map f C, map f p)
	  | TPar ps => TPar(List.map (map f) ps)

    structure IntSet = struct
       open Rbset
       type t = int set
       val empty = empty Int.compare
       val singleton = singleton String.compare
    end

    fun holes0 (p,H) =
	case p of
	    THole i => IntSet.add(H,i)
	  | TNil => H
	  | TPrefix(_, p) => holes0 (p, H)
	  | TPar ps => List.foldl holes0 H ps
    fun holeIndices p = holes0 (p, IntSet.empty)
    fun maxHoleIndex p = valOf(IntSet.max(holeIndices p))

    fun plug plugs P =
	let val holes = Vector.foldli (fn (i,p,s) => IntSet.add(s,i)) 
				      IntSet.empty (plugs,0,NONE)
	    fun lookup i = Vector.sub(plugs,i)
	    fun plug0 P =
		case P of
		    THole i => if IntSet.member(holes,i) 
			       then (true, lookup i) else (false, P)
		  | TNil => (false, P)
		  | TPrefix(C, P') => 
		    let val (new, P'') = plug0 P'
		    in  if new then (true, Prefix(C, P'')) else (false, P)
		    end
		  | TPar Ps =>
		    let val Ps' = List.map plug0 Ps
			val new = List.exists #1 Ps'
		    in  if new 
			then (true,List.foldl (fn ((_,P),Ps) => Par(P,Ps)) Nil Ps')
			else (false,P)
		    end
	in  if Rbset.isEmpty holes then P (* since we are not plugging anything,
					     there is no reason to traverse P *)
	    else #2(plug0 P)
	end
    fun plug1 (j, p) P =
	let val plugs = Vector.tabulate(j, fn i => if i=j then p else Hole i)
	in  plug plugs P
	end

    datatype 'a view =
	     VPar of 'a t * 'a t
           | VPrefix of 'a Control.t * 'a t
           | VNil
	   | VHole of int

    fun view p =
	case p of 
	    TPar([]) => raise Fail("Shouldn't happen: view with empty TPar")
	  | TPar(p::ps) => VPar(p, TPar ps)
	  | TPrefix(C, p) => VPrefix(C, p)
	  | TNil => VNil
	  | THole i => VHole i

    datatype 'a pattern =
	     PSuccess
	   | PVar of string
	   | PPar of 'a pattern * 'a pattern
	   | PPrefix of 'a Control.t * 'a pattern
	   | PPrefixed of string * 'a pattern
	   | PHole of int
	   | PHoled of string
	   | PNil

    fun toplevel_pats0 (P,acc) =
	case P of
	    PPar(P1, P2) => toplevel_pats0 (P2, toplevel_pats0 (P1,acc))
	  | _ => P::acc
    fun toplevel_pats (P as PPar _) = rev(toplevel_pats0 (P, []))
      | toplevel_pats P = [P]

    structure M = Util.StringMap
    type 'a match = 'a t M.map * 'a Control.t M.map * int M.map
    val empty = (M.empty, M.empty, M.empty)
    fun extend_term (pmap,cmap,hmap) V p  = (M.add(V,p,pmap),cmap,hmap)
    fun extend_ctrl (pmap,cmap,hmap) VC C = (pmap,M.add(VC,C,cmap),hmap)
    fun extend_hole (pmap,cmap,hmap) Vi i = (pmap,cmap,M.add(Vi,i,hmap))
    fun lookup_term (pmap,cmap,hmap) V = M.lookup pmap V
    fun lookup_ctrl (pmap,cmap,hmap) VC = M.lookup cmap VC
    fun lookup_hole (pmap,cmap,hmap) Vi = M.lookup hmap Vi

    fun match1 map P p =
	case (P, p) of
	    (PSuccess, _) => SOME map
	  | (PVar V, _) => SOME(extend_term map V p)
	  | (PNil, TNil) => SOME map
	  | (PPrefix(PC, P'), TPrefix(C, p')) =>
	       if Control.name PC = Control.name C then match1 map P' p'
	       else NONE
	  | (PPrefixed(VC, P'), TPrefix(C, p')) =>
	       match1 (extend_ctrl map VC C) P' p'
	  | (PHole Pi, THole i) => if Pi = i then SOME map else NONE
	  | (PHoled Vi, THole i) => SOME(extend_hole map Vi i) 
	  | (P as PPar _, p) => 
	       let val ps = toplevels p
		   val Ps = toplevel_pats P
	       in  matchAll map Ps ps
	       end
	  | _ => NONE
    and matchInMany map PSuccess ps = SOME(map, [])
      | matchInMany map (PVar V) ps = SOME(extend_term map V (Par(TNil,TPar ps)),[])
      | matchInMany map P ps =
	let fun loop [] nomatch = NONE
	      | loop (p::ps) nomatch =
		   (case match1 map P p of
			SOME map => SOME(map, ps@nomatch)
		      | NONE => loop ps (p::nomatch))
	in  loop ps []
	end
    and matchAll map Ps ps =
	let fun loop map [] [] = SOME map
	      | loop map [] remaining = NONE
	      | loop map (P::Ps) remaining = 
		  (case matchInMany map P remaining of
		       SOME(map, remaining) => loop map Ps remaining
		     | NONE => (* FIXME: should really backtrack *) NONE)
	in  loop map Ps ps
	end
    fun match P p = match1 empty P p
    fun lookup map v = lookup_term map v
    fun lookupCtrl map vc = lookup_ctrl map vc
    fun lookupHole map vi = lookup_hole map vi

    open Pretty
    fun pp0 ppCtrl lev p =
	case p of
	    TNil => ppString "1"
	  | THole i => "[" ^+ ppInt i +^ "]"
	  | TPrefix(C, TNil) => ppCtrl C
	  | TPrefix(C, p) => break(0,0)(ppCtrl C +^ ".", pp0 ppCtrl 1 p)
	  | TPar ps => (if lev>0 then bracket "(#)" else fn tree => tree) 
			   (clist " |# "(pp0 ppCtrl 0) ps)

    val pp = pp0 Control.pp 0
    val pp' = fn ppCtrl => pp0 ppCtrl 0
end (* structure Term *)

(*
structure TermTest = struct

    open Term

    (* matches *)
    fun runMatchTest (t, P, p, expected) =
	let fun checkBindings M b =
		let fun loop [] = ()
		      | loop ((v,p)::ps) =
			   let val SOME p' = lookup M v
			   in  if compare(p,p') = EQUAL then ()
			       else print(t^": expected result not found for "^v^"\n")
			   end handle Bind => print(t^": variable "^v^" unbound\n")
		in  loop b
		end
	    val M = match P p
	in  case (M, expected) of
		(NONE, NONE) => ()
	      | (SOME M, SOME b) => checkBindings M b
	      | (SOME _, NONE) => print(t^": expected failure, got success\n")
	      | (NONE, SOME _) => print(t^": expected success, got failure\n")
	end
    val (C1, C2) = ( ("C1", ()) , ("C2", ()) )

    val C1Pat = PPrefix(C1, PVar "p1")
    val C2Pat = PPrefix(C2, PVar "p2")
    val C1PatNil = PPrefix(C1, PNil)
    val C2PatNil = PPrefix(C2, PNil)
    val C1ParPat = PPar(C1Pat, PVar "p")

    val C1Term = Prefix(C1, Nil)
    val C2Term = Prefix(C2, Nil)
    val larger = Prefix(C1,Par(C2Term,C2Term))
    val large  = Par(C2Term,C1Term)
    val test = [ ("t1", C1ParPat, C1Term, SOME[("p1", Nil), ("p", Nil)])
               , ("t2", C1ParPat, Prefix(C1,Par(C2Term,C2Term)), SOME[("p1", Par(C2Term,C2Term)), ("p", Nil)])
               , ("t3", C1ParPat, Par(C2Term,C1Term), SOME[("p1", Nil), ("p", C2Term)])
	       , ("t4", PPar(C1PatNil,C1PatNil), C1Term, NONE)
               , ("t5", PPar(C1PatNil,C1PatNil), Par(C1Term,C2Term), NONE)
	       , ("t6", PPar(C1PatNil,C1PatNil), Par(C1Term,C1Term), SOME[])
               , ("t7", PPar(C1PatNil,PSuccess), Par(C2Term,C1Term), SOME[])
               , ("t8", PPar(C1PatNil,PPar(C1PatNil,PSuccess)), Par(C1Term,Par(C2Term,C1Term)), SOME[])
               ]

    fun run() = List.app runMatchTest test

    val _ = if compare(larger, large) = EQUAL then print "Failure\n"
	    else print "Success\n"

    val _ = if not(compare(larger, larger) = EQUAL) then print "Failure\n"
	    else print "Success\n"

end (* structure TermTest *)

val _ = TermTest.run()
*)
