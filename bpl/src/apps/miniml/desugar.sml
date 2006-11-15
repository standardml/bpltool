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

structure Desugar :> DESUGAR = struct

    (* Eventually I plan to move all desugaring here. For now we
       implement:

        * tuples to pairs of pairs, etc.
       [* we also resolve span and arity of constructors used by the
          match compiler.]

     *)
    structure M = MiniML
    structure P = Pattern 

    type 'a prog = ('a,Pattern.pat) MiniML.prog

    structure Map = Util.StringMap
    val lookup = Map.lookup
    val add = Map.add
    val empty = Map.empty

(*
    (* Represent tuples as (e1, (e2, (e3, ... (en-1, en)))). 
       Projections then become #1(#2(#2( ... #2 e))).
     *)
    fun mkPairs [] = Util.abort 58293
      | mkPairs es = 
	let val (en,es) = case rev(es) of
			       [] => Util.abort 58292
			     | en::es => (en,rev es)
	in  List.foldr (fn (e, rest) => M.Tuple[e,rest]) en es
	end
    fun mkProj i e = if i <= 2 then M.Proj(i, e)
		     else mkProj (i-1) (M.Proj(2,e))
*)

    fun removeInfo (M.Info(i, e)) = removeInfo e
      | removeInfo e = (NONE, e)

    fun desugar prog =
	let 
	    fun mk map (P.TupleCon i) = (P.TupleCon i)
	      | mk map (P.ConstCon{name,arity,span}) =
		  let val {arity,span} = case lookup map name of
					     SOME info => info
					   | NONE => raise Fail("No info for " ^ name)
		  in  P.ConstCon{name=name,arity=arity,span=span}
		  end
	    fun loopPat map (P.PVar x) = P.PVar x
	      | loopPat map (P.PCon(c, ps)) = P.PCon(mk map c, 
						     List.map (loopPat map) ps)
	    fun loopP map (pat, e) = (loopPat map pat, loopE map e)
	    and loopPs map ps = List.map (loopP map) ps
	    and loopE map exp =
		case exp of
		    (* fn x => case x of y => e  -->  fn y => e *)
		    M.Abs(x, cas) =>
		      let val (info, exp) = removeInfo cas
		      in  case exp of
			      M.Case(M.Var x', [(P.PVar y,e)]) =>
			        if x = x' then loopE map (M.Abs(y, e))
				else M.Abs(x, M.Case(M.Var x',[(P.PVar y, loopE map e)]))
			    | exp => M.Abs(x, loopE map exp)
		      end
		  | M.Fix(f, x, cas) =>
		      let val (info, exp) = removeInfo cas
		      in  case exp of
			      M.Case(M.Var x', [(P.PVar y,e)]) =>
			        if x = x' then loopE map (M.Fix(f, y, e))
				else M.Fix(f, x, M.Case(M.Var x',[(P.PVar y, loopE map e)]))
			    | exp => M.Fix(f, x, loopE map exp)
		      end
		  | M.Var x => M.Var x
		  | M.Integer i => M.Integer i
		  | M.String s => M.String s
		  | M.Unit => M.Unit
		  | M.Const(C, e) => M.Const(C, loopE map e)
		  | M.App(e1,e2) => M.App(loopE map e1, loopE map e2)
		  | M.Case(e, ps) => M.Case(loopE map e, loopPs map ps)
		  | M.PrimOp(ope,e1,e2) => M.PrimOp(ope, loopE map e1, loopE map e2)
		  | M.Let(x,e1,e2) => M.Let(x, loopE map e1, loopE map e2)
		  | M.Tuple es => M.Tuple(loopEs map es)
		  | M.Proj(i, e) => M.Proj(i, loopE map e)
		  | M.Ref e => M.Ref(loopE map e)
		  | M.DeRef e => M.DeRef(loopE map e)
		  | M.Assign(e1,e2) => M.Assign(loopE map e1, loopE map e2)
		  | M.Info(i, e) => M.Info(i, loopE map e)
		  | _ => Util.abort 2345
	    and loopEs map es = List.map (loopE map) es
	    fun loopB (M.ValBind(x,e), (bs,map)) =
		(M.ValBind(x,loopE map e)::bs, map)
	      | loopB (M.TyBind(t,targs,ty), (bs,map)) = 
		(M.TyBind(t,targs,ty)::bs, map)
	      | loopB (M.DatBind(t,targs,cbs), (bs,map)) =
		let val sp = List.length cbs
		    fun ar (M.TyTuple ts) = List.length ts
		      | ar _ = 1
		    fun f (M.Con(C, ty), map) = 
			add(C, {arity=ar ty,span=sp}, map)
		    val map = List.foldl f map cbs
		in  (M.DatBind(t,targs,cbs)::bs, map)
		end
	    val M.Export(exports,bs) = prog
	in  M.Export(exports,rev(#1(List.foldl loopB ([], empty) bs)))
	end

end (* structure Desuard *)
