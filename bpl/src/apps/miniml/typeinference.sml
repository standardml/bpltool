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

(** Simple implementation of a type inference algorithm.
 * @version $LastChangedRevision$
 * Modified: $Date: 2006/06/04 20:13:28 $ by: $Author: hniss $
 *)

structure TypeInference :> TYPEINFERENCE = struct

    structure TE = TypeExp
    structure M  = MiniML

    type pos = int * int
    type typeexp = TE.typeexp

    exception TypeError of pos * string list
    fun typeError(pos, tau1, tau2, reason) =
	raise TypeError(pos, ["Expression of type","  "^tau1,
			      "cannot have type","  "^tau2,
			      "("^reason^")"])

    structure StringSet = Util.StringSet
    structure StringMap = Util.StringMap

    (* typing contexts *)
    type var         = string
    type typename    = string
    type constructor = string

    type constructorset = StringSet.Set
    type tynamemap   = (* typename -> *) constructorset StringMap.map
    type environment = (* variable -> *) TE.typescheme StringMap.map
    type context     = environment * tynamemap 

    val emptyCtx = (StringMap.empty, StringMap.empty)

    fun var pos (env,_) x = 
	case StringMap.lookup env x of
	    NONE => raise TypeError(pos, ["Unbound identifier " ^ x])
	  | SOME sigma => sigma
    fun bind1 ((x,v), (env,tymap)) = 
	(StringMap.add(x, TE.promote v, env), tymap)
    fun bind1' ((x,v), (env,tymap)) = 
	(StringMap.add(x, v, env), tymap)
    fun bind (ctx:context) bnds = List.foldr bind1 ctx bnds
    fun bind' (ctx:context) bnds = List.foldr bind1' ctx bnds

    fun unifyExp pos tau1 tau2 =
	let (* We need to do this prior to unification, as that
             * destructively updates the types - even in case of
             * a mismatch. *)
	    val t1 = TE.toString tau1
	    val t2 = TE.toString tau2
	in  TE.unify(tau1,tau2) 
	    handle TE.Unify reason =>
		   typeError(pos, t1, t2, reason)
	end

    (* infProg uses algorithm M (a top-down version of algorithm W)
          to infer types for the expressions in the program. The
          basic idea is to pass down alongside the expression a 
          type constraint specifying a which type we need to use
          the expressions. See Lee & Yi, ``Proofs about a Folklore
          Let-Polymorphic Type Inference Algorithm'', TOPLAS 98,
          for details.
    *)
    fun infProg mkinfo noinfo getpos (M.Prog binds) = 
	let fun inf' pos (ctx as (env,tymap)) exp exp_tau =
	    case exp of
		M.Var x =>
		    let val tau = TE.instance(var pos ctx x)
			val _ = unifyExp pos tau exp_tau
		    in  M.Var x
		    end
	      | M.Unit =>
		    let val _ = unifyExp pos (TE.unitty()) exp_tau
		    in  M.Unit
		    end
	      | M.Integer i => 
		    let val _ = unifyExp pos (TE.intty()) exp_tau
		    in  M.Integer i
		    end
	      | M.String s => 
		    let val _ = unifyExp pos (TE.stringty()) exp_tau
		    in  M.String s
		    end
	      | M.PrimOp(ope, e1, e2) =>
		    if M.isRelational ope 
		    then let val _ = unifyExp pos (TE.boolty()) exp_tau
			     val alpha = TE.fresh ()
			     val e1' = inf ctx e1 alpha
			     val e2' = inf ctx e2 alpha
			     (*val _ = numericConstraint pos alpha*)
			 in  M.PrimOp(ope, e1', e2')
			 end
		    else let val _ = unifyExp pos (TE.intty()) exp_tau
     	                     val e1' = inf ctx e1 exp_tau
			     val e2' = inf ctx e2 exp_tau
			     (*val _ = numericConstraint pos exp_tau*)
			 in  M.PrimOp(ope, e1', e2')
			 end
	      | M.Let(x,e1,e2) =>
		    let val _ = TE.pushLevel()
			val x_tau = TE.fresh()
			val e1' = inf ctx e1 x_tau
			val _ = TE.popLevel()
			val e2' = inf (bind' ctx [(x,TE.generalize  x_tau)]) 
				      e2 exp_tau
		    in  M.Let(x,e1',e2')
		    end
	      | M.Abs(x, e) =>
                    let val (alpha,beta) = (TE.fresh(), TE.fresh())
			val _ = unifyExp pos (TE.arrowty(alpha,beta)) exp_tau
			val e' = inf (bind ctx [(x,alpha)]) e beta
		    in  M.Abs(x,e')
		    end
	      | M.Fix(f, x, e) =>
		    let val (alpha,beta) = (TE.fresh(), TE.fresh())
			val fun_tau = TE.arrowty(alpha,beta)
			val _ = unifyExp pos fun_tau exp_tau
			val e' = inf (bind ctx [(f,fun_tau),(x,alpha)]) e beta
		    in  M.Fix(f, x, e')
		    end
	      | M.App(e1,e2) =>
		    let val beta = TE.fresh()
			val fun_tau = TE.arrowty(beta, exp_tau)
			val e1' = inf ctx e1 fun_tau
(*
			val (arg_tau,res_tau) = (TE.fresh(),TE.fresh())
			val _ = unifyExp (getpos (#1 e1))
					 fun_tau (TE.arrow(arg_tau,res_tau))
			val _ = unifyExp pos res_tau exp_tau
*)
			val e2' = inf ctx e2 beta
		    in  M.App(e1',e2')
		    end
	      | M.Ref e =>
		    let val alpha = TE.fresh()
			val _ = unifyExp pos (TE.refty alpha) exp_tau
			val e' = inf ctx e alpha
		    in  M.Ref e'
		    end
	      | M.DeRef e =>
		    let val e' = inf ctx e (TE.refty exp_tau)
		    in  M.DeRef e'
		    end
	      | M.Assign(e1,e2) =>
		    let val _ = unifyExp pos (TE.unitty()) exp_tau
			val alpha = TE.fresh()
			val e1' = inf ctx e1 (TE.refty alpha)
			val e2' = inf ctx e2 alpha
		    in  M.Assign(e1',e2')
		    end
	      | M.Info(i, e) => inf ctx e exp_tau
	      | M.Const(C, e) =>
		    let val tau = TE.instance (var pos ctx C)
			val (alpha,beta) = (TE.fresh(), TE.fresh())
			val _ = unifyExp pos tau (TE.arrowty(alpha,beta))
			val e' = inf ctx e alpha
			val _ = unifyExp pos beta exp_tau
		    in  M.Const(C, e')
		    end
	      | M.Deconst(C, e) =>
		    let val tau = TE.instance (var pos ctx C)
			val (alpha,beta) = (TE.fresh(), TE.fresh())
			val _ = unifyExp pos tau (TE.arrowty(alpha,beta))
			val e' = inf ctx e beta
			val _ = unifyExp pos alpha exp_tau
		    in  M.Deconst(C, e')
		    end
	      | M.Tuple es =>
		    let val alphas = List.map TE.fresh es
			val tau = TE.tuplety alphas
			val _ = unifyExp pos tau exp_tau
			val es' = List.map (fn (e,a) => inf ctx e a)
				           (ListPair.zip(es,alphas))
		    in  M.Tuple(es')
		    end
	      | M.Proj(i, e) =>
		    let val alpha = TE.fresh()
			val e' = inf ctx e alpha
			val tau =
			    case TE.view alpha of
				TE.Tuple arity => 
				  if i <= arity 
				  then let val alphas = List.tabulate(arity,TE.fresh)
					   val _ = unifyExp pos (TE.tuplety alphas) alpha
				       in  List.nth(alphas,i-1)
				       end
				  else typeError(pos, TE.toString alpha,
						 TE.toString(TE.tuplety(List.tabulate(i,TE.fresh))),
						 "not enough components")
			      | _ => raise TypeError(pos, ["Unresolved tuple pattern"])
			val _ = unifyExp pos tau exp_tau
		    in  M.Proj(i,e')
		    end
	      | M.Case(e, match) =>
		    let val tau = TE.fresh()
			val e' = inf ctx e tau
			fun one (pat, exp) =
			    let val (pat', ctx') = infPat pos ctx pat tau
				val exp' = inf ctx' exp exp_tau
			    in  (pat', exp')
			    end
		    in  M.Case(e', List.map one match)
		    end
	      | M.Switch(e, switch, default) =>
		    let val tau = TE.fresh()
			val e' = inf ctx e tau
			fun one (C, exp) =
			    let val (alpha,beta) = (TE.fresh(),TE.fresh())
				val _ = unifyExp pos (TE.arrowty(alpha,beta))
						 (TE.instance (var pos ctx C))
				val _ = unifyExp pos beta tau
				val exp' = inf ctx exp exp_tau
			    in  (C, exp')
			    end
			val default' = inf ctx default exp_tau
		    in  M.Switch(e', List.map one switch, default')
		    end

	    and inf ctx (M.Info(info, exp)) exp_tau =
		let val pos = getpos info
		    val exp' = inf' (getpos info) ctx exp exp_tau
		in  M.Info(mkinfo info exp_tau, exp')
		end
	      | inf ctx exp exp_tau = 
		( inf ctx (M.Info(noinfo, exp)) exp_tau)

	    and infPat pos ctx pat pat_tau =
		case pat of
		    Pattern.PVar id => (pat, bind ctx [(id,pat_tau)])
		  | Pattern.PCon(Pattern.TupleCon ar, pats) =>
		      let val _ = if List.length pats = ar then ()
				  else raise TypeError(pos, ["Wrong number of components in tuple pattern"])
			  val alphas = List.tabulate(ar, TE.fresh)
			  val _ = unifyExp pos (TE.tuplety alphas) pat_tau
			  fun one ((p,a), (pats,ctx)) =
			      let val (p',ctx') = infPat pos ctx p a
			      in  (p'::pats, ctx')
			      end
			  val (pats', ctx') = 
			      List.foldl one ([],ctx) 
					 (ListPair.zip(pats,alphas))
		      in  (Pattern.PCon(Pattern.TupleCon ar, pats'), ctx')
		      end
		  | Pattern.PCon(Pattern.ConstCon{name,arity,span}, pats) =>
		      let val (alpha,beta) = (TE.fresh(),TE.fresh())
			  val con_tau = TE.arrowty(alpha,beta)
			  val _ = unifyExp pos con_tau (TE.instance (var pos ctx name))
			  val _ = unifyExp pos beta pat_tau
			  val pat_taus =
			      if arity > 1 then 
				  let val alphas = List.tabulate(arity, TE.fresh)
				      val _ = unifyExp pos (TE.tuplety alphas)
					               alpha
				  in  alphas
				  end
			      else [alpha]
			  fun one ((p,t), (pats,ctx)) =
			      let val (p',ctx') = infPat pos ctx p t
			      in  (p'::pats, ctx')
			      end
			  val (pats', ctx') = 
			      List.foldl one ([],ctx)
			                 (ListPair.zip(pats,pat_taus))
		      in  (Pattern.PCon(Pattern.ConstCon{name=name,arity=arity,span=span}, pats'), ctx')
		      end

	    fun infDatbind ((Tn, targs, Cs), (env,tynames)) =
		case StringMap.lookup tynames Tn of
		    NONE =>
		      let val cs = StringSet.fromList 
				       (List.map (fn M.Con(C,_) => C) Cs)
			  val tynames' = StringMap.add(Tn, cs, tynames)
			  fun f (M.Con(C,ty), ctx) =
			      let 
				  val _ = TE.pushLevel()
				  val alphas = List.map TE.freshVar targs
				  val map = TE.mkMap (ListPair.zip (targs,alphas))
				  val tau = TE.tyconty(Tn, alphas)
				  val c_tau = TE.arrowty(TE.fromAST map ty,tau)
				  val _ = TE.popLevel()
			      in  bind' ctx [(C, TE.generalize c_tau)]
			      end
			  val ctx' = List.foldl f (env,tynames') Cs
		      in  ctx'
		      end
		  | SOME _ =>
		      raise TypeError((0,0), ["Redeclaration of type " ^ Tn])
		
		
	    fun infBind (M.ValBind(x,e), (binds,ctx)) =
		let val _ = TE.pushLevel()
		    val alpha = TE.fresh()
		    val e' = inf ctx e alpha
		    val _ = TE.popLevel()
		in  (M.ValBind(x,e') :: binds, bind' ctx [(x,TE.generalize alpha)])
		end
	      | infBind (M.DatBind datbind, (binds,ctx)) =
		let val ctx' = infDatbind (datbind, ctx)
		in  (M.DatBind datbind :: binds, ctx')
		end
	      | infBind (M.TyBind typeabbrev, (binds,ctx)) =
		(M.TyBind typeabbrev :: binds, ctx)

	in  M.Prog(rev(#1(List.foldl infBind ([],emptyCtx) binds)))
	end

    val dump_typed =
	Flags.makeBoolFlag{name="/dump/typed",short="",default=false,
			   long="dump-typed",arg="",
			   desc="Dump MiniML program after type inference"}

    val inference = fn noinfo => fn getpos => fn prog =>
	let val prog' = infProg (fn i1 => fn tau => (i1,tau)) noinfo getpos prog
	    fun pp (_,tau) = Pretty.++(Pretty.ppString ":",  TE.pp tau)
	in  if !dump_typed
	    then Dump.pretty (MiniML.pp pp Pattern.ppPat) "typed" prog'
	    else ()
	  ; prog'
	end

end (* structure TypeInference *)
