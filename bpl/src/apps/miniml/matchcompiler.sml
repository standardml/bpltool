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

(** Match compiler for MiniML.
 * Based on
 * - Peter Sestoft. "ML pattern match compilation and partial
 *     evaluation". In Danvy, Glück, and Thiemann (editors): Partial
 *     Evaluation. Dagstuhl Castle, Germany, February 1996. Lecture Notes in
 *     Computer Science, vol. 1110, pages 446-464. Springer-Verlag 1996.
 * with a little help from the Moscow ML sources and K. F. Larsens
 * implementation.
 *
 * @version $LastChangedRevision$
 * Modified: $Date: 2006/09/04 20:54:23 $ by: $Author: hniss $
 *)

structure MatchCompiler :> MATCHCOMPILER = struct

    structure M = MiniML

    type pat = Pattern.pat
    type con = Pattern.con
    structure ConSet = Pattern.ConSet

    type 'info exp = ('info, string*string) MiniML.exp
    type 'info access = 'info exp
    datatype 'info decision =
	     Success of 'info exp
	   | IfEq of 'info access * con * 'info decision * 'info decision
    datatype termd = Pos of con * termd list | Neg of ConSet.Set
    type context = (con * termd list) list
    datatype staticmatch = No | Yes | Maybe

    val Bot = Neg ConSet.empty
    fun bots n = List.tabulate(n, fn _ => Bot)

    fun staticmatch pcon (Pos(c,_)) = 
	if pcon = c then Yes else No
      | staticmatch pcon (Neg nonset) = 
	if ConSet.member pcon nonset then No
	else if Pattern.span pcon = 1 + ConSet.size nonset 
	then Yes
	else Maybe

    fun addneg(Neg nonset, con) = Neg(ConSet.insert' con nonset)
      | addneg(dsc, _) = dsc

    fun augment([], dsc) = []
      | augment((con,args)::rest,dsc) = (con, dsc::args) :: rest
    fun norm ((con,args) :: rest) = augment(rest, Pos(con, rev args))
      | norm _ = raise Fail"Peter, what to do here?"

    fun builddsc([], dsc, []) = dsc
      | builddsc((con,args)::rest, dsc, (_, _, dargs) :: work) =
	builddsc(rest, Pos(con, rev args @ (dsc :: dargs)), work)
      | builddsc _ = Util.abort 97343

    type pos = int * int
    exception NonExhaustiveMatch of pos
    fun compile1 pos caseexps allmrules =
	let open Pattern 
	    (* toplevel pattern lists *)
	    val noofpats = List.length caseexps
	    val topcon = TupleCon noofpats
	    val topctx = [(topcon, [])] : context

	    fun fail(dsc, []) = 
                ( raise NonExhaustiveMatch pos
                )
	      | fail(Pos(_, dscs), (pats1, rhs1) :: rulerest) =
		succeed(topctx, [(pats1, caseexps, dscs)], rhs1, rulerest)
	      | fail(_, _) = Util.abort 8765
	    and succeed(ctx, [], rhs, rules) = Success rhs
	      | succeed(ctx, work1::workr, rhs, rules) =
		  case work1 of
		      ([], [], []) => succeed(norm ctx, workr, rhs, rules)
		    | (pat1::patr, obj1::objr, dsc1::dscr) =>
		        match(pat1, obj1, dsc1, ctx,
			      (patr, objr, dscr) :: workr, rhs, rules)
		    | _ => Util.abort 45343
	    and match(PVar _, obj, dsc, ctx, work, rhs, rules) =
		  succeed(augment(ctx, dsc), work, rhs, rules)
	      | match(PCon(TupleCon arity,pargs),obj,dsc,ctx,work,rhs,rules) =
		  let fun args f = List.tabulate(arity, f)
		      fun getdargs (Neg _) = args (fn _ => Neg ConSet.empty)
			| getdargs (Pos(con,dargs)) = dargs
		      fun gettoargs () = args (fn i => M.Proj(i+1, obj))
		  in
		      succeed ((TupleCon arity, [])::ctx,
			       (pargs, gettoargs (), getdargs dsc)::work,
			       rhs, rules)
		  end
	      | match(PCon(pcon as (ConstCon{name=C,...}),pargs),obj,dsc,ctx,work,rhs,rules) =
		  let fun args f = List.tabulate(arity pcon, f)
		      fun getdargs (Neg _) = args (fn _ => Neg ConSet.empty)
			| getdargs (Pos(con,dargs)) = dargs
		      fun gettoargs () = args (fn i => M.Proj(i+1, M.Deconst(C, obj)))
		      fun succeed' () = 
			  succeed((pcon,[]):: ctx,
				  (pargs,gettoargs (),getdargs dsc)::work,
				  rhs, rules)
		      fun fail' newdsc = 
			  fail(builddsc(ctx, newdsc, work), rules)
		  in  case staticmatch pcon dsc of
			  Yes => succeed' ()
			| No => fail' dsc
			| Maybe => IfEq(obj, pcon, succeed'(), 
					fail' (addneg(dsc,pcon)))
		  end
	in  fail(Pos(topcon, bots noofpats), allmrules)
	end

    fun switchify dec =
	let open Pattern
	    fun collect get last cases 
			(otherwise as IfEq(obj, con, thn, els)) =
		if last = obj then collect get last ((get con,thn)::cases) els
		else (cases,otherwise)
	      | collect _ _ cases otherwise = (cases,otherwise)
	    and mk (IfEq(obj, ConstCon{name,...}, thn, els)) =
		let fun get (ConstCon{name,...}) = name
		      | get _ = Util.abort 34535
		    val (cases,otherwise) = collect get obj [] els
		in  M.Switch(obj, 
			     (name,mk thn)::
			       (List.map (fn (c,t) => (c, mk t)) cases),
			     mk otherwise
                            )
		end
	      | mk (Success rhs) = rhs
	      | mk _ = Util.abort 54654
	in  mk dec
	end

    fun compile getpos noinfo mkinfo (M.Prog binds) =
      let open Pattern
	  local 
	      val counter = ref 0
	  in
          fun fresh _ =
	      "_f" ^ Int.toString(!counter)
	      before
	      counter := !counter + 1
	  end
	fun selectCon C i exp = M.Proj(i,M.Deconst(C,exp))
	fun selectTup i exp   = M.Proj(i,            exp )
	fun extract top pat body =
	    let fun one select (pat, (i,exp)) =
		    (i-1,extract (select i top) pat exp)
	    in  case pat of
		    PVar id => M.Let(id,top,body)
		  | PCon(con as ConstCon({name=C,...}), pats) =>
		      #2(List.foldr (one (selectCon C)) (arity con,body) pats)
		  | PCon(con as TupleCon _,pats) =>
		      #2(List.foldr (one selectTup) (arity con,body) pats)
	    end

	fun comp (M.Info(info, exp)) =
	    ( case exp of
		  M.Var x => M.Var x
		| M.Unit => M.Unit
		| M.Integer i => M.Integer i
		| M.String s => M.String s
		| M.Const(C, e) => M.Const(C, comp e)
		| M.Abs(x,e) => M.Abs(x, comp e)
		| M.App(e1,e2) => M.App(comp e1, comp e2)
		| M.Fix(f,x,e) => M.Fix(f, x, comp e)
		| M.PrimOp(ope,e1,e2) => M.PrimOp(ope,comp e1,comp e2)
		| M.Let(x,e1,e2) => M.Let(x,comp e1, comp e2)
		| M.Tuple es => M.Tuple(List.map comp es)
		| M.Proj(i,e) => M.Proj(i, comp e)
		| M.Ref e => M.Ref(comp e)
		| M.DeRef e => M.DeRef(comp e)
		| M.Assign(e1,e2) => M.Assign(comp e1, comp e2)
		| M.Info(i,e) => M.Info(mkinfo i, comp e)
		| M.Deconst(C, e) => M.Deconst(C, comp e)
		| M.Switch(e, switch, default) =>
		  let fun mk1 (C,e) = (C, comp e)
		  in  M.Switch(comp e, List.map mk1 switch, comp default)
		  end
		| M.Case(e, match) => 
		  let val x = fresh()
		      val e' = comp e
		      fun mk1 (p,e) = ([p], extract (M.Var x) p (comp e))
		      val match' = List.map mk1 match
		      val dec = compile1 (getpos info) [M.Var x] match'
		  in  M.Let(x, e', switchify dec)
		  end
	     )
	  | comp exp = comp (M.Info(noinfo, exp))

	fun compilebind (M.ValBind(x,e)) = M.ValBind(x, comp e)
	  | compilebind (M.DatBind cb) = M.DatBind cb
	  | compilebind (M.TyBind tb) = M.TyBind tb
      in  M.Prog(List.map compilebind binds)
      end

    val dump_match_compile =
	Flags.makeBoolFlag{name="/dump/matchcompiled",short="",default=false,
			   long="dump-matchcompiled",arg="",
			   desc="Dump match-compiled MiniML program"}

    val compile = fn getpos => fn noinfo => fn mkinfo => fn prog =>
        let val prog' = compile getpos noinfo mkinfo prog
	in  if !dump_match_compile
	    then Dump.pretty (MiniML.pp' MiniML.ppPat) "match" prog'
	    else ()
          ; prog'
	end

end (* structure MatchCompiler *)
