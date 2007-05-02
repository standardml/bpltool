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

(*
 Mapping from BPL abstract syntax tree to BgVal.
*)

open TextIO; (* for writing output *)

structure BG = BG (structure ErrorHandler = PrintErrorHandler);
structure B = BG.BgVal
structure S = BG.Sugar
(*
structure P = BG.Permutation
structure NameSet = BG.NameSet
structure Ion     = BG.Ion
structure Name    = BG.Name
structure Control = BG.Control
structure Wiring  = BG.Wiring
structure Link    = BG.Link
structure LinkSet = BG.LinkSet
structure R       = BG.Rule
structure Bdnf    = BG.BgBDNF
structure M       = BG.Match
structure Re = Reaction (structure RuleNameMap = Util.StringMap
                         structure Info = BG.Info
                         structure Interface = BG.Interface
                         structure Wiring = BG.Wiring
                         structure BgVal = BG.BgVal
                         structure BgBDNF = BG.BgBDNF
                         structure Match = BG.Match
                         structure Instantiation = BG.Instantiation
                         structure Rule = BG.Rule
                         structure Origin = Origin
                         structure ErrorHandler = PrintErrorHandler)
*)

datatype id = Id of string
	    | CtrlId of string
	    | NamedSite of string
	    | Int of int
datatype epsilon = Eps
datatype wire = IdId of Id * Id
	      | IdS of Id
	      | SId of Id
datatype wirelist = Wirs of wire * wirelist
		  | epsilon
datatype wiring = wirelist
datatype ctrlid = CtrlId
datatype edges = Edgs of edges * Id
	       | Id
datatype edgelist = edges
		  | epsilon
datatype ports = Ports of edgelist
datatype bigraph = wiring
		 | Par of bigraph * bigraph
		 | Pri of bigraph * bigraph
		 | Com of bigraph * bigraph
		 | Emb of bigraph * bigraph
		 | Ten of bigraph * bigraph
		 | CtrlId1
		 | CtrlIdB of CtrlId * edgelist
		 | CtrlIdF of CtrlId * edgelist
		 | CtrlIdBF of CtrlId * edgelist * edgelist
		 | Clo of edgelist * bigraph
		 | Abs of * edgelist * bigraph
		 | Site of Int
		 | Sitep of Int * ports
		 | Nsite of NamedSite
		 | Nsitep of NamedSite * ports
		 | Id
		 | Empty
datatype rule = Rule of bigraph * bigraph
datatype ctrlkind = Active
		  | Passive
		  | Atomic
datatype ctrldef = CtrlId * ctrlkind * Int * Int
datatype ctrldefs = ctrldefs * ctrldef
		  | ctrldef
datatype sig = Sig of ctrldefs
datatype dec = dec * dec
	     | Rule of Id * rule
	     | Value of Id * bigraph
datatype prog = dec

val info = BG.Info.noinfo
val empty = NameSet.empty

(* operators *)
infixr || (* parallel product *)
infixr pp (* prime product *)
infixr tt (* tensor product *)
infixr oo (* composition *)
infixr ++ (* add a name to a nameset not containing it already *)
fun (b1:bgval) || (b2:bgval) = B.Par info [b1,b2]
fun (b1:bgval) pp (b2:bgval) = B.Pri info [b1,b2]
fun (b1:bgval) tt (b2:bgval) = B.Ten info [b1,b2]
fun (b1:bgval) oo (b2:bgval) = B.Com info (b1, b2)

(* This translation should not be altered, just change the generating
functions above to match change in BG signatures. *)

fun bpl2bgval (X:nameset) tree =
    case tree of
	M.Var x =>
	let val x' = v2n x in
	    if NameSet.member x' X
	    then (VAR x') tt (make_onames (NameSet.remove x' X))
	    else raise Fail ("Unbound variable " ^ x)
	end
      | M.Integer i => Util.abort 19844
      (*(VAL tt id X) oo (INT i tt make_onames X)*)
      | M.String s => Util.abort 19845
      | M.Const(C, e) => (CONST C tt id X) oo (exp2bg X e)
      | M.Abs(x, e) =>
	let val x' = v2n x in
	    (VAL tt id X) oo 
	    (LAM x' tt id X) oo
	    (ABS [x'] (exp2bg (x' ++ X) e))
	end
      | M.App(e1, e2) =>
	(APP tt id X) oo
	 (((APPL tt id X) oo (exp2bg X e1)) pp
	  ((APPR tt id X) oo (EXP tt id X) oo (exp2bg X e2)))
      | M.Fix(f, x, e) =>
	let val x' = v2n x
	    val f' = v2n f in
	    (VAL tt id X) oo
	    (FIX [f',x'] tt id X) oo
	    (ABS [f',x'] (exp2bg (f' ++ x' ++ X) e))
	end
      | M.Case(e, matches) =>
	let fun f (((Ci,xi),ei), bg) =
		let val xi' = v2n xi 
		    val X' = xi' ++ X
		in  ((CASEE xi' tt id X) oo
		    (ABS [xi']
			 ((CONST Ci tt id X') oo
			 (EXP tt id X') oo
			 (exp2bg X' ei)))) pp bg
		end
	in
	    (CASE tt id X) oo
	    (List.foldr f ((CASEL tt id X) oo (exp2bg X e)) matches)
	end
      | M.PrimOp(ope, e1, e2) => Util.abort 6
      | M.Let(x, e1, e2) =>
	let val x' = v2n x in 
	(LET tt id X) oo
	 (((LETD tt id X) oo (exp2bg X e1)) pp
	  ((LETB x' tt id X) oo (ABS [x'] (exp2bg (x' ++ X) e2))))
	end
(*
      | M.Tuple [e1,e2] =>
	(PAIR tt id X) oo
	 (((PAIRL tt id X) oo (exp2bg X e1)) pp
	  ((PAIRR tt id X) oo (EXP tt id X) oo (exp2bg X e2)))
      | M.Tuple es => Util.abort 3
      | M.Proj(1, e) => (FST tt id X) oo (exp2bg X e)
      | M.Proj(2, e) => (SND tt id X) oo (exp2bg X e)
      | M.Proj(i, e) => Util.abort 4
*)

      (* I changed the encoding of tuples slightly - to allow
         for more than just pairs. HN

         The idea is that a tuple (e1,e2,...,en) is translated
         to

             TUPLE ( COMP1([[e1]]) | COMP2(EXP([[e2]])) | ... | COMPn(EXP([[en]])) )

         and a projection #i e is translated to

             PROJi ([[e]])

         We then need to generate rules 

             COMPi(val[0]) | COMPi+1(exp[1]) --> COMPi(val[0]) | COMPi+1([1])

         for i = 1,.., n-1 and rules

             TUPLE( COMP1(val[0]) | COMP2(val[1]) | ... | COMPn(val[n-1]) )
             --> val (TUPLE (...) )

         --- one for each size of tuple occurring in the program.

         Finally, we need projections (also one for each kind of
         projection occurring in the program).

             PROJi( TUPLE( COMPi(val[0]) | [1] ) )
             -->  [0]

      *)
      | M.Tuple [] => (VAL tt id X) oo (UNIT tt make_onames X)
      | M.Tuple (e::es) =>
	let fun f (e, (p,b)) =
                (p+1, b pp (COMP p tt id X) oo (EXP tt id X) oo (exp2bg X e))
	in  (TUPLE tt id X) oo
               (#2(List.foldr f (2,(COMP 1 tt id X) oo (exp2bg X e)) es))
	end
      | M.Proj(i, e) => (PROJ i tt id X) oo (exp2bg X e)        
      | M.Unit => (VAL tt id X) oo (UNIT tt make_onames X)
      | M.Ref e => (REF tt id X) oo (exp2bg X e)
      | M.DeRef e => (DEREF tt id X) oo (exp2bg X e)
      | M.Assign(e1, e2) =>
	(ASSIGN tt id X) oo
	 (((ALOC tt id X) oo (exp2bg X e1)) pp
	  ((AVAL tt id X) oo (EXP tt id X) oo (exp2bg X e2)))
      | M.Exchange(e1, e2) =>
	(EXC tt id X) oo
	 (((EXCL tt id X) oo (exp2bg X e1)) pp
	  ((EXCR tt id X) oo (EXP tt id X) oo (exp2bg X e2)))
      | M.Info(_,e) => exp2bg X e
      | M.Switch(e, switch, default) =>
        (*
         * [switch e of {C_i => e_i} | _ => e_(n+1)]_X =
         *    (switch \o id_X)
         *       ((switchl \o id_X)([e]_X) |
         *        (switche \o id_X)(C1 \o id_X)(exp \o id_X)([e1]_X) |
         *        ...
         *        (switche \o id_X)(Cn \o id_X)(exp \o id_X)([en]_X) |
         *
         * where 1<=i<=n and all C_i belong to some d_k with n_k constructors
         *
         *  FIXME: What about default?
         *)
	let fun f ((C,e), bg) = 
		   (SWITCHE tt id X) oo 
		   ((CONST C tt id X) oo (EXP tt id X) oo (exp2bg X e)) 
		pp bg
	in  (SWITCH tt id X) oo
	    (List.foldr f ((SWITCHL tt id X) oo (exp2bg X e))
	                (switch)
            ) (* FIXME: What about default? *)
	end
      | M.Deconst(C, e) => (* FIXME: What about C? *)
        (* [deconstruct e with C]_X =
         *    (deconst \o id_X)[e]_X
         *
         * rule deconstruct_1 =
         *    deconst(const([0] | val([1])))
         *      -> 
         *    val([1])
         *)
	(DECONST tt id X) oo (exp2bg X e)
