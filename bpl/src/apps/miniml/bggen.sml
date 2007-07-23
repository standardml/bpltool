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

(** Translation from MiniML to bigraphs.
 * @version $LastChangedRevision$
 * Modified: $Date: 2006/09/04 09:31:14 $ by: $Author: hniss $
 *)

functor BGGen(structure BG: BG_ADT
  	         sharing type BG.Control.control = BG.Ion.control
  	         sharing type BG.Ion.ion = BG.BgVal.ion
  	         sharing type BG.Wiring.wiring = BG.BgVal.wiring
		 sharing type BG.Name.name =
			      BG.NameSet.elt =
			      BG.Link.name =
			      BG.Ion.name
		 sharing type BG.NameSet.Set =
			      BG.Link.nameset =
			      BG.Ion.nameset =
			      BG.Interface.nameset =
			      BG.Wiring.nameset =
			      BG.BgVal.nameset
		 sharing type BG.Link.link = BG.LinkSet.elt
		 sharing type BG.LinkSet.Set = BG.Wiring.linkset
		 sharing type BG.Permutation.permutation =
			      BG.BgVal.permutation
		 sharing type BG.Interface.interface =
			      BG.BgVal.interface
		 sharing type BG.BgVal.bgval = BG.BgBDNF.bgval
              ) : BGGEN = struct

(*
 Mapping from Miniml abstract syntax tree (AST) to BG AST using
 abstract syntax. The mapping produces bgvals.

 Henning Niss and Ebbe Elsborg, April 23 2007
 Corresponds to binding-extended-mini-ml.bpl of April 22 2007
*)

structure BgVal   = BG.BgVal
structure NameSet = BG.NameSet
structure Ion     = BG.Ion
structure Name    = BG.Name
structure Control = BG.Control
structure Wiring  = BG.Wiring
structure Link    = BG.Link
structure LinkSet = BG.LinkSet
structure NameMap = OrderFinMap(Name.Order)

structure M   = MiniML
structure B   = BgVal

(* interface types *)
type ppstream = PrettyPrint.ppstream

(* aux. stuff *)
val info = BG.Info.noinfo
type bgval = B.bgval
type name = BG.Name.name
type nameset = NameSet.Set

fun makeion (c:string) (f:name list) (b:nameset list) =
    B.Ion info (
      Ion.make {
        ctrl =
          Control.make (c, Control.Active, length b, length f),
        free = f,
        bound = b})

fun mkctrl (c:string) = makeion c [] []

fun mkatom (c:string) = B.Com info (makeion c [] [], B.Mer info 0)

fun mklink iname oname = 
    Link.make {outer = SOME oname, inner = iname}

fun make_olink (x:name) = mklink NameSet.empty x

fun make_olinkset (X:nameset) =
    NameSet.fold (LinkSet.insert o make_olink) LinkSet.empty X

fun make_onames (X:nameset) = B.Wir info (Wiring.make (make_olinkset X))

fun make_closure (X:nameset) =
    B.Wir info (Wiring.make(LinkSet.singleton(
	Link.make{outer=NONE,inner=X})))

val barren = B.Mer info 0
val id = B.Per info o BG.Permutation.id_n
val id1 = id 1
fun v2n x = BG.Name.make (String.toString x)

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
fun (x:name) ++ (X:nameset) = if NameSet.member x X
			      then raise NameSet.DuplicatesRemoved
			      else NameSet.insert x X

(* Generate bgterms from abstract Miniml syntax - see miniml-sig.sml. *)
fun id (X:nameset) = B.Wir info (Wiring.id_X X)
fun ABS (x:name list) (b:bgval) = B.Abs info (NameSet.fromList x, b)
fun VAR (x:name) = B.Com info (makeion "CVAR" [x] [], B.Mer info 0)
fun LAM (x:name) = makeion "CLAM" [] [NameSet.singleton x]
fun FIX (X:name list) = makeion "CFIX" [] [NameSet.fromList X]
fun LETB (x:name) = makeion "CLETB" [] [NameSet.singleton x]
fun INT (n:int) = mkatom ("C" ^ Int.toString(n))
fun CONST (C:string) = mkctrl C
fun CASEE (x:name) = makeion "CASEE" [] [NameSet.singleton x]
val VAL = mkctrl "CVAL"
val APP = mkctrl "CAPP"
val APPL = mkctrl "CAPPL"
val APPR = mkctrl "CAPPR"
val EXP = mkctrl "CEXP"
val LET = mkctrl "CLET"
val LETD = mkctrl "CLETD"
(*
val PAIR = mkctrl "CPAIR"
val PAIRL = mkctrl "CPAIRL"
val PAIRR = mkctrl "CPAIRR"
val FST = mkctrl "CFST"
val SND = mkctrl "CSND"
*)
val TUPLE = mkctrl "CTUPLE"
fun COMP (i:int) = mkctrl ("COMP"^Int.toString i)
fun PROJ (i:int) = mkctrl ("PROJ"^Int.toString i)
val UNIT = mkatom "CUNIT"
val REF = mkctrl "CREF"
val DEREF = mkctrl "CDEREF"
val ASSIGN = mkctrl "CASSIGN"
val ALOC = mkctrl "CALOC"
val AVAL = mkctrl "CVAL"
val CASE = mkctrl "CASE"
val CASEL = mkctrl "CASEL"
val EXC = mkctrl "EXC"
val EXCL = mkctrl "EXCR"
val EXCR = mkctrl "EXCL"
val SWITCH = mkctrl "SWITCH"
val SWITCHL = mkctrl "SWITCHL"
val SWITCHE = mkctrl "SWITCHE"
val DECONST = mkctrl "DECONST"

fun DEF' (x:name) = makeion "DEF'" [x] []

(* This translation should not be altered, just change the generating
functions above to match change in BG signatures. *)

fun exp2bg (X:nameset) exp =
    case exp of
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

fun getValBind (M.ValBind(x,e)) = SOME(x,e)
  | getValBind _                = NONE

fun isValBind (M.ValBind _) = true
  | isValBind _             = false

val empty = NameSet.empty

(* interface *)
type bg = BgVal.bgval

fun toBG (M.Export(exports,binds)) =
    let val binds = List.mapPartial getValBind binds
	val exports = NameSet.fromList(List.map v2n exports)
	fun gen ((x,e), (X,bg)) = (v2n x ++ X, 
				   ((DEF' (v2n x) tt id X) oo (exp2bg X e)) pp bg)
	val (X, bg) = List.foldl gen (empty, barren) binds
    in  (id1 tt make_closure (NameSet.difference X exports) tt id exports) 
        oo bg
    end

(*
fun toBG single (M.Export(exports,binds)) =
    (* for now just test by translating all expressions in
       binds to BG terms *)
    let val binds = List.filter isValBind binds
	val binds = 
	    case binds of
		[M.ValBind(x,e)] => [(x, e)]
	      | _ => if single then [("it", M.mkLet binds M.Unit)]
		     else List.mapPartial getExp binds
    in  List.map (fn (x,e) => (x, Some(exp2bg empty e) handle E => None E)) 
		 binds
    end

val toBG = fn binds =>
    case toBG true binds of
	[(_,res)] => res
      | _ => raise Fail("Shouldn't happen: toBG returned multiple results")
*)

nonfix pp

fun pp indent pps bg =
    ( BgVal.pp indent pps bg
    ; PrettyPrint.add_string pps ": "
    ; BG.Interface.pp indent pps (BgVal.innerface bg)
    ; PrettyPrint.add_string pps " -> "
    ; BG.Interface.pp indent pps (BgVal.outerface bg)
    )

end
