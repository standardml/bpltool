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

(** Type expressions used during type inference.
 * @version $LastChangedRevision$
 * Modified: $Date: 2006/06/04 20:13:28 $ by: $Author: hniss $
 *)

structure TypeExp :> TYPEEXP = struct

    exception Unify of string
    structure U = URef
    structure M  = MiniML

    fun min(x,y) = if x < y then x else y
    fun max(x,y) = if x > y then x else y

    (* Type expressions s *)
    type typename = string
    datatype typeexp' =
	     TyVar of typevar
	   | ArrowTy of typeexp * typeexp
	   | TupleTy of typeexp list
	   | TyCon of typename * typeexp list
	 and typevar =
	     V of {id : int, nm : string option, inst: typeexp option ref,
		   level : int ref, mark : bool ref}
    withtype typeexp = {term : typeexp'} U.uref

    fun make {term} = U.uRef{term=term}
    fun term (tau:typeexp) = #term(U.!! tau)

    fun var_lt (V v1) (V v2) = #id v1 < #id v2
    structure VarSet = OrderSet(type T = typevar val lt = var_lt)

    (* Marks *)
    fun mark_var   (V{mark,...}) = mark := true
    fun unmark_var (V{mark,...}) = mark := false
    fun marked_var (V{mark,...}) = !mark

    (* Primitives on type expressions *)
    fun subs term =
	case term of
	    TyVar _ => []
	  | ArrowTy(ty1,ty2) => [ty1,ty2]
	  | TupleTy tys => tys
	  | TyCon(_,tys) => tys

    fun free' p (tau, acc) =
	let val t = U.!! tau
	in  case #term t of
		TyVar v => 
		if marked_var v orelse not(p v) then acc
		else (mark_var v; v::acc)
	      | term => List.foldl (free' p) acc (subs term)
	end

    fun free tau = 
        let val vs = free' (fn _ => true) (tau, [])
	in  List.app unmark_var vs ; vs
	end


    (* Levels *)
    val current_level = ref 0
    fun pushLevel () = current_level := !current_level + 1
    fun popLevel  () = current_level := !current_level - 1

    fun var v = make {term = TyVar(V v)}
    local 
	val c = ref 0
    in  
        fun fresh' name = 
	    (c := !c + 1; var {id= !c, inst=ref NONE, mark=ref false, nm=name,
			       level=ref(!current_level)})
    end

    fun level (V v) = !(#level v)
    infix ::=
    fun (V v) ::= l = #level v := l

    fun lower (l:int) {term} =
	case term of
	    TyVar v => if level v > l then v ::= l
		       else ()
	  | term => List.app (lower l o U.!!) (subs term)

    fun lower_vars (v1,v2) =
	if level v1 < level v2 then v2 ::= level v1
	else v1 ::= level v2


    (* Pretty printing *)
    open Pretty
    fun ppVar (V{id,nm=SOME a,...}) = ppString ("'a" ^ Int.toString id) ++ ("[" ^+ ppString a +^ "]")
      | ppVar (V{id,nm=NONE,...}) = ppString ("'a" ^ Int.toString id)
    fun pp tau =
	let
	    fun paren my safe tree0 =
		if my<=safe then bracket "(#)" tree0
		else tree0
	    val pptyid = annotate ITALICS o ppString
	    fun pptexp safe tau =
		case term tau of
		    TyVar v => ppVar v
		  | ArrowTy(tau1,tau2) => 
		      let val t1 = pptexp 1 tau1
			  val t2 = pptexp 1 tau2
		      in  paren 1 safe (ppBinary(t1,"->",t2))
		      end
		  | TupleTy [] => pptyid "unit"
		  | TupleTy [tau] => pptexp safe tau
		  | TupleTy taus => 
		      let val ts = pptexps taus
		      in  clist " #* " (fn t=>t) ts
		      end
		  | TyCon(T,[]) => pptyid T
		  | TyCon(T,[tau]) => 
		      let val tree = pptexp 2 tau
		      in  tree ++ pptyid T
		      end
		  | TyCon(T,taus) => 
		      let val ts = pptexps taus
		      in  (ilist ",#" (fn t=>t) ts) ++ pptyid T
		      end
	    and pptexps taus = List.map (pptexp 0) taus
	in  pptexp 0 tau
	end
    val toString = Pretty.ppToString o pp

    fun ppTyScheme ([], tau) = pp tau
      | ppTyScheme (vars, tau) = "\\/" ^+ (clist " #," ppVar vars  +^ ".") ++ (pp tau)

    val tyschemeToString = Pretty.ppToString o ppTyScheme

    (* Constructors *)
    fun fresh _ = fresh' NONE
    fun freshVar id = fresh' (SOME id)

    fun intty    _ = make {term = TyCon("int",   [])}
    fun stringty _ = make {term = TyCon("string",[])}
    fun unitty   _ = make {term = TyCon("unit",  [])}
    fun boolty   _ = make {term = TyCon("bool",  [])}

    fun refty           ty = make {term = TyCon("ref",[ty])}
    fun tuplety        tys = make {term = TupleTy tys}
    fun arrowty (ty1, ty2) = make {term = ArrowTy(ty1, ty2)}
    fun tyconty  (Tn, tys) = make {term = TyCon(Tn, tys)}

    fun constructor term =
	case term of
	    ArrowTy _ => (fn [tau1,tau2] => arrowty(tau1,tau2)
			   | _ => Util.abort 43765)
	  | TupleTy _ => (fn taus => tuplety taus)
	  | TyCon(Tn,_) => (fn taus => tyconty (Tn,taus))
	  | TyVar _ => Util.abort 43766

    (* Type schemes *)
    type typescheme = typevar list * typeexp

    fun instance ((vars, tau):typescheme) =
	( app (fn V v => #inst v := SOME(fresh())) vars
        ; let val tau' = copy tau
	  in  app (fn V v => #inst v := NONE) vars
	    ; tau'
	  end
        )
    and copy tau =
	let val t = U.find tau
	in  case term t of
		TyVar(V{inst=ref(SOME tau'),...}) => tau'
	      | TyVar(V{inst=ref(NONE),...}) => t
	      | term =>
		  let val ts = List.foldl (fn (t,ts) => copy t :: ts)
					  [] (subs term)
		  in  (constructor term) (rev ts)
		  end
	end

    fun promote tau = ([], tau)

    fun generalize tau =
	let val vars = free' (fn v => level v > !current_level) (tau,[])
	in  app (fn v => v ::= ~1) vars
	  ; (vars, tau)
	end

    (* Accessors *)
    datatype view =
	     Var
	   | Tuple of int
	   | Const of typename
	   | Arrow
    fun view ty =
	case term ty of
	    TyVar _ => Var
	  | TupleTy tys => Tuple(List.length tys)
	  | ArrowTy _ => Arrow
	  | TyCon(T,_) => Const T

    (* Conversion *)
    structure Map = OrderFinMap(type T = string fun lt x y = String.<(x,y))
    type map = typeexp Map.map
    val mkMap = Map.fromList
    fun fromAST map astty =
	case astty of
	    M.TyVar alpha => ( case Map.lookup map alpha of
				   NONE => raise Fail("Unbound tyvar " ^ alpha)
				 | SOME ty => ty
                             )
	  | M.TyCon([],"int") => intty ()
	  | M.TyCon([],"string") => stringty ()
	  | M.TyCon([],"unit") => unitty ()
	  | M.TyArrow(ty1,ty2) => arrowty(fromAST map ty1, fromAST map ty2)
	  | M.TyTuple tys => tuplety(List.map (fromAST map) tys)
	  | M.TyCon(tys, tyname) => tyconty(tyname, List.map (fromAST map) tys)

    (* Unification *)
    fun unify taus =
	let 
	    fun err reason = raise Unify reason
	    fun occurs' (v as (V{id,...})) t =
		case #term t of
		    TyVar(V{id=id',...}) => id=id'
		  | ArrowTy(t1,t2) => occurs v t1 orelse occurs v t2
		  | TyCon(_, ts) => List.exists (occurs v) ts
		  | _ => false
	    and occurs v t = occurs' v (U.!! t)

	    fun combine (i, i') =
		case (#term i, #term i') of
		    (TyVar v, TyVar v') => (lower_vars(v,v');i')
		  | (TyVar v, _)  => if occurs' v i' then err("circularity")
				     else (lower (level v) i'; i')
		  | (_, TyVar v') => if occurs' v' i then err("circularity")
				     else (lower (level v') i; i)
		  | _ => err("constructor clash")

	    fun unify (t,t') =
		let val (t, t') = (U.find t, U.find t')
		in  if U.equal(t, t') then ()
		    else case (term t, term t') of
			     (ArrowTy(t1,t2), ArrowTy(t1',t2')) =>
			       ( U.unify #1 (t, t')
                               ; unify (t1, t1')
                               ; unify (t2, t2')
                               )
			   | (TupleTy ts, TupleTy ts') =>
                               ( U.unify #1 (t, t')
                               ; if length ts = length ts' then ()
                                 else err("tuple size mismatch")
                               ; app unify (ListPair.zip(ts,ts'))
                               )
			   | (TyCon(Tn,ts), TyCon(Tn', ts')) =>
                               ( U.unify #1 (t, t')
			       ; if Tn = Tn' then ()
				 else err("type constructor mismatch")
			       ; if length ts = length ts' then ()
				 else err("arity mismatch")
			       ; app unify (ListPair.zip(ts,ts'))
                               )
			   | _ => U.unify combine (t,t')
		end
	in  unify taus
	end

end (* structure TypeExp *)
