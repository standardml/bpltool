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
 * @version $Revision: 1.2 $
 * Modified: $Date: 2006/06/04 20:13:28 $ by: $Author: hniss $
 *)

structure TypeExp :> TYPEEXP = struct

    exception Unify of string
    structure U = URef
    structure M  = MiniML

    fun min(x,y) = if x < y then x else y
    fun max(x,y) = if x > y then x else y

    (* Marks used during traversals *)
    fun mark() = ref()
    val no_mark = mark()

    (* Constructors *)
    type typename = string
    type typevar = {id : int, nm : string option}
    datatype typeexp' =
	     TyVar of typevar
	   | ArrowTy of typeexp * typeexp
	   | TupleTy of typeexp list
	   | TyCon of typename * typeexp list
    withtype typeexp = {term : typeexp', rank : int ref, mark : unit ref ref} U.uref

    (* Levels *)
    fun incr (r as ref n) = r := n+1
    fun decr (r as ref n) = r := n-1
    infix ::==
    infix !!
    fun arr ::== (i, v) = Array.update(arr, i, v)
    fun arr !! i = Array.sub(arr, i)

    val level = ref 0
    val level_terms : typeexp list array ref = ref(Array.array(2, []))
    fun resetLevels () = ( level := 1
			 ; !level_terms ::== (0,[])
			 ; !level_terms ::== (1,[])
			 )
    fun popLevel () = ( !level_terms ::== (!level, [])
                      ; decr level
                      )
    fun pushLevel () =
	( if Array.length(!level_terms) < !level + 2
	  then let val new = Array.array(!level + 2, [])
		   val _ = Array.copy{src = !level_terms, dst = new, di = 0}
	       in  level_terms := new
	       end
	  else ()
	; incr level
        )

    fun make {term} = 
	let val ty = U.uRef{term=term, rank = ref(!level), mark = ref no_mark}
	    val _ = !level_terms ::== (!level, ty :: (!level_terms !! !level))
	in  ty
	end
    fun unmake tau = #term((U.!! tau) : {term : typeexp', rank : int ref, mark : unit ref ref})
    fun sub_types tau =
	case tau of
	    TyVar _ => []
	  | ArrowTy(ty1,ty2) => [ty1,ty2]
	  | TupleTy tys => tys
	  | TyCon(_,tys) => tys
			    
    open Pretty
    fun ppVar {id,nm=SOME a} = ppString a
      | ppVar {id,nm=NONE} = ppString ("'a" ^ Int.toString id)
    fun pp tau =
	let
	    fun paren my safe tree0 =
		if my<=safe then bracket "(#)" tree0
		else tree0
	    val pptyid = annotate ITALICS o ppString
	    fun pptexp safe tau =
		case unmake tau of
		    TyVar v => ppVar v
		  | ArrowTy(tau1,tau2) => paren 1 safe
						(ppBinary( pptexp 1 tau1
							 , "->"
							 , pptexp 1 tau2 ))
		  | TupleTy [] => pptyid "unit"
		  | TupleTy [tau] => pptexp safe tau
		  | TupleTy taus => clist " #* "(pptexp 0) taus
		  | TyCon(T,[]) => pptyid T
		  | TyCon(T,[tau]) => pptexp 2 tau ++ pptyid T
		  | TyCon(T,taus) => (ilist ",#" (pptexp 0) taus) ++ pptyid T

	in  pptexp 0 tau
	end
    val toString = Pretty.ppToString o pp

    (* Constructors *)
    fun var v = make {term = TyVar v}
    local 
	val c = ref 0
    in  
        fun fresh' name = (c := !c + 1; var {id = !c, nm = name})
    end
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

    (* Accessors *)
    datatype view =
	     Var
	   | Tuple of int
	   | Const of typename
	   | Arrow
    fun view ty =
	case unmake ty of
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
	    fun occurs' (v as {id,nm}) t =
		case #term t of
		    TyVar{id=id',...} => id=id'
		  | ArrowTy(t1,t2) => occurs v t1 orelse occurs v t2
		  | TyCon(_, ts) => List.exists (occurs v) ts
		  | _ => false
	    and occurs v t = occurs' v (U.!! t)

	    fun combine (i, i') =
		let val (r, r') = (!(#rank i), !(#rank i'))
		    val m = min(r,r')
		in  case (#term i, #term i') of
			(TyVar v, TyVar v') => (# rank i := m; i' )
		      | (TyVar v, _) => if occurs' v i' then err("circularity")
					else ( #rank i := m ; i' )
		      | (_, TyVar v') => if occurs' v' i then err("circularity")
					 else ( #rank i' := m ; i )
		      | (c,c') => err("constructor clash")
		end

	    fun unify (t,t') =
		let val (t, t') = (U.find t, U.find t')
		in  if U.equal(t, t') then ()
		    else case (unmake t, unmake t') of
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

    exception Cycle
    fun occur_check visited =
	let val visiting = mark()
	    fun occur_check ty =
		let val {term,mark,rank} = U.!! ty
		in  if !mark = visiting then raise Cycle
		    else if not(!mark = visited) andalso !rank = !level
		    then let val tys = sub_types term
			 in  mark := visiting
			   ; List.app occur_check tys
			   ; mark := visited
			 end
		    else ()
		end
	in  occur_check
	end

    fun propagate_and_realize n fresh visited =
	let val generic_index = ref 0
	    val visiting = mark()
	    fun propagate_realize k ty =
		let val {rank,mark,term} = U.!! ty
		in  ( if !mark = visiting then raise Cycle
		      else if not(!mark = visited)
		      then ( ( case term of
				   TyVar _ => rank := min(!rank, k)
				 | _ => let val tys = sub_types term
					    val p_r = propagate_realize(min(!rank,k))
					in  mark := visiting
					  ; rank := List.foldl (fn (t,k) => max(k,p_r t)) 1 tys
					end
			     )
			   ; mark := visited
			   ; if !rank = n then rank := (decr generic_index; !generic_index)
			     else ()
                           )
		      else ()
                    )
                  ; if !rank < 0 then n else !rank
		end
	in  propagate_realize
	end

    fun generalize ty =
	let val visited = mark()
	    val fresh = mark()
	    val generalize = propagate_and_realize (!level) fresh visited
	    val occur_check = occur_check visited
	    val sorted = Array.array(!level+1, [])

	    fun sort ty =
		let val {term,mark,rank=ref rank} = U.!! ty
		in  mark := fresh
		  ; sorted ::== (rank, ty :: (sorted!!rank))
		end
	    val _ = List.app sort (!level_terms !! !level)

	    fun loop i = if i <= !level-1 
			 then (List.map (generalize i) (sorted !! i); loop(i+1); ())
			 else ()
	    val _ = loop 0

	    val _ = generalize (!level) ty

	    fun update ty =
		let val {term,mark,rank=ref rank} = U.!! ty
		in  if rank = !level then occur_check ty
		    else if rank < 0 then ()
		    else !level_terms  ::== (rank, ty :: (!level_terms !! rank))
		end
	    fun loop i = if i <= !level
			 then (List.app update (sorted !! i); loop(i+1))
			 else ()
	    val _ = loop 0
	in  ()
	end

    fun instance ty =
	let val {term,mark,rank=ref rank} = U.!! ty 
	in  if rank >= 0 then ty
	    else 
	      let val table = Array.array(~rank, NONE)
		  fun instance ty =
		      let val {term,mark,rank=ref rank} = U.!! ty 
		      in  if rank>= 0 then ty else
			  case Array.sub(table, ~rank - 1) of
			      NONE =>
			      let val ty' = fresh ()
				  val _ = Array.update(table,~rank - 1,SOME ty')
				  val new = 
				      case term of
					  TyVar v => TyVar v
					| ArrowTy(ty1,ty2) => ArrowTy(instance ty1, instance ty2)
					| TupleTy tys => TupleTy(List.map instance tys)
					| TyCon(Tn, tys) => TyCon(Tn, List.map instance tys)
				  val _ = U.::=(ty', {term=new,mark=ref no_mark,rank= ref(!level)})
			      in  ty'
			      end				  
			    | SOME ty' => ty'
		      end
	      in  instance ty
	      end
	end

end (* structure TypeExp *)
