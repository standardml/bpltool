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

(** Abstract syntax for MiniML terms.
 * @version $LastChangedRevision$
 * Modified: $Date: 2006/09/04 20:54:23 $ by: $Author: hniss $
 *)

structure MiniML: MINIML =
struct

    structure P = Pattern

    datatype operator = 
	Arith of string
      | Rel   of string

    fun isRelational(Rel _) = true
      | isRelational _ = false
    fun isArithmetical(Arith _) = true
      | isArithmetical _ = false

    datatype ('info, 'pat) exp = 
	Var     of string
      | Integer of int
      | String  of string
      | Unit    
      | Const   of string * ('info,'pat) exp
      | Abs     of string * ('info,'pat) exp
      | App     of ('info,'pat) exp * ('info,'pat) exp
      | Fix     of string * string * ('info,'pat) exp
      | Case    of ('info,'pat) exp * ('pat * ('info,'pat) exp) list
      | PrimOp  of operator * ('info,'pat) exp * ('info,'pat) exp
      | Let     of string * ('info,'pat) exp * ('info,'pat) exp
      | Tuple   of ('info,'pat) exp list
      | Proj    of int * ('info,'pat) exp
      | Ref     of ('info,'pat) exp
      | DeRef   of ('info,'pat) exp
      | Assign  of ('info,'pat) exp * ('info,'pat) exp
      | Exchange of ('info,'pat) exp * ('info,'pat) exp
      (* without surface syntax *)
      | Info    of 'info * ('info,'pat) exp
      | Switch  of ('info, 'pat) exp * 
		   (string (* constructor *) * ('info, 'pat) exp) list *
                   ('info, 'pat) exp (* default *)
      | Deconst of string (* expected constructor *) * ('info, 'pat) exp

    datatype tyexp =
        TyVar   of string
      | TyCon   of tyexp list * string
      | TyTuple of tyexp list
      | TyArrow of tyexp * tyexp

    datatype conbind = 
        Con     of string * tyexp

    datatype ('info,'pat) bind =
        ValBind of string * ('info, 'pat) exp
      | DatBind of string * string list * conbind list
      | TyBind  of string * string list * tyexp

    datatype ('info,'pat) prog = 
        Export of string list * ('info,'pat) bind list

    type pat = string (* constructor *) * string (* variable *)
    type 'info prog' = ('info, pat) prog


    fun mkLet bindings body =
	let fun f (ValBind(x,e),b) =  Let(x,e,b)
	      | f _ = Util.abort 343532
	in  List.foldr f body bindings
	end

    fun opeToString(Arith ope) = ope
      | opeToString(Rel   ope) = ope

    (* pretty print code by Henning Makholm - taken from the regfun impl *)
    open Pretty
    infixr 5 ^+
    infix 4 +^
    infixr 4 ++
    val (kw_style, id_style, regannot_style) =
	(BOLD,     ITALICS,  COLOR("red"))
    val ppKw = annotate kw_style o ppString
    val ppId = annotate id_style o ppString
    val ppCtor = ppString
    val ppCmt = annotate SMALL o close(0," *)") o prefix "(* "

    fun build_let([],body) = body
      | build_let(b0::bs,body)
	= break(1,0)( ppKw "let" ++ makelist(true,"") (b0 :: map forcemulti bs)
		    , compose(ppKw "in" ++ body,1,2,0,ppKw "end")
		    )
	
    fun paren my safe t = if my < safe then "(" ^+ t +^ ")" else t

    fun ppValbind ppBindInfo ppExp (x,e) = 
	let val (i,e) = case e of
			    Info(i,e) => (ppBindInfo i,e)
			  | e => (ppNone(), e)
	in  (ppKw "val" ++ ppId x ++ i ++ ppString "=") ++ ppExp e
	end

    fun ppExp ppBindInfo ppPat exp =
	let fun ppelist front back es
	      = compose(front ^+ clist "#, " (ppe 1) es,0,2,0,back)
	    and pperoundlist es = ppelist "(" (ppString ")") es
	    and ppe safe exp =
		case exp
 		 of Var x => ppId x
		  | Unit => ppString "()"
		  | String s => "\"" ^+ ppString s +^ "\""
		  | Integer i => ppInt i
		  | Const(C,Unit) => ppCtor C
		  | Const(C,e) => paren 3 safe (ppCtor C ++ ppe 6(*4*) e)
		  | PrimOp(ope,e1,e2) =>
		    paren 2 safe (break(1,0)
				       (ppe 3 e1, (ppString (opeToString ope)) ++ ppe 3 e2))
		  | Proj(i, e) => paren 3 safe ("#" ^+ ppInt i ++ ppe 4 e)
		  | Tuple es => pperoundlist es
		  | Case(e,match)
                    => let fun ppMRule(p,e) = ppBinary(ppPat p, "=>", ppe 1 e)
		       in  paren 1 safe
			        ( break(1,0)( ppKw "case" ++ ppe 1 e
		      	 		    , ppKw "of" ++ (clist "# | " ppMRule match)
			 		    )
				)
		       end
		  | Switch(e, switch, default)
		    => let fun Some(C,e) = (SOME C,e)
			   fun ppBranch(SOME C,e) = 
			       ppBinary(ppString C,"=>", ppe 1 e)
			     | ppBranch(NONE,e) =
			       ppBinary(ppString "_","=>", ppe 1 e)
		       in  paren 1 safe
			        ( break(1,0)( ppKw "switch" ++ ppe 1 e
					    , ppKw "of" ++ (clist "# | " ppBranch (map Some switch @ [(NONE,default)]))
                                            )
                                )
		       end
		  | Deconst(C,e) => paren 3 safe ("#" ^+ ppString C ++ ppe 4 e)
		  | Let(x,e1,e2) => build_let([ppvalbind(x,e1)], ppe 1 e2)
		  | App(e,e') => paren 3 safe (ppe 3 e ++ ppe 4 e')
		  | Abs(x, e) => paren 2 safe ((("fn " ^+ ppId x) +^ " =>") ++ ppe 1 e)
		  | Fix(f,x,e) => 
		    paren 2 safe (((("fix " ^+ ppId f) ++ bracket "(#)" (ppId x)) +^ " =>")++ ppe 1 e)
		  | Ref e => paren 2 safe ("ref " ^+ ppe 1 e)
		  | DeRef e => paren 2 safe ("!" ^+ ppe 1 e)
		  | Assign(e,e') => paren 2 safe (ppe 1 e ++ ppKw ":=" ++ ppe 1 e')
		  | Exchange(e,e') => paren 3 safe (ppe 3 e ++ ppe 4 e')
		  | Info(i,e) => ppe safe e
	    and ppvalbind (x,e) = ppValbind ppBindInfo (ppe 1) (x,e)
	in  ppe 1 exp
	end

    fun pptyid tv = "'" ^+ ppString tv
    fun ppt safe texp =
	case texp of
	    TyVar tv => pptyid tv
	  | TyCon([],tc) => ppString tc
	  | TyCon([te],tc) => ppt 3 te ++ ppString tc
	  | TyCon(tes,tc) => 
	      ("(" ^+ clist "#, " (ppt 1) tes +^ ")") ++ ppString tc
	  | TyTuple tes =>
	    paren 2 safe (clist "# * " (ppt 2) tes)
	  | TyArrow(te1,te2) =>
	    paren 1 safe (ppt 2 te1 ++ ppString "->" ++ ppt 2 te2)
    val ppt = fn t => ppt 1 t
    fun pptyids [] tycon = tycon
      | pptyids [tyid] tycon = pptyid tyid ++ tycon
      | pptyids tyids tycon = ("(" ^+ ilist "#, " pptyid tyids +^ ")") ++ tycon
    fun ppb ppBindInfo ppPat bind =
	case bind of
	    ValBind(x,e) => ppValbind ppBindInfo (ppExp ppBindInfo ppPat) (x,e)
	  | DatBind(tname,tids,cons) => 
	    break(1,0)( "datatype " ^+ pptyids tids (ppString tname)
		      , "= " ^+ ilist "# | " ppc cons
		      )
	  | TyBind(tname,tids, te) =>
	    break(1,0)( "type " ^+ pptyids tids (ppString tname)
		      , "= " ^+ ppt te
		      )

    and ppc (Con(C, TyCon([],"unit"))) = ppCtor C
      | ppc (Con(C, t)) = ppCtor C ++ ("of " ^+ ppt t)

    fun pp ppBindInfo ppPat (Export(exps, binds)) =
	let val ppb = ppb ppBindInfo ppPat
	    fun ppBS [] = Util.abort 12345
	      | ppBS [bind] = ppb bind
	      | ppBS (b::bs) =
		let val (b,bs) = (ppb b, map ppb bs)
		in  makelist(true,"") (b :: map forcemulti bs)
		end
	    val binds = ppBS binds
	in  case exps of
		[] => binds
	      | _ => 
                compose( break(1,2)
                            ( compose( ppKw "export" ++ clist "#, " ppString exps
                                     ,1,2,0, ppKw "from")
                            , binds
                            )
                       , 1,2,0,ppKw "end"
                       )
	end
    fun ppPat (C,x) = ppCtor C ++ ppId x

    val pp' = fn ppPat => fn prog => pp Pretty.ppNone ppPat prog
    val ppExp' = fn ppPat => fn exp => ppExp Pretty.ppNone ppPat exp

    val dump_fresh =
	Flags.makeBoolFlag{name="/dump/fresh",short="",long="dump-alpha",
			   arg="",desc="Dump alpha-renamed MiniML program",
			   default=false}

    local 
	val counter = ref 0
    in
        fun fresh _ =
	      "_x" ^ Int.toString(!counter)
	    before
	      counter := !counter + 1
    end

    structure Map = Util.StringMap
    type map = string Map.map
    type 'pat freshpat = (map->string->(string*map)) -> map -> 'pat -> 'pat * map
    fun addFresh map x =
	let val x' = fresh x
	in  (x', Map.add(x, x', map))
	end
    fun freshExp freshPat map exp =
	case exp of
	    Var x =>
	      ( case Map.lookup map x of
		    SOME x' => Var x'
		  | NONE => raise Fail("Fresh: Unbound variable " ^ x)
              )
	  | Integer int => Integer int
	  | String str => String str
	  | Unit => Unit
	  | Const(C, e) => Const(C, freshExp freshPat map e)
	  | App(e1, e2) => 
	      App(freshExp freshPat map e1, freshExp freshPat map e2)
	  | Case(e, match) => 
	      let fun f (p, e) = 
		      let val (p',map') = freshPat addFresh map p
		      in  (p', freshExp freshPat map' e)
		      end
	      in  Case(freshExp freshPat map e, List.map f match)
	      end
	  | Switch(e, switch, default) =>
	      let fun f (C,e) = (C, freshExp freshPat map e)
	      in  Switch(freshExp freshPat map e,
			 List.map f switch,
			 freshExp freshPat map default)
	      end
	  | Deconst(C, e) => Deconst(C, freshExp freshPat map e)
	  | PrimOp(ope, e1, e2) => 
	      PrimOp(ope,freshExp freshPat map e1, freshExp freshPat map e2)
	  | Tuple es =>
	      Tuple(List.map (freshExp freshPat map) es)
	  | Proj(p, e) => Proj(p, freshExp freshPat map e)
	  | Ref e => Ref(freshExp freshPat map e)
	  | DeRef e => DeRef(freshExp freshPat map e)
	  | Assign(e1, e2) => 
	      Assign(freshExp freshPat map e1, freshExp freshPat map e2)
          | Exchange(e1, e2) =>
	      Exchange(freshExp freshPat map e1, freshExp freshPat map e2)
	  | Abs(x, e) =>
	      let val x' = fresh x
		  val map' = Map.add(x, x', map)
	      in  Abs(x',freshExp freshPat map' e)
	      end
	  | Fix(f, x, e) => 
	      let val (f',x') = (fresh f, fresh x)
		  val map' = Map.add(x, x', Map.add(f, f', map))
	      in  Fix(f', x', freshExp freshPat map' e)
	      end
	  | Let(x, e1, e2) => 
	      let val x' = fresh x
		  val map' = Map.add(x, x', map)
	      in  Let(x', freshExp freshPat map e1, freshExp freshPat map' e2)
	      end
	  | Info(i, e) => Info(i, freshExp freshPat map e)

    fun freshProg freshPat (Export(exports, binds)) =
	let fun f (ValBind(x,e),(bs,map)) =
		let val x' = fresh x
		    val e' = freshExp freshPat map e
		in  (ValBind(x',e')::bs, Map.add(x,x',map))
		end
	      | f (b,(bs,map)) = (b::bs,map)
	    val (binds',map) = List.foldl f ([],Map.empty) binds
	    fun g x = case Map.lookup map x of
			  NONE => Util.abort 67890
			| SOME y => y
	    val exports' = List.map g exports
	in  Export(exports', rev binds')
	end
    val fresh = freshProg
    fun freshPat add map (C,x) = 
	let val (x',map') = add map x
	in  ((C,x'), map')
	end

    fun subExp exp =
	case exp of
	    Var _ => []
	  | Integer _ => []
	  | String _ => []
	  | Unit => []
	  | Const(_, e) => [e]
	  | Abs(_,e) => [e]
	  | Fix(_,_,e) => [e]
	  | Proj(_, e) => [e]
	  | Ref e => [e]
	  | DeRef e => [e]
	  | Info(_,e) => [e]
	  | Deconst(_,e) => [e]
	  | Assign(e1,e2) => [e1,e2]
          | Exchange(e1,e2) => [e1,e2]
	  | App(e1,e2) => [e1,e2]
	  | PrimOp(_,e1,e2) => [e1,e2]
	  | Let(_,e1,e2) => [e1,e2]
	  | Case(e,ps) => e :: (List.map (fn(p,e)=>e) ps)
	  | Switch(e,ps,d) => e :: (List.map (fn(p,e)=>e) ps) @ [d]
	  | Tuple es => es

    fun max (i1,i2) = if i1 > i2 then i1 else i2
    fun app1 f (x,y) = (f x, y)
    fun sizeExp exp =
	case exp of
	    Info(_,e) => sizeExp e
	  | _ => 1 + List.foldl (fn (e,s) => sizeExp e + s) 0 (subExp exp)

    fun size (Export(_, bs)) =
	let fun sizeBind (ValBind(_,e)) = 1+sizeExp e
	      | sizeBind (DatBind(t,targs,cbs)) = 1
	      | sizeBind (TyBind(t,targs,abb)) = 1
	in  List.foldl (fn (b,s) => sizeBind b + s) 0 bs
	end


    fun simplifyExp exp =
	case exp of
            (* fn x => case x of y => e  -->  fn y => e *)
	    Abs(x, Case(Var x', [(P.PVar y, e)])) => 
	      if x = x' then simplifyExp (Abs(y, e) )
	      else Abs(x,Case(Var x',[(P.PVar y, simplifyExp e)]))
	  | Fix(f, x, Case(Var x', [(P.PVar y, e)])) => 
	      if x = x' then simplifyExp (Fix(f, y, e))
	      else Fix(f, x, Case(Var x',[(P.PVar y, simplifyExp e)]))
	  | Const(c, e) => Const(c, simplifyExp e)
	  | Abs(x,e) => Abs(x, simplifyExp e)
	  | Fix(f, x, e) => Fix(f, x, simplifyExp e)
	  | Proj(i, e) => Proj(i, simplifyExp e)
	  | Ref e => Ref(simplifyExp e)
	  | DeRef e => DeRef(simplifyExp e)
	  | Info(i, e) => Info(i, simplifyExp e)
	  | Deconst(c, e) => Deconst(c, simplifyExp e)
	  | Assign(e1,e2) => Assign(simplifyExp e1, simplifyExp e2)
	  | Exchange(e1,e2) => Exchange(simplifyExp e1, simplifyExp e2)
	  | App(e1,e2) => App(simplifyExp e1, simplifyExp e2)
	  | PrimOp(ope, e1,e2) => PrimOp(ope,simplifyExp e1, simplifyExp e2)
	  | Let(x, e1,e2) => Let(x,simplifyExp e1, simplifyExp e2)
	  | Case(e, ps) => Case(simplifyExp e, List.map (fn(p,e)=>(p,simplifyExp e)) ps)
	  | Switch(e,ps,d) => Switch(simplifyExp e, List.map (fn(p,e)=>(p,simplifyExp e)) ps, simplifyExp d)
	  | Tuple es => Tuple(List.map simplifyExp es)
	  | e => e
end

