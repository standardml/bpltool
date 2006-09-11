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
 * @version $Revision: 1.18 $
 * Modified: $Date: 2006/09/04 20:54:23 $ by: $Author: hniss $
 *)

structure MiniML: MINIML =
struct

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
        Prog of ('info,'pat) bind list

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

    fun ppExp ppPat exp =
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
		  | Info(i,e) => ppe safe e
	    and ppvalbind (x,e)= ((ppKw "val" ++ ppId x) +^ " =") ++ ppe 1 e
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
    fun ppb ppPat bind =
	case bind of
	    ValBind(x,e) => 
	    ((ppKw "val" ++ ppId x) +^ " =") ++ ppExp ppPat e
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

    fun pp ppPat prog =
	case prog of
	    Prog [] => Util.abort 12354
	  | Prog[bind] => ppb ppPat bind
	  | Prog(b::bs) =>
	       let val (b,bs) = (ppb ppPat b, map (ppb ppPat) bs)
	       in  makelist(true,"") (b :: map forcemulti bs)
	       end

    fun ppPat (C,x) = ppCtor C ++ ppId x

    val dump_fresh =
	Flags.makeBoolFlag{name="/dump/fresh",default=false,
			   short="",long="dump-alpha",
			   desc="Dump alpha-renamed MiniML program"}
			  

    local 
	val counter = ref 0
    in
        fun fresh _ =
	      "_x" ^ Int.toString(!counter)
	    before
	      counter := !counter + 1
    end

    fun curry f x y = f(x,y)
    structure M = OrderFinMap(type T = string
                              val lt = curry String.<)

    type map = string M.map
    type 'pat freshpat = (map->string->(string*map)) -> map -> 'pat -> 'pat * map
    fun addFresh map x =
	let val x' = fresh x
	in  (x', M.add(x, x', map))
	end
    fun freshExp freshPat map exp =
	case exp of
	    Var x =>
	      ( case M.lookup map x of
		    SOME x' => Var x'
		  | NONE => raise Fail("Fresh: Unbound variable " ^ x)
              )
	  | Integer int => Integer int
	  | String str => String str
	  | Unit => Unit
	  | Const(C, e) => Const(C, freshExp freshPat map e)
	  | App(e1, e2) => 
	      App(freshExp freshPat map e1,freshExp freshPat map e2)
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
	  | Abs(x, e) =>
	      let val x' = fresh x
		  val map' = M.add(x, x', map)
	      in  Abs(x',freshExp freshPat map' e)
	      end
	  | Fix(f, x, e) => 
	      let val (f',x') = (fresh f, fresh x)
		  val map' = M.add(x, x', M.add(f, f', map))
	      in  Fix(f', x', freshExp freshPat map' e)
	      end
	  | Let(x, e1, e2) => 
	      let val x' = fresh x
		  val map' = M.add(x, x', map)
	      in  Let(x', freshExp freshPat map e1, freshExp freshPat map' e2)
	      end
	  | Info(i, e) => Info(i, freshExp freshPat map e)

    fun freshProg freshPat (Prog binds) =
	let fun f (ValBind(x,e),(bs,map)) =
		let val x' = fresh x
		    val e' = freshExp freshPat map e
		in  (ValBind(x',e')::bs, M.add(x,x',map))
		end
	      | f (b,(bs,map)) = (b::bs,map)
	    val (binds',_) = List.foldl f ([],M.empty) binds
	in  Prog(rev binds')
	end
    val fresh = freshProg
    fun freshPat add map (C,x) = 
	let val (x',map') = add map x
	in  ((C,x'), map')
	end

end

