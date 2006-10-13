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

signature MINIML = 
sig

    (* datatype's defining the language - visible
     * to all consumers of structures of type AST.
     *)
    datatype operator = 
	Arith of string
      | Rel   of string

    val isRelational   : operator -> bool
    val isArithmetical : operator -> bool

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

    val mkLet : ('info,'pat) bind list -> ('info, 'pat) exp 
		-> ('info, 'pat) exp

    type map
    type 'pat freshpat = (map->string->(string*map)) -> map -> 'pat -> 'pat * map
    val fresh : 'pat freshpat -> ('info,'pat) prog -> ('info,'pat) prog
    val freshPat : pat freshpat
    val dump_fresh : bool ref

    val ppPat : pat Pretty.pp
    val ppExp : 'i Pretty.pp -> 'p Pretty.pp -> ('i,'p)  exp Pretty.pp
    val pp    : 'i Pretty.pp -> 'p Pretty.pp -> ('i,'p) prog Pretty.pp

    val ppExp' : 'p Pretty.pp -> ('i,'p)  exp Pretty.pp
    val pp'    : 'p Pretty.pp -> ('i,'p) prog Pretty.pp

end (* signature AST *)
