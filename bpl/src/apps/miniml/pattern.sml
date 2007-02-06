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

(** (Full) patterns used during parsing - later compiled away by
 * match compilation.
 * @version $LastChangedRevision$
 * Modified: $Date: 2006/05/31 15:01:26 $ by: $Author: hniss $
 *)

structure Pattern :> PATTERN = struct

    datatype con = 
         TupleCon of int
       | ConstCon of { name : string, arity : int, span : int }
    datatype pat = PVar of string | PCon of con * pat list

    fun lt (TupleCon _) (ConstCon _) = true
      | lt (ConstCon{name=n1,...}) (ConstCon{name=n2,...}) = String.<(n1,n2)
      | lt _ _ = false
    structure ConSet 
      = OrderSet(type T = con val lt = lt)

    fun span (TupleCon _) = 0
      | span (ConstCon{span,...}) = span

    fun arity (TupleCon ar) = ar
      | arity (ConstCon{arity,...}) = arity

    open Pretty
    infixr 5 ^+
    infix 4 +^
    infixr 4 ++
    fun ppCon (TupleCon i) = Util.abort 24680
      | ppCon (ConstCon{name,arity,span}) = ppString name

    fun ppPat (PVar x) = ppString x
      | ppPat (PCon(con as TupleCon _,pats)) =
	compose("(" ^+ clist "#, " ppPat pats,0,2,0,ppString ")")
      | ppPat (PCon(con as ConstCon _,[])) = ppCon con
      | ppPat (PCon(con as ConstCon _,pats)) = 
	compose(break(0,0)((ppCon con +^ "("),clist "#, " ppPat pats),
		0,2,0,ppString ")")

end (* structure Pattern *)
