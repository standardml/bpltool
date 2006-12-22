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

(** Abstract data type for modelling bigraph interfaces. 
 * @version $LastChangedRevision$
 *)
functor Interface'(structure NameSet : MONO_SET
		   structure NameSetPP : COLLECTIONPRETTYPRINT
                     where type ppstream    = PrettyPrint.ppstream
		   sharing type NameSet.Set = NameSetPP.collection)
	: INTERFACE where type nameset = NameSet.Set =
struct
  type nameset = NameSet.Set

  type interface = {width : int, loc : nameset list, glob : nameset}

  fun make {loc, glob} = {width = length loc, loc = loc, glob = glob}
  fun unmk {width, loc, glob} = {width = width, loc = loc, glob = glob}

  fun width {width, loc, glob} = width
  fun loc {width, loc, glob} = loc
  fun glob {width, loc, glob} = glob
  fun names {width, loc, glob} =
      foldl (fn (X, all) => NameSet.union X all) glob loc


  fun ListPair_allEq p ([], []) = true
    | ListPair_allEq p (x::xs, y::ys)
      = p(x,y) andalso ListPair_allEq p (xs,ys)
    | ListPair_allEq _ _ = false

  fun eq ({width = width1, loc = loc1, glob = glob1}, 
	  {width = width2, loc = loc2, glob = glob2}) =
      width1 = width2 andalso
      NameSet.eq glob1 glob2 andalso
      ListPair_allEq (fn (X1, X2) => NameSet.eq X1 X2) (loc1, loc2)

  fun x ({width = width1, loc = loc1, glob = glob1},
	 {width = width2, loc = loc2, glob = glob2}) =
      {width = width1 + width2,
       loc = loc1 @ loc2,
       glob = NameSet.union glob1 glob2}

  val zero = {width = 0, loc = [], glob = NameSet.empty}

  val one = {width = 1, loc = [NameSet.empty], glob = NameSet.empty}

  fun m m = 
      let
	fun mptis i = if i <= 0 then
			[]
		      else
			NameSet.empty :: mptis (i - 1)
      in
	{width = m, loc = mptis m, glob = NameSet.empty}
      end

  fun X X = {width = 0, loc = [], glob = X}
  fun pp indent pps (I as {width, loc, glob}) =
      let
	open PrettyPrint
	val show = add_string pps
	fun <<< () = begin_block pps CONSISTENT indent
	fun >>> () = end_block pps
	fun << () = begin_block pps INCONSISTENT indent
	fun >> () = end_block pps
	fun brk () = add_break pps (1, 0)
      in
	case width of
	  0 => if NameSet.isEmpty glob then
		 show "0"
	       else
		 NameSetPP.pp indent pps glob
	| 1 => if NameSet.isEmpty glob then
		 if NameSet.isEmpty (hd loc) then
		   show "1"
		 else
		   NameSetPP.ppbr indent "(" ")" pps (hd loc)
	       else
		 (<<(); show "<"; 
		  if NameSet.isEmpty (hd loc) then 
		    ()
		  else
		    (NameSetPP.ppbr indent "(" ")" pps (hd loc);
		     show ","; brk());
		  NameSetPP.pp indent pps glob;
		  show ">"; >>())
	| m => if NameSet.isEmpty glob andalso
		  List.all NameSet.isEmpty loc then
		 show (Int.toString m)
	       else
		 (<<<(); show "<"; 
		  show (Int.toString m);
		  show ","; brk();
		  if List.all NameSet.isEmpty loc then 
		    ()
		  else
		    (<<(); show "[";
		     foldl 
		       (fn (X, notfirst) =>
			   (if notfirst then (show ","; brk()) else ();
			    NameSetPP.pp indent pps X;
			    true))
		       false
		       loc;
		     show "],"; >>(); brk());
		  NameSetPP.pp indent pps glob;
		  show ">"; >>>())
      end

  val op * = x

end


functor Interface (structure NameSet : MONO_SET
		   structure NameSetPP : COLLECTIONPRETTYPRINT
                     where type ppstream    = PrettyPrint.ppstream
		   sharing type NameSet.Set = NameSetPP.collection)
	:> INTERFACE where type nameset = NameSet.Set =
struct
  structure Interface = Interface'(structure NameSet = NameSet
				   structure NameSetPP = NameSetPP)
  open Interface
end