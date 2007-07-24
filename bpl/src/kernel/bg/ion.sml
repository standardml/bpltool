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

(** Abstract data type for modelling ions.
 * @version $LastChangedRevision$
 *)

functor Ion'(structure Control : CONTROL
	     structure Name : NAME
	     structure NameSet : MONO_SET
	     structure NameSetPP : COLLECTIONPRETTYPRINT
               where type ppstream = PrettyPrint.ppstream
             structure NameBijectionConstraints : BIJECTION_CONSTRAINTS
	     sharing type Name.name = NameSet.elt
	     sharing type NameSet.Set =
                          NameBijectionConstraints.set =
                          NameSetPP.collection) : ION 
	     where type control  = Control.control
	       and type name     = Name.name
 	       and type nameset  = NameSet.Set
               and type nameconstraints = NameBijectionConstraints.constraints =
struct
  type control  = Control.control
  type name     = Name.name
  type nameset  = NameSet.Set
  type nameconstraints = NameBijectionConstraints.constraints

  type ion = {ctrl : control, free : name list, bound : nameset list}
  fun make x = x
  fun unmk x = x
  fun innernames {ctrl, free, bound} =
      foldl (fn (Xs, X) => NameSet.union X Xs) NameSet.empty bound
  fun outernames ({ctrl, free, bound}) = NameSet.fromList free

  exception WrongArity of ion

  exception UnknownControl of ion

  fun replacectrl ctrllist (ion as {ctrl, free, bound}) =
    let
      fun lookup c =
        let
          val cname = Control.name c
          fun lk [] = raise UnknownControl ion
            | lk (c' :: cs) =
              if cname = Control.name c' then
                c'
              else
                lk cs
        in
          lk ctrllist
        end
      val ctrl = lookup ctrl
    in
      if Control.bound ctrl <> length bound orelse
        Control.free ctrl <> length free then
        raise WrongArity ion
      else
        {ctrl = ctrl, free = free, bound = bound}
    end




  fun eq {ctrl = ctrl1, free = free1, bound = bound1}
         {ctrl = ctrl2, free = free2, bound = bound2} =
      Control.eq ctrl1 ctrl2
      andalso ListPair.all Name.== (free1, free2)
      andalso ListPair.all (fn (ns1, ns2) => NameSet.eq ns1 ns2) (bound1, bound2)

  structure Constraints = NameBijectionConstraints
  fun eq' C {ctrl = ctrl1, free = free1, bound = bound1}
            {ctrl = ctrl2, free = free2, bound = bound2} =
      let
        (* check that the bound namesets has the same
         * size and gather constraints *)
        fun check_bound [] [] C' = SOME C'
          | check_bound (b1::bs1) (b2::bs2) C' =
            if NameSet.size b1 = NameSet.size b2 then
              check_bound bs1 bs2 (Constraints.add ((b1, b2), C'))
            else
              NONE
          | check_bound _ _ _ = NONE
        
        (* generate result constraints (if possible) *)
        fun make_C [] [] C = SOME C
          | make_C (n1::ns1) (n2::ns2) C =
            make_C ns1 ns2
                   (Constraints.add
                      ((NameSet.singleton n1, NameSet.singleton n2), C))
          | make_C _ _ _ = NONE
      in
        case check_bound bound1 bound2 Constraints.empty of
          SOME C' =>
          if Control.eq ctrl1 ctrl2 andalso Constraints.are_combineable (C, C')
          then
            make_C free1 free2 Constraints.empty
          else
            NONE
        | NONE => NONE
      end
      

  fun pp' langle rangle lbrack rbrack
          indent pps ({ctrl, free, bound} : ion) =
      let
	open PrettyPrint
	val show = add_string pps
	fun << () = begin_block pps INCONSISTENT indent
	fun >> () = end_block pps
	fun <<< () = begin_block pps CONSISTENT indent
	fun >>> () = end_block pps
	fun brk () = add_break pps (1, 0)
	fun brk0 () = add_break pps (0, 0)
	fun pplist pp_y ys =
	    (<<(); show langle;
	     foldl (fn (y, notfirst) => 
		       (if notfirst then (show ","; brk()) else ();
			pp_y y;
			true)) false ys;
	     show rangle; >>())
      in
	<<<();
	show (#1 (Control.unmk ctrl));
	case bound of
	  [] =>
	  (case free of
	     [] => ()
	   | (y :: ys) => pplist (Name.pp indent pps) free)
	| (X :: Xs) =>
	  (brk0();
	   pplist (Name.pp indent pps) free;
	   brk0();
	   pplist (NameSetPP.ppbr indent lbrack rbrack pps) bound);
	>>>()
      end handle e => raise e
      
  val pp = pp' "[" "]" "[" "]"
  val oldpp = pp' "<" ">" "{" "}"
end


functor Ion (structure Control : CONTROL
	     structure Name : NAME
	     structure NameSet : MONO_SET
	     structure NameSetPP : COLLECTIONPRETTYPRINT
               where type ppstream = PrettyPrint.ppstream
             structure NameBijectionConstraints : BIJECTION_CONSTRAINTS
	     sharing type Name.name = NameSet.elt
	     sharing type NameSet.Set =
                          NameBijectionConstraints.set =
                          NameSetPP.collection) :> ION 
	     where type control  = Control.control
	       and type name     = Name.name
 	       and type nameset  = NameSet.Set
               and type nameconstraints = NameBijectionConstraints.constraints =
struct
  structure Ion = Ion'(structure Control = Control
		       structure Name = Name
		       structure NameSet = NameSet
                       structure NameBijectionConstraints = NameBijectionConstraints
		       structure NameSetPP = NameSetPP)
  open Ion
end
