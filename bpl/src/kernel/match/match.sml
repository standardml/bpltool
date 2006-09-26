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

(** Match datatype and inference.
 * @version $LastChangedRevision$
 *)
 
functor Match (
  structure Permutation : PERMUTATION
  structure Wiring : WIRING
  structure Interface : INTERFACE
  structure BgBDNF : BGBDNF
  structure NameSet : MONO_SET
  structure LazyList : LAZYLIST
  structure PrettyPrint : PRETTYPRINT
  sharing type Permutation.nameset = NameSet.Set
  sharing type NameSet.Set = Wiring.nameset
                           = Interface.nameset
  sharing type Interface.interface = BgBDNF.interface
  sharing type PrettyPrint.ppstream
             = BgBDNF.ppstream
             = Permutation.ppstream
             = Wiring.ppstream) : MATCH
  where type 'class bgrbdnf  = 'class BgBDNF.bgrbdnf
    and type B               = BgBDNF.B
    and type D               = BgBDNF.D
    and type nameset         = NameSet.Set
    and type ppstream        = PrettyPrint.ppstream
    and type 'a lazylist     = 'a LazyList.lazylist =
struct
  type 'class bgrbdnf  = 'class BgBDNF.bgrbdnf
  type B        = BgBDNF.B
  type D        = BgBDNF.D
  type ppstream = PrettyPrint.ppstream
  type Mutable = Permutation.Mutable
  type 'kind permutation = 'kind Permutation.permutation
  type nameset = NameSet.Set
  open LazyList
  
  type match = {context : B bgrbdnf,
                redex : B bgrbdnf,
                parameter : D bgrbdnf}

  fun unmk m = m

  (* Signals that there are no more new ways of splitting. *)
	exception NoMoreSplits
	
	exception ThisCannotHappen

	(* A split of m elements into n parts is a list of n integers
	 * whose sum is m, e.g. [m, 0, 0, ..., 0] or [1, 0, 3, ..., 1].
	 * They can be totally ordered.
	 *)

  (* Return the first way of splitting m elements into n parts. *)
  fun firstsplit 0 0 = []
    | firstsplit x 0 = raise NoMoreSplits
    | firstsplit m n =
    let
      fun zeros 0 = []
        | zeros n = 0 :: zeros (n - 1)
    in
      m :: zeros (n - 1)
    end

  (* Given a split, return the next way of splitting, i.e., the 
   * smallest split larger than the given one.
   *)
  fun nextsplit xs =
      let
        fun nextsplit' [] = raise NoMoreSplits
          | nextsplit' [x] = raise NoMoreSplits
          | nextsplit' (0 :: xs) =
            (case nextsplit' xs of
              (x', xs') => (x', 0 :: xs'))
          | nextsplit' (x :: x' :: xs) = (x - 1, x' + 1 :: xs)
        val (y, ys) = nextsplit' xs
      in
        y :: ys
      end

  (* An ordered permutation is a permutation with additional data that
   * supports incremental generation of all permutations of this width.
   *)
  datatype dir = Left | Right
  type 'kind operm = int * 'kind permutation * (int * dir) list
  val toPermutation : 'kind operm -> 'kind permutation = #2

  (* Signals that there are no more new permutations. *)
  exception NoMorePerms

  (* Return the first permutation in the ordering. *)
  fun firstperm names : Mutable operm =
    let
    	fun poslist 0 = []
	      | poslist n = (n - 1, Left) :: poslist (n - 1)
	    val pi = Permutation.copy (Permutation.id names)
	    val n = Permutation.width pi
	  in
      (n, pi, poslist n)
    end
  
  (* Update _destructively_ permutation p and return it as the next
   * permutation in the ordering.
   * This implementation uses the Johnson-Trotter algorithm.
   *)
  fun nextperm (n, pi, poss) =
    let
      fun np offset i [] = raise NoMorePerms
        | np offset i [_] = raise NoMorePerms
        | np offset i ((p, Left) :: ps) =
          if (p <= 0) then
            (p, Right) :: np (offset + 1) (i - 1) ps
          else
            (Permutation.swap pi (offset + p - 1, offset + p);
             (p - 1, Left) :: ps)
        | np offset i ((p, Right) :: ps) =
          if (p >= i - 1) then
            (p, Left) :: np offset (i - 1) ps
          else
            (Permutation.swap pi (offset + p, offset + p + 1);
             (p + 1, Right) :: ps)
      val newposs = np 0 n poss
    in
      (n, pi, newposs)
    end 
    
  fun matches {agent, redex} = lzNil (* = not yet implemented! *)

    (*********************************)
    (*                               *)
    (* MAIN MATCHING ALGORITHM HERE! *)
    (*                               *)
    (*********************************)


  fun amatch agentredex =
    case lzunmk (matches agentredex) of
      Nil => NONE
    | Cons (m, ms) => SOME m

  val allmatches : {agent : B bgrbdnf, redex : B bgrbdnf }
                 -> match list = lztolist o matches
  
  fun pp indent pps ({context, redex, parameter} : match) =
    let
      open PrettyPrint
       val show = add_string pps
       fun << () = begin_block pps CONSISTENT indent
	     fun >> () = end_block pps
	     fun brk () = add_break pps (1, 0)
	     val Z = Interface.glob (BgBDNF.routerface parameter)
			 val id_Z = Wiring.id_X Z
    in
			<<();
 			  show "("; BgBDNF.ppr indent pps context; show ")";
			  brk();
			  show "o ";
			  <<();
			    show "("; Wiring.pp indent pps id_Z;
			    brk();
			    show "* "; BgBDNF.ppr indent pps redex; show ")";
			  >>();
			  brk();
			  show "o ("; BgBDNF.ppr indent pps parameter; show ")";
			>>()
    end
end
