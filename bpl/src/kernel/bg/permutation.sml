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

(** Abstract data type for modelling bigraph permutations.
 * @version $LastChangedRevision$
 *)
functor Permutation (structure IntSet : MONO_SET
                       where type elt = int
		     structure NameSet : MONO_SET
		     structure Name : NAME
		     structure Interface : INTERFACE
		     structure ErrorHandler : ERRORHANDLER
                       where type ppstream    = PrettyPrint.ppstream
                         and type break_style = PrettyPrint.break_style
                         and type origin      = Origin.origin
		     structure NameSetPP : COLLECTIONPRETTYPRINT
                       where type ppstream = PrettyPrint.ppstream
		     sharing type Interface.nameset = NameSet.Set
		     sharing type NameSet.Set = NameSetPP.collection
	             sharing type Name.name = NameSet.elt)
	:> PERMUTATION 
	   where type nameset = NameSet.Set
             and type interface = Interface.interface =
struct
  type nameset = NameSet.Set
  type interface = Interface.interface

  type Mutable = unit
  type Immutable = unit

  open Debug
  open ErrorHandler

  val file_origin = Origin.mk_file_origin
                      "$BPL/src/kernel/bg/permutation.sml"
                      Origin.NOPOS
  fun mk_explainer errtitle (explainer : exn -> explanation list) e =
      Exp (LVL_USER, Origin.unknown_origin, mk_string_pp errtitle,
           explainer e)

  (** The permutation data type uses O(n) space, where n = width. *)
  type 'a permutation = {width : int,
		      pi : (int * nameset) array,
		      pi_inv : (int * nameset) array}

  exception LogicalError of string
  fun explain_LogicalError (LogicalError errtxt) =
      [Exp (LVL_LOW, file_origin, mk_string_pp errtxt, [])]
  val _ = add_explainer
            (mk_explainer
               "an internal error occurred"
               explain_LogicalError)

  exception NotPermutation of (int * nameset) list
  fun explain_NotPermutation (NotPermutation p) =
      let
        fun pp_elm indent pps (i, ns) =
            (PrettyPrint.add_string pps (Int.toString i);
             if NameSet.isEmpty ns then ()
             else NameSetPP.pp indent pps ns)
      in
        [Exp (LVL_USER, Origin.unknown_origin,
              mk_list_pp "[" "]" "," pp_elm p, []),
         Exp (LVL_LOW, file_origin, pp_nothing, [])]
      end
  val _ = add_explainer
            (mk_explainer "not a permutation" explain_NotPermutation)

  val array = Array.array
  val sub = Array.sub
  infix 8 sub
  val update = Array.update
  fun appi f array 
    = (Array.foldl (fn (x, i) => (f (i, x); i + 1)) 0 array; ())
  val acopy = Array.copy

  (** Determine whether some permutation is a nameless identity in
   * time O(n), where n = width.
   *)
  fun is_idn ({width, pi, pi_inv} : 'a permutation) =
      case Array.findi (fn (i, (j, X)) 
			   => i <> j orelse not (NameSet.isEmpty X)) pi of
	SOME _ => false
      | NONE => true

  fun pp indent pps (perm as {width, pi, pi_inv}) =
      let
	open PrettyPrint
	val show = add_string pps
	fun << () = begin_block pps INCONSISTENT indent
	fun >> () = end_block pps
	fun brk () = add_break pps (1, 0)
	fun ppmap ((j, X), notfirst) =
	    (if notfirst then (show ","; brk()) else ();
	     show (Int.toString j);
	     if not (NameSet.isEmpty X) then
	       NameSetPP.pp indent pps X
	     else
	       ();
             true)
      in
	if is_idn perm then
	  show ("idp_" ^ Int.toString width)
	else
	  (<<(); show "[";
	   Array.foldl ppmap false pi;
	   show "]"; >>())
      end

  val toString = PrettyPrint.pp_to_string 60 (pp 2)

  (** Construct a permutation in time O(n), where n = length Xs. *)
  fun make Xs =
      let
	val pi = Array.fromList Xs
	val width = Array.length pi
	val pi_inv = array (width, (~1, NameSet.empty))
	fun addinv (i, (j, X)) =
	    if j < width andalso #1 (pi_inv sub j) = ~1 then
	      update (pi_inv, j, (i, X))
	    else
	      raise NotPermutation (Xs)
      in
	appi addinv pi;
	{width = width, pi = pi, pi_inv = pi_inv}
      end
  (** Deconstruct a permutation in time O(n), where n = width. *)
  fun unmk {width, pi, pi_inv} = Array.foldr (op ::) [] pi
  (** Return the width of a permutation in time O(1). *)
  fun width {width, pi, pi_inv} = width
  (** Return the inner face of a permutation in time O(n), 
   * where n = width. 
   *)
  fun innerface {width, pi, pi_inv} =
      Interface.make {loc = Array.foldr 
			      (fn ((j, X), Xs) => X :: Xs) [] pi, 
		      glob = NameSet.empty}
  (** Return the outer face of a permutation in time O(n), 
   * where n = width. 
   *)
  fun outerface {width, pi, pi_inv} =
      Interface.make {loc = Array.foldr
			      (fn ((i, Y), Ys) => Y :: Ys) [] pi_inv, 
		      glob = NameSet.empty}
  (** Return an identity permutation in time O(n),
   * where n = length Xs. 
   *)
  fun id Xs = 
      let
	val width = length Xs
	val pi = array (width, (~2, NameSet.empty))
	fun mkpi i [] = ()
	  | mkpi i (X :: Xs) 
	    = (update (pi, i, (i, X)); mkpi (i + 1) Xs)
      in
	mkpi 0 Xs;
	{width = width, pi = pi, pi_inv = pi}
      end
  (** Return a nameless identity permutation in time O(n),
   * where n = width.
   *)
  fun id_n width =
      let
	val pi = array (width, (~3, NameSet.empty))
	fun mkpi i 
	  = if i < width then
	      (update (pi, i, (i, NameSet.empty)); mkpi (i + 1))
	    else
	      ()
      in
	mkpi 0;
	{width = width, pi = pi, pi_inv = pi}
      end
  (** Return the empty identity permutation in time O(1). *)
  val id_0 = 
      let 
	val pi = array (0, (~4, NameSet.empty)) 
      in
	{width = 0, pi = pi, pi_inv = pi} 
      end

  fun copy {width, pi, pi_inv}
    = let
        val newpi = array (width, (0, NameSet.empty))
        val newpi_inv = array (width, (0, NameSet.empty))
        val _ = acopy {src = pi, dst = newpi, di = 0}
        val _ = acopy {src = pi_inv, dst = newpi_inv, di = 0}
      in
        {width = width, pi = newpi, pi_inv = newpi_inv}
      end

  fun unchanged perm : 'kind permutation = perm

  exception Uncomposable of Mutable permutation * Mutable permutation * string
  fun explain_Uncomposable (Uncomposable (p1, p2, errtxt)) =
      map (fn p => Exp (LVL_USER, Origin.unknown_origin,
                        pack_pp_with_data pp p, [])) [p1, p2]
      @ [Exp (LVL_LOW, file_origin, mk_string_pp errtxt, [])]
  val _ = add_explainer
            (mk_explainer "permutations are not composable" explain_Uncomposable)

  fun compose (perm1 
		 as {width = width1, pi = pi1, pi_inv = pi_inv1},
	       perm2
		 as {width = width2, pi = pi2, pi_inv = pi_inv2}) =
      if width1 <> width2 then
	raise Uncomposable (copy perm1, copy perm2,
			    "unequal width in compose")
      else
	let
	  val pi = array (width1, (~5, NameSet.empty))
	  val pi_inv = array (width1, (~6, NameSet.empty))
	  fun compentry (i, (j1, X1)) =
	      let
		val (j2, X2) = pi1 sub j1
	      in
		if NameSet.eq X1 X2 then 
		  (update (pi, i, (j2, X2));
		   update (pi_inv, j2, (i, X2)))
		else
		  raise Uncomposable 
			(copy perm1, copy perm2,
			 "different local names in compose")
	      end
	in
	  appi compentry pi2;
	  {width = width1, pi = pi, pi_inv = pi_inv}
	end

  (** Return the tensor product of a list of permutations in time O(?),
   * where n is the sum of widths.
   *)
  fun ** (pis : 'kinda permutation list) = 
      let
	val width = foldl (fn (pi, n) => n + #width pi) 0 pis
	val pi = array (width, (~7, NameSet.empty))
	val pi_inv = array (width, (~8, NameSet.empty))
      in
	foldl
	  (fn ({pi = pi', ...} ,offset) =>
	      (appi (fn (i, (j, X)) 
			=> (update (pi, offset + i, (offset + j, X));
			    update (pi_inv, offset + j, (offset + i, X))))
		    pi';
	       offset + Array.length pi'))
	  0
	  pis;
	{width = width, pi = pi, pi_inv = pi_inv}
      end

  (** Return the tensor product of two permutations in time O(n),
   * where n = width1 + width2.
   *)
  fun x ({width = width1, pi = pi1, pi_inv = pi_inv1},
	 {width = width2, pi = pi2, pi_inv = pi_inv2}) =
      let
	val width = width1 + width2
	val pi = array (width, (~7, NameSet.empty))
	val pi_inv = array (width, (~8, NameSet.empty))
      in
	acopy {src = pi1, dst = pi, di = 0};
	acopy {src = pi_inv1, dst = pi_inv, di = 0};
	appi (fn (i2, (j2, X2)) 
		 => (update (pi, width1 + i2, (width1 + j2, X2));
		     update (pi_inv, width1 + j2, (width1 + i2, X2))))
	     pi2;
	{width = width, pi = pi, pi_inv = pi_inv}
      end

  (** Return the inverse of a permutation.
   * @params pi
   * @param pi  permutation to invert.
   * @return    the inverse of pi.
   *)
  fun invert (perm as {width, pi, pi_inv}) =
      {width = width, pi = pi_inv, pi_inv = pi}

  (** Permute the list of values as described by the permutation.
    * @params pi Xs
    * @param pi  the permutation.
    * @param Xs  the list of values to permute.
    *)
  fun permute (perm as {width, pi, pi_inv} : 'kind permutation) Xs =
      let
        val permuted = Array.fromList Xs
      in
        foldl (fn (x, i) =>
                  (update (permuted, #1 (pi sub i), x);
                   i + 1))
              0 Xs;
        Array.foldr (op ::) [] permuted
      end

  (** Signal that the length of the list of inner name lists does not
   * match the width of the permutation.
   *)
  exception InterfaceMismatch
	    of string * Mutable permutation * nameset list list * string
  (** Push a permutation through a product of primes in time O(m+n),
   * where m = width and n = length Xss. *)
  (* @params pi Xss
   * @param pi   permutation to push through n primes.
   * @param Xss  list of local inner name lists.  The jth (0 <= j < n)
   *             local inner name list is a list of name sets
   *             describing the inner names of the sites of the jth prime.
   * @return     a permutation pi_Xss such that pi o (P_0 x ... x P_n-1)
   *             = (P_pi(0) x ... x P_pi(n-1)) pi_Xss.
   * @see bigraph literature on the Pushthrough Lemma.
   *)
  fun pushthru (perm as {width, pi, pi_inv}) Xss =
      let
        (* macc_Xs is an array of pairs (macc_i, Xs_i), where macc
	 * is the accumulated number of inner name sets (i.e., inner
	 * widths) for sites
	 * of primes P_0..P_i-1, and Xs_i is the list of inner name
	 * sets of prime P_i.
	 *)
	val macc_Xs = array (width, (~9, []))
	val (i', width')
	  = (foldl (fn ((m, Xs), (i, m_sum)) 
		       => (update (macc_Xs, i, (m_sum, Xs)); 
			   (i + 1, m + m_sum)))
		   (0, 0)
	     o map (fn Xs => (length Xs, Xs)))
	      Xss
            handle Subscript => (~1, ~1)
        val _
          = if width <> i' then
	      raise InterfaceMismatch
	        ("permutation.sml", copy perm, Xss, "in pushthru")
            else ()

	val pi' = array (width', (~10, NameSet.empty))
	val pi_inv' = array (width', (~11, NameSet.empty))
        (* Given a mapping from pi_inv, j |-> (i, _), addmaps adds
	 * the maps corresponding to the sites of P_i to pi' and
	 * pi_inv'.  noff is the number of inner name sets (i.e.,
	 * inner widths) for sites of primes
	 * P_{pi_inv(0)}..P_{pi_inv(j-1)}.  Addmap returns 
	 * noff + n, where n is the inner width of P_i.
	 *)
	fun addmaps (j, (i, _), noff) =
	    let
	      val (moff, Xs) 
		= macc_Xs sub i
	      fun addmap (X, j')
		= (update (pi', noff + j', (moff + j', X));
		   update (pi_inv', moff + j', (noff + j', X));
		   j' + 1)
	      val n = foldl addmap 0 Xs;
	    in	    
	      noff + n
	    end
      in
	Array.foldli addmaps 0 pi_inv;
	{width = width', pi = pi', pi_inv = pi_inv'}
      end

  (** Signal that a permutation is not regularizable relative to a list
   *  of local inner name lists.
   *)
  exception NotRegularisable of Mutable permutation * nameset list list
  fun explain_NotRegularisable (NotRegularisable (p, nsss)) =
      [Exp (LVL_USER, Origin.unknown_origin, pack_pp_with_data pp p, []),
       Exp (LVL_USER, Origin.unknown_origin,
            mk_list_pp "{" "}" "," (mk_list_pp' "{" "}" "," NameSetPP.pp) nsss,
            []),
       Exp (LVL_LOW, file_origin, pp_nothing, [])]
  val _ = add_explainer
            (mk_explainer "permutation is not regularisable"
                          explain_NotRegularisable)

  (** Split a permutation into one major and a number of minor
   * permutations.
   * @params pi Xss
   * @param Xss  list of local inner name lists.
   * @exception NotRegularisable  if the permutation cannot be
   *                              regularized.
   *)
  fun split (perm as {width, pi, pi_inv} : 'kinda permutation) Xss =
      let
        val k = length Xss
        val major_pi = array (k, (~12, NameSet.empty))
        val major_pi_inv = array (k, (~13, NameSet.empty))

        val ns = map length Xss

        val minors 
            = Array.fromList
                (map 
                   (fn n =>
                       let
                         val minor_pi
                             = array (n, (~15, NameSet.empty))
                         val minor_pi_inv
                             = array (n, (~16, NameSet.empty))
                       in
                         {width = n,
                          pi = minor_pi,
                          pi_inv = minor_pi_inv}
                       end)
                   ns)

        (* j is n-located at #1 nlocated[j]  and
         * j's position within that group is #2 nlocated[j] *)
        val nlocated = array (width, (~17, ~17))
        val mlocated = array (width, (~18, ~18))
        local
          fun nlocatedAt (j, x, (l, n::ns, sumns)) =
              let
                val j' = j - sumns
              in
                if j' < n then
                  (update (nlocated, j, (l, j'));
                   (l, n::ns, sumns))
                else
                  nlocatedAt (j, x, (l + 1, ns, n + sumns))
              end
            | nlocatedAt _ =
                raise LogicalError ("the function nlocatedAt was \
                                    \unexpectedly called with an \
                                    \empty list")
          fun mlocatedAt (j, _, (l, i)) =
              (update (mlocated, j, (l, i));
               if j >= (width - 1)
                    orelse #1 (nlocated sub (#1 (pi sub j)))
                             = #1 (nlocated sub (#1 (pi sub (j + 1)))) then
                 (l, i + 1)
               else
                 (l + 1, 0))
        in
          val _ = (Array.foldli nlocatedAt (0, ns, 0) nlocated;
                   if #1 (Array.foldli mlocatedAt (0, 0) mlocated) >= k then
                     raise NotRegularisable (copy perm, Xss)
                   else
                     ())
        end

        fun make_minor_pi (Xs, (j, offset)) =
            let
              val {pi = minor_pi, pi_inv = minor_pi_inv, ...}
                  = minors sub j
            in
              (j + 1,
               foldl
                 (fn (X, i) =>
                     let
                       val (nloc, npos)  = nlocated sub i
                       val (pi_inv_i, _) = pi_inv sub i
                       val (mloc, mpos)  = mlocated sub pi_inv_i
                     in
                       (update (minor_pi, mpos, (npos, X));
                        update (minor_pi_inv, npos, (mpos, X));
                        (* the same entry is updated for
                           each site in that group... *)
                        update (major_pi, mloc, (nloc, NameSet.empty));
                        update (major_pi_inv, nloc,
                                (mloc, NameSet.empty));
                        i + 1)
                     end)
                 offset Xs)
            end
      in
        (foldl make_minor_pi (0, 0) Xss;
         {major = {width  = k,
                   pi     = major_pi,
                   pi_inv = major_pi_inv},
          minors = Array.foldr (op ::) [] minors})
      end

  exception UnequalLengths of nameset list list * nameset list list * string
  fun explain_UnequalLengths (UnequalLengths (nsss1, nsss2, errtxt)) =
      [Exp (LVL_USER, Origin.unknown_origin,
            mk_list_pp "{" "}" "," (mk_list_pp' "{" "}" "," NameSetPP.pp) nsss1,
            []),
       Exp (LVL_USER, Origin.unknown_origin,
            mk_list_pp "{" "}" "," (mk_list_pp' "{" "}" "," NameSetPP.pp) nsss2,
            []),
       Exp (LVL_LOW, file_origin, mk_string_pp errtxt, [])]
  val _ = add_explainer 
            (mk_explainer "lists of unequal length" explain_UnequalLengths)

  (** Compute a permutation for unzipping tensor products
   * in time O(?).
   *)
  (* @return a permutation ~pi such that 
   * merge ((X_{i< n} Ai) x X_{i< n} Mi) ~pi = merge X_{i< n} Ai x Mi,
   * where Ai : < li, Uis> -> Ji and Mi : < li', U'is> -> J'i.
   * @params Uiss U'iss
   * @param Uiss   list of Uis.
   * @param U'iss  list of U'is.
   * @exception UnequalLengths  if the lists have different lengths.
   *)
  fun unzip Uiss U'iss =
      let
	(* computesums computes the sums of lis and li's. *)
	fun computesums [] [] lsum l'sum = (lsum, l'sum)
	  | computesums [] (U's :: U'ss) lsum l'sum 
	    = computesums [] U'ss lsum (l'sum + length U's)
	  | computesums (Us :: Uss) U'ss lsum l'sum
	    = computesums Uss U'ss (lsum + length Us) l'sum
	val (lsum, l'sum) = computesums Uiss U'iss 0 0

	val width = lsum + l'sum
	val pi = array (width, (~12, NameSet.empty))
	val pi_inv = array (width, (~13, NameSet.empty))

	(* addmapss Uss U'ss accl accl' adds maps to pi and pi_inv.
	 * accl = l0 + ... + l{i-1}, where Us is the ith list, and
	 * accl' = l0' + ... + l{i-1}', where U's is the ith list.
	 *)
	fun addmapss [] [] _ _ = ()
	  | addmapss (Us :: Uss) (U's :: U'ss) accl accl' =
	    let
	      (* addmaps lsum0 accl0 i0 i Us  adds maps corresponding to each
	       * set Ui in Us.  addmaps returns i + the length of Us.
	       *)
	      fun addmaps lsum0 accl0 i0 i [] = i
		| addmaps lsum0 accl0 i0 i (Ui :: Us) = 
		  (update (pi, 
			   accl + accl' + i0 + i, 
			   (lsum0 + accl0 + i, Ui));
		   update (pi_inv, 
			   lsum0 + accl0 + i, 
			   (accl + accl' + i0 + i, Ui));
		   addmaps lsum0 accl0 i0 (i + 1) Us)
	      val li = addmaps 0 accl 0 0 Us
	      val l'i = addmaps lsum accl' li 0 U's
	    in
	      addmapss Uss U'ss (accl + li) (accl' + l'i)
	    end	    
	  | addmapss Uss U'ss _ _ = 
	    raise UnequalLengths (Uss, U'ss, "in addmapss")
      in
	addmapss Uiss U'iss 0 0;
	{width = width, pi = pi, pi_inv = pi_inv}
      end

  fun swap (perm as {width, pi, pi_inv} : Mutable permutation) (i, j) =
    let
      val iimg as (idx, Xi) = pi sub i
      val jimg as (jdx, Xj) = pi sub j
      val _ = update (pi, i, (jdx, Xi))
      val _ = update (pi, j, (idx, Xj))
      val _ = update (pi_inv, idx, (j, Xj))
      val _ = update (pi_inv, jdx, (i, Xi))
    in
      perm
    end

  (** Determine whether some permutation is the identity in time
   * O(n), where n = width.
   *)
  fun is_id ({width, pi, pi_inv} : 'kind permutation) =
      case Array.findi (fn (i, (j, _)) => i <> j) pi of
	SOME _ => false
      | NONE => true

  (** Determine whether some permutation is the zero identity in
   * time O(1).
   *)
  fun is_id0 ({width, ...} : 'kind permutation) = width = 0

  val op * = x

  (** Return the composition of two permutations in time O(n) * O(s),
   * where O(s) is the time for local name set comparison.
   *)
  val op o = compose
end
