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

(** Match datatype and inference.  For theoretical background
 * information, see                                          <ul><li>
 * Birkedal, Damgaard, Glenstrup, and Milner:                    <em>
 * Matching of bigraphs.                                        </em>
 * In Proceedings of Graph Transformation for Verification and
 * Concurrency Workshop 2006
 * Electronic Notes in Theoretical Computer Science. Elsevier.</li><li>
 * Damgaard and Glenstrup: <em>Normal inferences in Bigraph Matching.</em>
 * Internal note 2006, the BPL Project. IT University of Copenhagen.</li></ul>
 * @version $LastChangedRevision$
 *)
 
functor Match (
  structure Name        : NAME
  structure Link        : LINK
  structure LinkSet     : MONO_SET
  structure Permutation : PERMUTATION
  structure Wiring      : WIRING
  structure Interface   : INTERFACE
  structure BgVal       : BGVAL
  structure BgBDNF      : BGBDNF
  structure NameSet     : MONO_SET
  structure LazyList    : LAZYLIST
  structure PrettyPrint : PRETTYPRINT
  type info
  sharing type Permutation.nameset = NameSet.Set
  sharing type Permutation.permutation = BgBDNF.permutation
  sharing type Wiring.wiring = BgVal.wiring
                             = BgBDNF.wiring
  sharing type NameSet.Set = Link.nameset
                           = Wiring.nameset
                           = Interface.nameset
                           = BgBDNF.nameset
                           = BgVal.nameset
  sharing type Name.name = Link.name
                         = NameSet.elt
  sharing type LinkSet.Set = Wiring.linkset
  sharing type LinkSet.elt = Link.link
  sharing type Interface.interface = BgBDNF.interface
  sharing type BgVal.bgval = BgBDNF.bgval
  sharing type BgVal.bgmatch = BgBDNF.bgmatch
  sharing type PrettyPrint.ppstream
             = BgBDNF.ppstream
             = Permutation.ppstream
             = Wiring.ppstream
  sharing type info = BgBDNF.info
                    = BgVal.info) :> MATCH
  where type info            = info
    and type 'class bgbdnf   = 'class BgBDNF.bgbdnf
    and type BR              = BgBDNF.BR
    and type DR              = BgBDNF.DR
    and type nameset         = NameSet.Set
    and type ppstream        = PrettyPrint.ppstream
    and type 'a lazylist     = 'a LazyList.lazylist =
struct
  open BgVal
  type info = info
  type 'class bgbdnf  = 'class BgBDNF.bgbdnf
  type B        = BgBDNF.B
  type BR       = BgBDNF.BR
  type DR       = BgBDNF.DR
  type P        = BgBDNF.P
  type ppstream = PrettyPrint.ppstream
  type Mutable  = Permutation.Mutable
  type 'kind permutation = 'kind Permutation.permutation
  type nameset  = NameSet.Set
  open LazyList

  val bgvalmatch = BgVal.match
  val MalformedBDNF = BgBDNF.MalformedBDNF
  val make_P = BgBDNF.make_P
  val make_N = BgBDNF.make_N
  val unmkBR = BgBDNF.unmkBR
  val unmkDR = BgBDNF.unmkDR
  val unmkP = BgBDNF.unmkP
  val unmkN = BgBDNF.unmkN
  val innerface = BgBDNF.innerface
  val outerface = BgBDNF.outerface
  val width = Interface.width
  val loc = Interface.loc
  val glob = Interface.glob
  val permute = Permutation.permute
  val pushthru = Permutation.pushthru

  type match = {context : B bgbdnf,
                redex : BR bgbdnf,
                parameter : DR bgbdnf}

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
  type Immutable = Permutation.Immutable
  type Mutable = Permutation.Mutable
  val toPermutation : 'kind operm -> 'kind permutation = #2

  (* Signals that there are no more new permutations. *)
  exception NoMorePerms

  (* Signals that an error during grouping of tensor factors. *)
  exception GroupError of string * string * P bgbdnf list * int list

  (* Signals that a tuple of lists contained lists of unequal length. *)
  exception UnequalLengths
   of wiring list * wiring list * wiring list * int * P bgbdnf list list

  (* Returns a mutable copy of a perm. *)
  fun permcopy (n, pi, ps) = (n, Permutation.copy pi, ps)

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
  
  (* Return the first permutation in the ordering. *)
  fun firstperm_n n : Mutable operm =
    let
    	fun poslist 0 = []
        | poslist n = (n - 1, Left) :: poslist (n - 1)
      val pi = Permutation.copy (Permutation.id_n n)
    in
      (n, pi, poslist n)
    end
  
  (* Update _destructively_ permutation p and return it as the next
   * permutation in the ordering.  The inner face of the permutation
   * is preserved (cf. Permutation.swap).
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

  (* Matching rule functions
   * =======================
   *
   * Non-primed functions implement rules used at or below the SWX
   * rule, while primed functions implement rules used above it.
   *
   * Non-primed functions:
   * --------------------
   * All rule functions used in non-prime mode, except CLO, take
   * some specific arguments, as well as these general ones:
   * - usednames  a set containing all names that must NOT be used
   *              as outer names for s_a_e'
   * - s_a_e      substitution representing agent edge links
   * - s_a_n      substitution representing agent name links
   * - s_R_e      substitution representing redex edge links
   * - s_R_n      substitution representing redex name links
   *
   * They must return specific values, as well as
   * - s_a_e'     substitution representing agent edge links
   * - s_C        substitution representing context links
   *
   * where s_a_e' = Y/ * alpha s_a_e, i.e., s_a_e' must be an
   * outer-name-extended and renamed version of s_a_e.
   * Links of s_R_e must only be matched with links of s_a_e.
   * The substitutions in the theoretical note are
   * sigma^a = s_a_e' * s_a_n  and  sigma^R = s_R_e * s_R_n.
   *
   * Primed functions:
   * ----------------
   * All rule functions used in prime mode are used with s_R = id_0
   * and redex = id, which are therefore not passed as arguments.
   * They take some
   * specific arguments, as well as these general ones:
   * - s_a_e      substitution representing agent edge links
   * - s_a_n      substitution representing agent name links
   * - s_C_e      substitution representing (part of) context edge links
   * - s_C_n      substitution representing (part of) context name links
   *
   * They must return specific values, as well as
   * - s_a_e'     substitution representing agent edge links
   * - s_C        substitution representing (part of) context edge links
   *
   * where s_a_e' = Y/ * alpha s_a_e, i.e., s_a_e' must be an
   * outer-name-extended and renamed version of s_a_e.
   * Letting sigma^C represent the context substitution of the
   * theoretical note, s_C must fulfill
   * sigma^C = s_C (id * s_C_n) * s_C_e and
   * links of s_C_e must only be matched with links of s_a_e.
   
   OLD SCRAP:
   * a set, usednames, and 4 substitutions s_a', s_a, s_R', s_R
   * as well as other arguments, and return something, as well as 
   * s_C and s_a'', the latter being an outer-name-extended and renamed
   * version of s_a', i.e., s_a'' = Y/ * alpha s_a'.
   * The substitutions in the theoretical note are
   * sigma^a = s_a'' * s_a  and  sigma^R = s_R' * s_R; the links
   * of s_R' must be matched with links of s_a' (because
   * they correspond to the closed edges of the redex).
   * The set usednames contains all names that must NOT be used as
   * outer names for s_a'' (think of them as the outer names of
   * s_a, plus Y_R in the CLO rule).
   *
   * All rule functions used in prime mode are used with s_R = id_0,
   * which is therefore not passed as argument.  These functions
   * take ... substitutions s_a', s_a, s_C', s_C, as well as
   * other arguments, and return something, as well as s_C''
   * and s_a'', the latter being as above, the former being such that
   * sigma^C = s_C'' (id * s_C' * s_C).  Links of s_C' must be
   * matched with links of s_a'' (because they correspond to the
   * closed edges of redex and agent, respectively).
   *)

  datatype primetype = DiscretePrime | GlobalDiscretePrime

  (* Match a global discrete prime using the PAX rule:
   * Assuming X is a subset of g's outer names,
   * return s_a_e' = s_a_e, s_C = s_a_n * s_a_e, G = "X", Ps = (X)g
   *)
	fun matchPAX {s_a = {s_a_e, s_a_n}, g, X} = lzNil

  (* Match a global discrete prime using the SWX rule:
   * Assuming Ps = (id_Z * ^s)(W)G,
   * 1) Let s_C_e = s_R_e, s_C_n = s * s_R_n, and infer premise
   *    using s_a_e, s_a_n, s_C_e, s_C_n, g, G,
   *    yielding s_a_e', s_C, and qs
   * 2) Let Y_C_e = outernames s_C_e
   * 3) Return s_a_e', s_C := s_C * id_Y_C_e, qs
	 *)
	fun matchSWX {s_a = {s_a_e, s_a_n}, s_R = {s_R_e, s_R_n}, g, s, G} = lzNil
	
  (* Match a global discrete prime using the MER rule:
   * 1) ...
	 *)
  fun matchMER {s_a, s_R, g, Ps} = lzNil
  
  (* Match a global discrete prime using a SWX, PAX or MER rule:
   * 1) Try using PAX rule if s_R_e = s_R_n = id_0 and
   *    Ps = id_(X) for some X subset of g's outer names,
   * 2) Else try using SWX rule if Ps = [P],
   *    where P = (id_Z * ^s)(W)G,
   * 3) Else try using MER rule if... 
   *)
(* NEW:  fun matchDG {usednames, s_a, s_R = {s_R_e, s_R_n}, g, Ps} = lzNil 
OLD:*)
fun matchDG usednames s_a s_a' s_R g Ps = lzNil

  (* Match a global discrete prime using the ION rule, if possible:
   * 1) Determine how many (top-level) molecules it contains.
   * 2) If 1 molecule, match an ion:
   *    2a) For agent ion K_yX, compute the set Y = {vec y}
   *    2b) Compute s_Y || s_a^new = s_a by domain-restricting
   *        s_a to Y
   *    2c) Compute s'_Y || s_a'^new = s_a' by domain-restricting
   *        s'_a to Y
   *    2d) Construct p = (id * (vec v)/(vec X))n and infer premise
   *        using s_a'^new, s_a^new, s_R, p, and Ps, yielding
   *        s_a''^new, s_C^new, P = (id * (vec v)/(vec Z))N, and qs
   *    2e) Construct s_C = s_Y || s'_Y || s_C^new
   *        and     s_a'' = s'_Y || s_a''^new
   *        and         G = (id * K_vZ)N
   *    2f) Return s_a'', s_C, G, qs
   * 3) Otherwise, perform a D_G match
   *)
  fun matchDS usednames s_a' s_a s_R e Ps = lzNil
  
  
  (* Match an abstraction:
   * 1) Deconstruct p, yielding s_a_L : Z -> W and g.
   * 2) Compute s_a := s_a_L * s_a and add outer names of s_a_L to usednames.
   * 3) Using usednames, s_a, s_a', s_R, g, Ps, infer premise,
   *    yielding s_a'', s_C, G, qs.
   * 4) Determine s_C_L : U -> W and s_C' by outername restriction
   *    using W such that s_C' * s_C_L = s_C
   * 5) Construct and return s_a'', s_C', (id * s_C_L)(U)G, and qs
   *)  
  fun matchDP usednames s_a' s_a s_R p Ps = lzmake (fn () =>
		let
			val {idxlocsub, N = n} = unmkP p
			val {absnames = Z, G = g} = unmkN n
			val (s_a_L, W) =
			  case BgVal.match (PAbs (PCom (PTen [PWir, PPer],
			                                PCon))) idxlocsub of
			    MAbs (W, MCom (MTen [MWir s_a_L, MPer id_1], MCon Z))
			  => (s_a_L, W)
			  | wrongterm
			     => raise BgBDNF.MalformedBDNF
			         ("match/match.sml", info idxlocsub, wrongterm,
			          "matching idxlocsub in matchDP")
			val s_a = Wiring.* (s_a_L, s_a)
			val usednames = NameSet.union W usednames
			fun toDP {s_a'', s_C, G, qs} =
			  let
			  	val {inCod = s_C_L, notInCod = s_C'}
			  	  = Wiring.split_outer s_C W
			  	val U = Wiring.innernames s_C_L
			  	val P = make_P s_C_L (make_N U G)
			  in
			  	{s_a'' = s_a'', s_C = s_C', E = P, qs = qs}
			  end
			val matches = lzmap toDP (matchDG usednames s_a' s_a s_R g Ps)
		in
			lzunmk matches
		end)

  (* fresh returns a name similar to basename, not in usednames. *)
  local
    fun fresh' i basename usednames =
			let
			  val name = basename ^ Int.toString i
			  val x = Name.make name
			in
			  if NameSet.member x usednames then
			    fresh' (i + 1) basename usednames
			  else
			    x
			end
  in
	  fun freshname basename = fresh' 0 (basename ^ "_")
  end

  (* Match a parallel composition:
   * 1) For each e_i : -> <1,(X_i),X_i + Y_i> determine
   *    s_a_i = s_a domainrestricted to Y_i
   *    s_a'_i = s_a' domainrestricted to Y_i.
   * 2) For each Ps_i : -> <n,\vec X_i,X_i + Y_i> determine
   *    s_R_i = s_R domainrestricted to Y_i.
   * 3) Using usednames, s_a_i, s_a'_i, s_R_i, e_i, Ps_i, infer premise, 
   *    yielding s_a''_i, s_C_i, E_i and ps_i.
   * 4) Compute s_a'', Es and pss by product.
   * 5) Determine names Y_a'', Y_a, Y_R
   *    introduced by s_a'', s_a and s_R, respectively.
   * 6) If Y_a'' = Y_a = {} and Y_R <> {}, then add a fresh
   *    name to Y_a'' and its name introduction to s_a''.
   * 7) Construct substitution s_I : Y_R -> Y_a'' + Y_a.
   * 8) Compute s_C as the extension* of s_C_i's and s_I.
   * 9) Return s_a'', s_C, Es and pss.
   *
   * *=extension is a parallel product where inner name
   *   clash is allowed, providing such names map to the
   *   same link.  Internal edges are all "identical", causing
   *   edges in two factors to merge if they have a point
   *   in common.
   *)
  fun matchPARn E usednames s_a' s_a s_R es Pss = lzmake (fn () =>
    let
      val s_a's
        = map (Wiring.restrict s_a' o glob o outerface) es
      val s_as
        = map (Wiring.restrict s_a o glob o outerface) es
      val s_Rs
        = map (Wiring.restrict s_R
               o foldr (fn (Y_i, Y) => NameSet.union Y_i Y)
                       NameSet.empty
               o map (glob o outerface)) Pss
      val match = case E of 
          DiscretePrime => matchDP
        | GlobalDiscretePrime => matchDS
      val y = freshname "PARnE" usednames
      val usednames = NameSet.insert y usednames
      fun submatches
          (s_a' :: s_a's) (s_a :: s_as) (s_R :: s_Rs)
          (e :: es) (Ps :: Pss)
        = match usednames s_a' s_a s_R e Ps
           :: submatches s_a's s_as s_Rs es Pss
        | submatches [] [] [] [] [] = []
        | submatches s_a's s_as s_Rs es Pss
        = raise UnequalLengths (s_a's, s_as, s_Rs, length es, Pss)
      val Y_a = Wiring.introductions s_a
      val Y_R = Wiring.introductions s_R
      val Y_a_empty_and_Y_R_nonempty
        = NameSet.isEmpty Y_a andalso not (NameSet.isEmpty Y_R)
      fun toPARn (matches : {E : 'a bgbdnf, qs : P bgbdnf list,
                             s_C : wiring, s_a'' : wiring} list) =
        let
          val s_a'' = Wiring.||| (map #s_a'' matches)
          val Es = map #E matches
          val qss = map #qs matches
          val Y_a'' = Wiring.introductions s_a''
          val Y_a''
            = if Y_a_empty_and_Y_R_nonempty
              andalso NameSet.isEmpty Y_a'' then
                NameSet.singleton y
              else
                Y_a''
          val theoutername
            = case NameSet.foldUntil
                (fn y => fn NONE => (true, SOME y) | y' => (true, y'))
                (NameSet.foldUntil 
                  (fn y => fn NONE => (true, SOME y) | y' => (true, y'))
                  NONE Y_a'')
                Y_a of
                 SOME y => y
               | NONE => raise ThisCannotHappen (* as Y_a'' + Y_a <> {} *)
          val s_I
           = Wiring.*
             (Wiring.make
                (LinkSet.singleton
                  (Link.make {outer = SOME theoutername, inner = Y_R})),
              Wiring.introduce
                (NameSet.remove
                   theoutername (NameSet.union Y_a'' Y_a)))
          val s_C = Wiring.++ (s_I :: map #s_C matches)
        in
          {s_a'' = s_a'', s_C = s_C, Es = Es, qss = qss}
        end
      val matches
        = lzmap toPARn
            (lzcombine (submatches s_a's s_as s_Rs es Pss))
    in
      lzunmk matches
    end)

  fun matchPARe E usednames s_a' s_a s_R es Ps = lzmake (fn () =>
	  let
	    val n = length es
	    val m = length Ps
	    fun group [] [] = []
	      | group Ps (n :: ns)
	        = (List.take (Ps, n) :: group (List.drop (Ps, n)) ns
	           handle Subscript
	           => raise GroupError ("kernel/match/match.sml", 
	                               "matchPARe", Ps, n :: ns))
	      | group Ps []
	        = raise GroupError ("kernel/match/match.sml", 
	                               "matchPARe", Ps, [])
	    fun nextmatch split =
	      let
	        val P'ss = group Ps split
	        fun toPARe {s_a'', s_C, Es, qss}
	          = {s_a'' = s_a'', s_C = s_C, Es = Es,
	             qs = List.concat qss}
    	    val matches
	          = lzmap toPARe (matchPARn E usednames s_a' s_a s_R es P'ss)
          fun next () = lzunmk (nextmatch
                                (nextsplit split)
                                handle NoMoreSplits => lzNil)
	      in
	        lzappend matches (lzmake next)
	      end
	  in
	    lzunmk (nextmatch (firstsplit m n))
	  end)

  fun matchPER E usednames s_a' s_a s_R es Qs = lzmake (fn () =>
    let
      val Xss = map (loc o innerface) Qs
      fun nextmatch (perm as (_, pi, _) : Mutable operm) =
        let
          val Qs' = permute pi Qs
          val pibar = pushthru pi Xss
          fun toPER {s_a'', s_C, Es, qs}
            = {s_a'' = s_a'', s_C = s_C, Es = Es, pi = pi,
               qs = permute pibar qs}
          val matches
            = lzmap toPER (matchPARe E usednames s_a' s_a s_R es Qs')
          fun next () = lzunmk (nextmatch 
                                (nextperm perm)
                                handle NoMorePerms => lzNil)
        in
          lzappend matches (lzmake next)
        end
    in
      lzunmk (nextmatch (firstperm (map (hd o loc o outerface) Qs)))
    end)

  (* Match a closure:
   * 1) Open w_a, yielding s_a' * s_a
   * 2) Open w_R, yielding s_R and fresh outer names Y_R of s_R
   * 3) Compute usednames as the outer names of s_a' * s_a plus Y_R
   * 4) Using usednames, s_a', s_a, s_R, infer premise,
   *    yielding s_a'', s_C, Qs, pi, qs,
   *    where s_a'' = Y/ * alpha s_a' has outer names Y_R + Y_C
   * 5) Check that s_C = id_{Y_R} * s'_C
   * 6) Return a new w_C as s'_C where links Y_C are closed.
   *)
  fun matchCLO (w_a : Wiring.wiring) w_R ps Ps = lzmake (fn () =>
    let
      open Wiring
    	val {opened = s_a', rest = s_a, usednames} = splitopen NameSet.empty w_a
    	val {opened = s_R, newnames = Y_R, usednames} = openup usednames w_R
    	val usednames = NameSet.union (Wiring.outernames s_a) usednames
	    val matches = matchPER DiscretePrime usednames s_a' s_a s_R ps Ps
	    val is_id_Y_R_x_sigma = Wiring.is_id_x_sigma Y_R
    	fun toCLO ({s_a'', s_C, Es = Qs, pi, qs}, rest) =
    	  if is_id_Y_R_x_sigma s_C then
     	    let
    	      val Y_C = NameSet.difference (outernames s_a'') Y_R
    	      val w_C = closelinks Y_C s_C
          in
            lzCons
              (fn () =>
                ({w_C = w_C, Qs = Qs, pi = pi, qs = qs}, rest ()))
          end
        else
          rest ()
    in
      lzunmk (lzfoldr toCLO lzNil matches)
    end)
    
  fun matches {agent, redex} = lzmake (fn () =>
    let
      val {wirxid = w_axid, D = D_a} = unmkBR agent
      val ps = #Ps (unmkDR D_a)
      val {wirxid = w_Rxid, D = D_R} = unmkBR redex
      val Ps = #Ps (unmkDR D_R)
      fun toMatch {w_C, Qs, pi, qs} =
        let
          val Xs = map (hd o loc o outerface) Qs
        in
          {context
            = BgBDNF.make_B w_C Xs (BgBDNF.make_D Wiring.id_0 Qs pi),
           redex = redex,
           parameter = BgBDNF.make_DR Wiring.id_0 qs}
        end
    in
      case bgvalmatch (PTen [PWir, PVar]) w_axid of
      MTen [MWir w_a, _] =>
        (case bgvalmatch (PTen [PWir, PVar]) w_Rxid of
         MTen [MWir w_R, _] => lzunmk (lzmap toMatch (matchCLO w_a w_R ps Ps))
         | wrongterm => 
            raise MalformedBDNF
                    ("match.sml", BgVal.info w_Rxid, wrongterm,
                     "matching w_Rxid in matches"))
      | wrongterm => 
         raise MalformedBDNF
                 ("match.sml", BgVal.info w_axid, wrongterm,
                  "matching w_axid in matches")
    end)
    
    (*********************************)
    (*                               *)
    (* MAIN MATCHING ALGORITHM HERE! *)
    (*                               *)
    (*********************************)

  fun amatch agentredex =
    case lzunmk (matches agentredex) of
      Nil => NONE
    | Cons (m, ms) => SOME m

  val allmatches : {agent : BR bgbdnf, redex : BR bgbdnf }
                 -> match list = lztolist o matches
  
  fun pp indent pps ({context, redex, parameter} : match) =
    let
      open PrettyPrint
      val show = add_string pps
      fun << () = begin_block pps CONSISTENT indent
      fun >> () = end_block pps
      fun brk () = add_break pps (1, 0)
      val Z = Interface.glob (BgBDNF.outerface parameter)
      val id_Z = Wiring.id_X Z
    in
      <<();
      show "("; BgBDNF.pp indent pps context; show ")";
      brk();
      show "o ";
      <<();
      show "("; Wiring.pp indent pps id_Z;
      brk();
      show "* "; BgBDNF.pp indent pps redex; show ")";
      >>();
      brk();
      show "o ("; BgBDNF.pp indent pps parameter; show ")";
      >>()
    end
end
