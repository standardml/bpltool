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
  structure Ion         : ION
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
  sharing type Ion.ion = BgVal.ion = BgBDNF.ion
  sharing type NameSet.Set = Link.nameset
                           = Wiring.nameset
                           = Interface.nameset
                           = BgBDNF.nameset
                           = BgVal.nameset
                           = Ion.nameset
  sharing type Name.name = Ion.name
   											 = Link.name
                         = NameSet.elt
                         = BgVal.name
  sharing type LinkSet.Set = Wiring.linkset
  sharing type LinkSet.elt = Link.link
                           = Wiring.link
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
  type G        = BgBDNF.G
  type ppstream = PrettyPrint.ppstream
  type Mutable  = Permutation.Mutable
  type 'kind permutation = 'kind Permutation.permutation
  type nameset  = NameSet.Set
  type link     = Link.link
  type linkset  = LinkSet.Set
  open LazyList

  val bgvalmatch = BgVal.match
  val MalformedBDNF = BgBDNF.MalformedBDNF
  val make_P = BgBDNF.make_P
  val make_N = BgBDNF.make_N
  val make_G = BgBDNF.make_G
  val make_S = BgBDNF.make_S
  val SMol = BgBDNF.SMol
  val make_M = BgBDNF.make_M
  val unmkBR = BgBDNF.unmkBR
  val unmkDR = BgBDNF.unmkDR
  val unmkP = BgBDNF.unmkP
  val unmkN = BgBDNF.unmkN
  val unmkG = BgBDNF.unmkG
  val unmkS = BgBDNF.unmkS
  val unmkM = BgBDNF.unmkM
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

  exception AgentNotGround of string * G bgbdnf * string

  exception Unmatchedv of string * link list * linkset * string

  exception NotName of string * link * link list * linkset * string

   (* Apparently the following hash function is advocated by
   * Knuth - at the very least it actually works in Moscow ML.
   *)
  fun stringhash s = 
      let open Word
	  fun f (c,h) = 
	      xorb(xorb(<<(h,0w5),>>(h,0w27)), Word.fromInt (ord c));
      in  CharVector.foldr f 0w0 s
      end

  exception NOT_FOUND
  structure NameMap 
    = HashTableFn (type hash_key = name
                   val hashVal = stringhash o Name.unmk
		   val sameKey = Name.==);
  fun createNameMap size = NameMap.mkTable (size, NOT_FOUND)

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
   of wiring list * wiring list * wiring list * wiring list * int * P bgbdnf list list

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
   *)

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
  fun matchDG {usednames, s_a, s_R = {s_R_e, s_R_n}, e = g, Ps} = lzNil 

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

  (* makefreshlinks returns a list of links vs/Xs, where vs are
   * fresh names in descending alphabetical order.
   *)
  fun makefreshlinks usednames Xs =
    let
      fun freshname' i =	freshname ("v_" ^ Int.toString i)
    	fun addfreshname (X, (i, vXs, usednames)) =
    	  let
    	  	val v = freshname' i usednames
    	  in
    	    (i + 1,
    	     Link.make {outer = SOME v, inner = X} :: vXs,
    	     NameSet.insert v usednames)
    	  end
    in
      #2 (foldr addfreshname (0, [], usednames) Xs)
    end

  (* sortlinksby returns vZs sorted using the vs as key, ordering
   * them as in vXs.
   *)
  fun sortlinksby vXs vZs =
    let
      val vZs_ht = createNameMap (LinkSet.size vZs)
      fun addlink vZ
        = case Link.outername vZ of
          SOME v => NameMap.insert vZs_ht (v, vZ)
        | NONE => raise NotName ("match/match.sml", vZ, vXs, vZs,
                                 "sortlinksby:addlink")
      fun findZ vX =
        case Link.outername vX of
          SOME v =>
            (case NameMap.find vZs_ht v of
              SOME vZ => Link.innernames vZ
            | NONE
            => raise Unmatchedv ("match/match.sml", vXs, vZs,
                                 "sortlinksby"))
        | NONE => raise NotName ("match/match.sml", vX, vXs, vZs,
                                 "sortlinksby:findvZ")
    in
      LinkSet.apply addlink vZs; map findZ vXs
    end

  (* Match a global discrete prime using the ION rule, if possible:
   * 1) Determine how many (top-level) molecules it contains.
   * 2) If 1 molecule, match an ion:
   *    2a) For agent ion K_yX, compute the set Y = {vec y}
   *    2b) Compute s_Y_n || s_a_n_new = s_a_n by domain-restricting
   *        s_a_n to Y
   *    2c) Compute s_Y_e || s_a_e_new = s_a_e by domain-restricting
   *        s_a_e to Y
   *    2d) Construct p = (id * (vec v)/(vec X))n and infer premise
   *        using s_a_e_new, s_a_n_new, s_R, p, and Ps, yielding
   *        s_a_e'_new, s_C_new, P = (id * (vec v)/(vec Z))N, and qs
   *    2e) Construct s_C = s_Y_n || s_Y_e || s_C_new
   *        and    s_a_e' = s_Y_e || s_a_e'_new
   *        and         G = (id * K_yZ)N
   *    2f) Return s_a_e', s_C, G, qs
   * 3) Otherwise, perform a D_G match
   *)
  fun matchDS (args as {usednames, s_a = {s_a_e, s_a_n}, s_R, e = g, Ps})
    = lzmake (fn () =>
    case unmkG g of	{idxmerge, Ss = [s]} =>
    	(case unmkS s of BgBDNF.SMol m =>
    	  let
    	    val {idxion, N = n} = unmkM m
    	  in
    	    case match (PTen [PWir, PIon]) idxion of
    	      MTen [MWir id_Z, MIon KyX] =>
    	      let
    	        val {ctrl, free = ys, bound = Xs} = Ion.unmk KyX
    	        val Y = foldr (fn (y, Y) => NameSet.insert y Y) NameSet.empty ys
    	        val {inDom = s_Y_n, notInDom = s_a_n_new}
    	          = Wiring.split s_a_n Y
    	        val {inDom = s_Y_e, notInDom = s_a_e_new}
    	          = Wiring.split s_a_e Y
    	        val s_Y = Wiring.|| (s_Y_n, s_Y_e)
    	        val vXs = makefreshlinks usednames Xs
    	        val p = make_P (Wiring.make' vXs) n
    	        fun toDS {s_a_e', s_C, E = P, qs} =
    	          let
    	          	val {idxlocsub, N} = unmkP P
    	         		val vZs =
			    	       	case match
			    	       	  (PTen [PWir, PAbs (PCom (PTen [PWir, PPer], PCon))])
			    	       	  idxlocsub
			    	       	of
			    	       		MTen [MWir _,
			    	       		      MAbs (_, MCom (MTen [MWir vZ, MPer _], MCon _))]
			    	        => Wiring.unmk vZ
			    	        | wrongterm
			    	        => raise MalformedBDNF ("match/match.sml", info idxlocsub,
			    	        	wrongterm, "matching idxlocsub in matchDS")
			    	      val Zs = sortlinksby vXs vZs
			    	      val s_C = Wiring.|| (s_Y, s_C)
			    	      val s_a_e' = Wiring.|| (s_Y_e, s_a_e')
			    	      val KyZ = Ion.make {ctrl = ctrl, free = ys, bound = Zs}
			    	      val G = make_G [make_S (SMol (make_M KyZ N))]
			    	    in
			    	    	{s_a_e' = s_a_e', s_C = s_C, G = G, qs = qs}
    	          end
    	        val matches
    	          = lzmap toDS (matchDP {usednames = usednames,
    	                                 s_a = {s_a_e = s_a_e_new,
    	                                        s_a_n = s_a_n_new},
    	                                 s_R = s_R,
    	                                 e = p,
    	                                 Ps = Ps})
    	      in
    	      	lzunmk matches
    	      end
    		 end
    	 | _ => raise AgentNotGround ("kernel/match/match.sml", g,
    	 	                            "in matchDS"))
    | _ => lzunmk (matchDG args))
  
  (* Match an abstraction:
   * 1) Deconstruct p, yielding s_a_L : Z -> W and g.
   * 2) Compute s_a_n_new = s_a_L * s_a_n and add outer names of s_a_L to usednames.
   * 3) Using usednames, s_a_n_new, s_a_e, s_R, g, Ps, infer premise,
   *    yielding s_a_e', s_C, G, qs.
   * 4) Determine s_C_L : U -> W and s_C' by outername restriction
   *    using W such that s_C' * s_C_L = s_C
   * 5) Construct and return s_a_e', s_C', (id * s_C_L)(U)G, and qs
   *)  
  and matchDP {usednames, s_a = {s_a_e, s_a_n}, s_R as {s_R_e, s_R_n}, e = p, Ps}
    = lzmake (fn () =>
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
			val s_a_n_new = Wiring.* (s_a_L, s_a_n)
			val usednames = NameSet.union W usednames
			fun toDP {s_a_e', s_C, G, qs} =
			  let
			  	val {inCod = s_C_L, notInCod = s_C'}
			  	  = Wiring.split_outer s_C W
			  	val U = Wiring.innernames s_C_L
			  	val P = make_P s_C_L (make_N U G)
			  in
			  	{s_a_e' = s_a_e', s_C = s_C', E = P, qs = qs}
			  end
			val matches
			  = lzmap toDP
			     (matchDG {usednames = usednames,
			               s_a = {s_a_e = s_a_e, s_a_n = s_a_n_new},
			               s_R = s_R,
			               e = g,
			               Ps = Ps})
		in
			lzunmk matches
		end)

  (* Match a parallel composition:
   * 1) For each e_i : -> <1,(X_i),X_i + Y_i> determine
   *    s_a_n_i = s_a_n domainrestricted to Y_i
   *    s_a_e_i = s_a_e domainrestricted to Y_i.
   * 2) For each Ps_i : -> <n,\vec X_i,X_i + Y_i> determine
   *    s_R_n_i = s_R_n domainrestricted to Y_i.
   *    s_R_e_i = s_R_e domainrestricted to Y_i.
   * 3) Using usednames, s_a_n_i, s_a_e_i, s_R_n_i, s_R_e_i,
   *    e_i, Ps_i, infer premise, 
   *    yielding s_a_e'_i, s_C_i, E_i and ps_i.
   * 4) Compute s_a_e', Es and pss by product.
   * 5) Determine names Y_a_e', Y_a_n, Y_R_e, Y_R_n
   *    introduced by s_a_e', s_a_n, s_R_e, and s_R_n, respectively.
   * 6) If Y_a_e' = Y_a_n = {} and Y_R_e + Y_R_n <> {}, then add a
   *    fresh name to Y_a_e' and its name introduction to s_a_e'.
   * 7) Construct substitution s_I : Y_R_e + Y_R_n -> Y_a_e' + Y_a_n.
   * 8) Compute s_C as the extension* of s_C_i's and s_I.
   * 9) Return s_a_e', s_C, Es and pss.
   *
   * *=extension is a parallel product where inner name
   *   clash is allowed, providing such names map to the
   *   same link.  Internal edges are all "identical", causing
   *   edges in two factors to merge if they have a point
   *   in common.
   *)
  fun matchPARn
      {matchE, usednames, s_a = {s_a_e, s_a_n}, s_R as {s_R_e, s_R_n},
      es, Pss} = lzmake (fn () =>
    let
      val Ys = map (glob o outerface) es
      val s_a_es = map (Wiring.restrict s_a_e) Ys
      val s_a_ns = map (Wiring.restrict s_a_n) Ys
      val Ys'
        = map (foldr (fn (Y_i, Y) => NameSet.union Y_i Y)
                       NameSet.empty
               o map (glob o outerface)) Pss
      val s_R_es = map (Wiring.restrict s_R_e) Ys'
      val s_R_ns = map (Wiring.restrict s_R_n) Ys'
      val y = freshname "PARnE" usednames
      val usednames = NameSet.insert y usednames
      fun submatches
          (s_a_e :: s_a_es) (s_a_n :: s_a_ns)
          (s_R_e :: s_R_es) (s_R_n :: s_R_ns)
          (e :: es) (Ps :: Pss)
        = matchE {usednames = usednames,
                  s_a = {s_a_e = s_a_e, s_a_n = s_a_n},
                  s_R = s_R,
                  e = e,
                  Ps = Ps}
           :: submatches s_a_es s_a_ns s_R_es s_R_ns es Pss
        | submatches [] [] [] [] [] [] = []
        | submatches s_a_es s_a_ns s_R_es s_R_ns es Pss
        = raise UnequalLengths (s_a_es, s_a_ns, s_R_es, s_R_ns, length es, Pss)
      val Y_a_n = Wiring.introductions s_a_n
      val Y_R_e = Wiring.introductions s_R_e
      val Y_R_n = Wiring.introductions s_R_n
      val Y_a_n_empty_and_Y_R_nonempty
        = NameSet.isEmpty Y_a_n andalso
        	 not (NameSet.isEmpty Y_R_e andalso NameSet.isEmpty Y_R_n)
      fun toPARn (matches : {E : 'a bgbdnf, qs : P bgbdnf list,
                             s_C : wiring, s_a_e' : wiring} list) =
        let
          val s_a_e' = Wiring.||| (map #s_a_e' matches)
          val Es = map #E matches
          val qss = map #qs matches
          val Y_a_e' = Wiring.introductions s_a_e'
          val Y_a_e'
            = if Y_a_n_empty_and_Y_R_nonempty
              andalso NameSet.isEmpty Y_a_e' then
                NameSet.singleton y
              else
                Y_a_e'
          val theoutername
            = case NameSet.foldUntil
                (fn y => fn NONE => (true, SOME y) | y' => (true, y'))
                (NameSet.foldUntil 
                  (fn y => fn NONE => (true, SOME y) | y' => (true, y'))
                  NONE Y_a_e')
                Y_a_n of
                 SOME y => y
               | NONE => raise ThisCannotHappen (* as Y_a_e' + Y_a_n <> {} *)
          val s_I
           = Wiring.*
             (Wiring.make
                (LinkSet.singleton
                  (Link.make {outer = SOME theoutername,
                              inner = NameSet.union Y_R_e Y_R_n})),
              Wiring.introduce
                (NameSet.remove
                   theoutername (NameSet.union Y_a_e' Y_a_n)))
          val s_C = Wiring.++ (s_I :: map #s_C matches)
        in
          {s_a_e' = s_a_e', s_C = s_C, Es = Es, qss = qss}
        end
      val matches
        = lzmap toPARn
            (lzcombine (submatches s_a_es s_a_ns s_R_es s_R_ns es Pss))
    in
      lzunmk matches
    end)

  fun matchPARe {matchE, usednames, s_a, s_R, es, Ps}
    = lzmake (fn () =>
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
	        fun toPARe {s_a_e', s_C, Es, qss}
	          = {s_a_e' = s_a_e', s_C = s_C, Es = Es,
	             qs = List.concat qss}
    	    val matches
	          = lzmap toPARe
	             (matchPARn {matchE = matchE,
	                         usednames = usednames,
	                         s_a = s_a,
	                         s_R = s_R,
	                         es = es,
	                         Pss = P'ss})
          fun next () = lzunmk (nextmatch
                                (nextsplit split)
                                handle NoMoreSplits => lzNil)
	      in
	        lzappend matches (lzmake next)
	      end
	  in
	    lzunmk (nextmatch (firstsplit m n))
	  end)

  fun matchPER {matchE, usednames, s_a, s_R, es, Qs}
    = lzmake (fn () =>
    let
      val Xss = map (loc o innerface) Qs
      fun nextmatch (perm as (_, pi, _) : Mutable operm) =
        let
          val Qs' = permute pi Qs
          val pibar = pushthru pi Xss
          fun toPER {s_a_e', s_C, Es, qs}
            = {s_a_e' = s_a_e', s_C = s_C, Es = Es, pi = pi,
               qs = permute pibar qs}
          val matches
            = lzmap toPER
               (matchPARe {matchE = matchE,
                           usednames = usednames,
                           s_a = s_a,
                           s_R = s_R,
                           es = es,
                           Ps = Qs'})
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
   * 1) Open w_a, yielding s_a = s_a_e * s_a_n
   * 2) Open w_R, yielding s_R = s_a_e * s_a_n and fresh outer names Y_R of s_R_e
   * 3) Compute usednames as the outer names of s_a_e * s_a_n plus Y_R
   * 4) Using usednames, s_a, s_R, infer premise,
   *    yielding s_a_e', s_C, Qs, pi, qs,
   *    where s_a_e' = Y/ * alpha s_a_e has outer names Y_R + Y_C
   * 5) Check that s_C = id_{Y_R} * s'_C
   * 6) Return a new w_C as s'_C where links Y_C are closed.
   *)
  fun matchCLO {w_a : Wiring.wiring, w_R, ps, Ps} = lzmake (fn () =>
    let
      open Wiring
    	val {opened = s_a_e, rest = s_a_n, usednames, ...}
    	  = splitopen NameSet.empty w_a
    	val {opened = s_R_e, rest = s_R_n, newnames = Y_R, usednames}
    	  = splitopen usednames w_R
    	val usednames = NameSet.union (Wiring.outernames s_a_n) usednames
	    val matches
	      = matchPER {matchE = matchDP,
	                  usednames = usednames,
	                  s_a = {s_a_e = s_a_e, s_a_n = s_a_n},
	                  s_R = {s_R_e = s_R_e, s_R_n = s_R_n},
	                  es = ps,
	                  Qs = Ps}
	    val is_id_Y_R_x_sigma = Wiring.is_id_x_sigma Y_R
    	fun toCLO ({s_a_e', s_C, Es = Qs, pi, qs}, rest) =
    	  if is_id_Y_R_x_sigma s_C then
     	    let
    	      val Y_C = NameSet.difference (outernames s_a_e') Y_R
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
         MTen [MWir w_R, _]
          => lzunmk
              (lzmap toMatch
               (matchCLO {w_a = w_a, w_R = w_R, ps = ps, Ps = Ps}))
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
