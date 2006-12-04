
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
  structure NameMap     : MONO_FINMAP
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
  sharing type NameMap.dom = Name.name
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
  val makeP = BgBDNF.makeP
  val makeN = BgBDNF.makeN
  val makeG = BgBDNF.makeG
  val makeS = BgBDNF.makeS
  val SMol = BgBDNF.SMol
  val SCon = BgBDNF.SCon
  val makeM = BgBDNF.makeM
  val unmkBR = BgBDNF.unmkBR
  val unmkDR = BgBDNF.unmkDR
  val unmkP = BgBDNF.unmkP
  val unmkP' = BgBDNF.unmkP'
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

  exception NOT_FOUND
  structure NameHashMap 
    = HashTableFn (type hash_key = name
                   val hashVal = Name.hash
       val sameKey = Name.==);
  fun createNameHashMap size = NameHashMap.mkTable (size, NOT_FOUND)

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
   * - ename      finite map, renaming some of s_a_e's outer names
   * - s_a_e      substitution representing agent edge links
   * - s_a_n      substitution representing agent name links
   * - s_R_e      substitution representing redex edge links
   * - s_R_n      substitution representing redex name links
   *
   * They must return specific values, as well as
   * - ename'     extension of ename, renaming some of s_a_e's outer names
   * - Y          set of names to add as introductions to s_a_e
   * - s_C        substitution representing context links
   *
   * Links of s_R_e must only be matched with links of s_a_e.
   * The substitutions in the theoretical note are
   * sigma^a = Y/ * ename' s_a_e * s_a_n  and  sigma^R = s_R_e * s_R_n.
   *
   * Primed functions:
   * ----------------
   * All rule functions used in prime mode are used with s_R = id_0
   * and redex = id, which are therefore not passed as arguments.
   * They take some
   * specific arguments, as well as these general ones:
   * - ename      finite map, renaming some of s_a_e's outer names
   * - s_a_e      substitution representing agent edge links
   * - s_a_n      substitution representing agent name links
   * - s_C_e      substitution representing (part of) context edge links
   * - s_C_n      substitution representing (part of) context name links
   *
   * They must return specific values, as well as
   * - ename'     extension of ename, renaming some of s_a_e's outer names
   * - s_C'       substitution representing (part of) context edge links
   *
   * Letting sigma^C represent the context substitution of the
   * theoretical note, s_C' must fulfill
   * sigma^C = s_C' (id * s_C_n) * s_C_e and
   * links of s_C_e must only be matched with links of s_a_e.
   *)

  (* Signal that no match of links between s_a and s_C exists. *)
  exception NoMatch
  (* Match up links of points for two substitutions pairwise.
   * The links of a list of points [y0, ..., yn] of s_a must be matched with
   * those of corresponding points [u0, ..., un] of s_C.
   * Matched link sets of s_a_e are moved to s_a_n, and copied to s_a_e'', and
   * matched link sets of s_C_e are moved to s_C_n, and copied to s_C_e''.
   * Updated copies of s_a, s_a'', s_C, s_C'' are returned.
   * If no match exists, a NoMatch exception is raised.
   * 1) Make copies of s_a_e, s_a_n, s_C_e, s_C_n, as the following
   *    operations are destructive.
   * 2) For each (yi, ui),
   *    if yi is a point of s_a_n then
   *      if ui is a point of s_C_n then
   *        if s_a_n(yi) <> s_C_n(ui) then return NO MATCH
   *      else
   *        move linkset s_C_e(ui) to s_C_n, renaming it to s_a_n(yi),
   *        and copy it to s_C_e''
   *    else
   *      if ui is a point of s_C_e then
   *        move linkset s_C_e(ui) to s_C_n, 
   *        and copy it to s_C_e''
   *      move linkset s_a_e(yi) to s_a_n, renaming it to s_C_n(ui),
   *      and copy it to s_a_e''
   *    remove yi from s_a_n and ui from s_C_n.
  *)
  fun matchpoints {ys, us, s_a as {s_a_e, s_a_n}, s_C as {s_C_e, s_C_n}} =
    (print "match/match.sml: matchpoints not implemented yet!\n";
     raise NoMatch;
     {s_a = s_a, s_C = s_C, s_a_e'' = s_a_e, s_C_e'' = s_C_e})               

  (* Match a global discrete prime g to a context G using the PAX rule:
   * 1) Deconstruct context G = "alpha" : (X) -> Y.
   * 2) Compute X, Y, and Z = dom(g) \ X.
   * 3) Let ename' = ename, s_C' = {}.
   * 4) For each x in Y + Z, 
   *    4a) If x in Y then x' = alpha^-1(x) else x' = x.
   *    4b) If x in dom(s_C_e) then
   *          check/adjust ename' so that ename' s_a_e(x') = s_C_e(x)
   *        else
   *          if x in dom(s_C_n) then x'' = s_C_n(x) else x'' = x
   *          if x in dom(s_a_e) then
   *            check/adjust ename' and s_C' so that
   *              ename' s_a_e(x') = s_C'(x'')
   *          else
   *            check/adjust s_C' so that s_a_n(x') = s_C'(x'').
   * 5) Return ename', s_C', qs = (X)g.
   *)
  fun matchPAX' {ename, s_a, s_C, g, G} = lzNil
      lzmake (fn () =>
      case #Ss (unmkG G) of
        [S] =>
      (case unmkS S of
        SCon (i, a) =>
        let
          val X = Wiring.innernames a
          val Y = Wiring.outernames a
          val Z = NameSet.difference (Interface.glob (outerface g)) X
          (*FIXME finish*)
        in lsNil
(*FIXME
          {ename' = ename,
           s_C' = ,
           qs = [makeP (Wiring.id_X X) (makeN X g)]}*)
        end
      | _ => lsNil)
      | _ => lsNil)
  
  and matchMER' {ename, s_a, s_C, g, G} = lzNil

  (* Match a global discrete prime to a context using the ION rule:
   * 1) Deconstruct agent g = id * K_y(X) and context G = i * L_u(Z).
   * 2) If ctrl(K) <> ctrl (L) return NO MATCH.
   * 3) Let ename'' = ename.
   * 4) For each (yi, ui),
   *      if ui in dom(s_C_e) then
   *        check/update ename'' so that ename'' s_a_e(yi) = s_C_e(ui).
   * 5) Construct (v)/(X) and (v)/(Z) for some fresh v.
   * 6) Using ename'', s_a, s_C, etc., infer premise,
   *    yielding ename', s_C', qs.
   * 7) For each (yi, ui),
   *      if ui notin dom(s_C_e) then
   *        if ui in dom(s_C_n) then u' = s_C_n(u) else u' = ui
   *        if yi in dom(s_a_e) then
   *          check/update ename' and s_C' so that
   *            ename' s_a_e(yi) = s_C'(u')
   *        else
   *          check/update s_C' so that s_a_n(yi) = s_C'(u').
   * 8) Return ename', s_C', qs.
   *)
  and matchION' {ename, s_a, s_C, g, G} = lzNil

  (* Match a global discrete prime using a PAX, MER or ION rule:
   * 1) First return any PAX rule matches,
   * 2) Then return any ION rule matches,
   * 3) Then if the agent contains n > 1 top-level molecules
   *    (to avoid infinite recursion via MER-PAR-PARe-PARn),
   *    return any MER rule matches.
   *)
  and matchDG' (args as {ename, s_a, s_C, g, G})
    = lzappend (matchPAX' args)
        (lzappend (matchION' args)
          (lzmake (fn () =>
            case #Ss (unmkG g) of
              (_ :: _ :: _) => lzunmk (matchMER' args)
            | _ => LazyList.Nil)))

  (* Match a prime to a context using the ABS rule:
   * 1) Deconstruct agent p = (id * ^s_a_L)(Z)g and
   *    context P = (id * ^s_C_L)(U)G.
   * 2) Compute s_a_n' = s_a_n * s_a_L
   *    and s_C_n' = s_C_n * s_C_L.
   * 3) Using ename, s_a = {s_a_e, s_a_n'}, s_C = {s_C_e, s_C_n'},
   *    infer premise, yielding ename', s_C', qs.
   * 4) Let W = outernames(s_C_L).
   * 5) Restrict s_C' by removing inner points that are in W.
   * 6) Return ename', s_C', qs.
   *)
  and matchABS' {ename, s_a, s_C, p, P} = lzNil
  
  (* Match a parallel composition to a context using the PARn rule:
   * 1) Let ename_0 = ename.
   * 2) Using ename_i, s_a, s_C, ei, Ei, infer premise i, yielding
   *    ename_i+1, s_C'i, qs_i.
   * 3) Compute s_C' by merging s_C'is using extension,
   *      returning NOMATCH if impossible.
   * 4) Return ename' = ename_n, s_C', qss = [qs_0, ..., qs_n-1]
   *)
  and matchPARn' {ename, s_a, s_C, es, Es} = lzNil
  
  (* Match a tensor product to a context using the PARe rule:
   * 1) Infer premise, yielding ename', s_C', qss.
   * 2) Let qs = concat qss.
   * 3) Return ename', s_C', qs.
   *)
  and matchPARe' {ename, s_a, s_C, es, Es} = lzNil
  
  (* Match a tensor product to a context permutation:
   * 1) Infer premise, yielding ename', s_C', qs.
   * 2) Permute qs by pi, yielding qs' so that qs'_i = qs_pi(i).
   * 3) Return ename', s_C, qs'.
   *)
  and matchPER' {ename, s_a, s_C, es, Es, pi} = lzNil
  
  (* Match a global discrete prime using the SWX rule:
   * If Ps = [P], where P = (id_Z * ^s)(W)G, then
   * 1) Let s_C_e = s_R_e, s_C_n = s * s_R_n, and infer premise
   *    using s_a_e, s_a_n, s_C_e, s_C_n, g, G,
   *    yielding s_a_e', s_C, and qs
   * 2) Let Y_C_e = outernames s_C_e
   * 3) Let U = outernames s
   * 4) Return s_a_e', s_C := s_C * id_Y_C_e, G := "U", qs
   *)
  fun matchSWX {ename, 
                s_a = {s_a_e, s_a_n}, 
                s_R = {s_R_e, s_R_n}, e = g, Ps = [P]}
    = lzmake (fn () =>
      let
        val {id_Z, Y = U, s, X = W, N} = unmkP P
        val {absnames = W, G} = unmkN N
        val s_C_e = s_R_e
        val s_C_n = Wiring.* (s, s_R_n)
        val id_Y_C_e = Wiring.id_X (Wiring.outernames s_C_e)
        val i = BgBDNF.info P
        fun toSWX {ename', s_C', qs} =
          {ename' = ename', s_C = Wiring.* (s_C', id_Y_C_e),
           G = makeG [makeS (SCon (i, Wiring.id_X U))], qs = qs}
        val matches
          = lzmap toSWX
                  (matchDG' {ename = ename,
                             s_a = {s_a_e = s_a_e,
                                    s_a_n = s_a_n},
                             s_C = {s_C_e = s_C_e, s_C_n = s_C_n},
                             g = g, G = G})
      in
        lzunmk matches
      end)
    | matchSWX _ = lzNil
  
  (* Match a global discrete prime using the PAX rule:
   * If s_R_e = s_R_n = id_0 and
   * Ps = id_(X) for some X subset of g's outer names, then
   * return s_a_e' = s_a_e, s_C = s_a_n * s_a_e, G = "X", Ps = (X)g
   *)
  fun matchPAX {ename,
                s_a = {s_a_e, s_a_n},
                s_R = {s_R_e, s_R_n}, e = g, Ps = [P]}
    = lzmake (fn () =>
      if Wiring.is_id0 s_R_e andalso Wiring.is_id0 s_R_n then
        let
          val {s, N, id_Z, Y = X, ...} = unmkP P
          val {G, ...} = unmkN N
        in
          case unmkG G of
            {Ss = [S], ...}
          => (case unmkS S of
                BgBDNF.SCon (i, a)
              => if Wiring.is_id0 id_Z then
                  let
                    val alpha = Wiring.o (s, a)
                   in
                     if Wiring.is_id0 alpha then
                       let
                         val XplusZ = Interface.glob (outerface g)
                       in
                        LazyList.Cons 
                          ({ename' = ename (* REVISE! *),
                           s_C = Wiring.* (s_a_n, s_a_e),
                           G = makeG [makeS (SCon (i, alpha))],
                           qs = [makeP (Wiring.id_X XplusZ)
                                       (makeN X g)]},
                           lzNil)
                      end
                     else
                       LazyList.Nil
                   end
                 else
                   LazyList.Nil
              | _ => LazyList.Nil)            
          | _ => LazyList.Nil
        end
      else
        LazyList.Nil)
    | matchPAX _ = lzNil

  (* makefreshlinks returns a list of links vs/Xs, where vs are
   * fresh names.
   *)
  fun makefreshlinks Xs =
      let
        fun addfreshname X =
            Link.make {outer = SOME (Name.fresh NONE), inner = X}
      in
        map addfreshname Xs
      end

  (* sortlinksby returns vZs sorted using the vs as key, ordering
   * them as in vXs.
   *)
  fun sortlinksby vXs vZs =
    let
      val vZs_ht = createNameHashMap (LinkSet.size vZs)
      fun addlink vZ
        = case Link.outername vZ of
          SOME v => NameHashMap.insert vZs_ht (v, vZ)
        | NONE => raise NotName ("match/match.sml", vZ, vXs, vZs,
                                 "sortlinksby:addlink")
      fun findZ vX =
        case Link.outername vX of
          SOME v =>
            (case NameHashMap.find vZs_ht v of
              SOME vZ => Link.innernames vZ
            | NONE
            => raise Unmatchedv ("match/match.sml", vXs, vZs,
                                 "sortlinksby"))
        | NONE => raise NotName ("match/match.sml", vX, vXs, vZs,
                                 "sortlinksby:findvZ")
    in
      LinkSet.apply addlink vZs; map findZ vXs
    end

  (* Match a global discrete prime using the MER rule:
   * 1) ...
   *)
  fun matchMER {ename, s_a, s_R, e, Ps} = lzNil
  
  (* Match a global discrete prime using the ION rule, if possible:
   * If it contains 1 top-level molecule, match an ion:
   * 1) For agent ion K_yX, compute the set Y = {vec y}
   * 2) Compute s_Y_n || s_a_n_new = s_a_n by domain-restricting
   *     s_a_n to Y
   * 3) Compute s_Y_e || s_a_e_new = s_a_e by domain-restricting
   *     s_a_e to Y
   * 4) Construct p = (id * (vec v)/(vec X))n and infer premise
   *     using s_a_e_new, s_a_n_new, s_R, p, and Ps, yielding
   *     s_a_e'_new, s_C_new, P = (id * (vec v)/(vec Z))N, and qs
   * 5) Construct s_C = s_Y_n || s_Y_e || s_C_new
   *     and    s_a_e' = s_Y_e || s_a_e'_new
   *     and         G = (id * K_yZ)N
   * 6) Return s_a_e', s_C, G, qs
   *)
  and matchION (args as {ename, 
                         s_a = {s_a_e, s_a_n}, s_R, e = g, Ps})
    = lzmake (fn () =>
    case unmkG g of  {Ss = [s], ...} =>
      (case unmkS s of BgBDNF.SMol m =>
        let
          val {id_Z, KyX, N = n} = unmkM m
          val {ctrl, free = ys, bound = Xs} = Ion.unmk KyX
          val Y = foldr (fn (y, Y) => NameSet.insert y Y) NameSet.empty ys
          val {inDom = s_Y_n, notInDom = s_a_n_new}
            = Wiring.split s_a_n Y
          val {inDom = s_Y_e, notInDom = s_a_e_new}
            = Wiring.split s_a_e Y
          val s_Y = Wiring.|| (s_Y_n, s_Y_e)
          val vXs = makefreshlinks Xs
          val p = makeP (Wiring.make' vXs) n
          fun toION {ename', s_C, E = P, qs} =
            let
              val {s = vZ, N, ...} = unmkP P
               val vZs =  Wiring.unmk vZ
                val Zs = sortlinksby vXs vZs
                val s_C = Wiring.|| (s_Y, s_C)
                val s_a_e' = Wiring.id_0 (* REVISE! *)
                val KyZ = Ion.make {ctrl = ctrl, free = ys, bound = Zs}
                val G = makeG [makeS (SMol (makeM KyZ N))]
              in
                {ename' = ename' (* REVISE! *), s_C = s_C, G = G, qs = qs}
            end
          val matches
            = lzmap toION (matchABS {
(*REVISE THIS LINE: *)               ename = ename,
                                   s_a = {s_a_e = s_a_e_new,
                                          s_a_n = s_a_n_new},
                                   s_R = s_R,
                                   e = p,
                                   Ps = Ps})
        in
          lzunmk matches
        end
       | _ => raise AgentNotGround ("kernel/match/match.sml", g,
                                     "in matchDS"))
      | _ => LazyList.Nil)
  
  (* Match a global discrete prime using a SWX, PAX, MER or ION rule:
   * 1) First return any PAX rule matches,
   * 2) Then return any SWX rule matches,
   * 3) Then return any ION rule matches,
   * 4) Then if the agent contains n > 1 top-level molecules
   *    (to avoid infinite recursion via MER-PAR-PARe-PARn),
   *    return any MER rule matches.
   *)
  and matchDG (args as {ename, s_a, s_R, e = g, Ps})
    = lzappend (matchPAX args)
        (lzappend (matchSWX args)
          (lzappend (matchION args)
            (lzmake (fn () =>
              case #Ss (unmkG g) of
                (_ :: _ :: _) => lzunmk (matchMER args)
              | _ => LazyList.Nil))))

  (* Match an abstraction:
   * 1) Deconstruct p, yielding s_a_L : Z -> W and g.
   * 2) Compute s_a_n_new = s_a_L * s_a_n.
   * 3) Using s_a_n_new, s_a_e, s_R, g, Ps, infer premise,
   *    yielding s_a_e', s_C, G, qs.
   * 4) Determine s_C_L : U -> W and s_C' by outername restriction
   *    using W such that s_C' * s_C_L = s_C
   * 5) Construct and return s_a_e', s_C', (id * s_C_L)(U)G, and qs
   *)  
  and matchABS {ename,
                s_a = {s_a_e, s_a_n}, s_R as {s_R_e, s_R_n}, e = p, Ps}
    = lzmake (fn () =>
    let
      val {Y = W, s = s_a_L, N = n, ...} = unmkP p
      val {absnames = Z, G = g} = unmkN n
      val s_a_n_new = Wiring.* (s_a_L, s_a_n)
      fun toABS {ename', s_C, G, qs} =
        let
          val {inCod = s_C_L, notInCod = s_C'}
            = Wiring.split_outer s_C W
          val U = Wiring.innernames s_C_L
          val P = makeP s_C_L (makeN U G)
        in
          {ename' = ename', s_C = s_C', E = P, qs = qs}
        end
      val matches
        = lzmap toABS
           (matchDG {ename = ename,
                     s_a = {s_a_e = s_a_e, s_a_n = s_a_n_new},
                     s_R = s_R,
                     e = g,
                     Ps = Ps})
    in
      lzunmk matches;Nil  (* REVISE! *)
    end)

  (* Match a parallel composition:
   * 1) For each e_i : -> <1,(X_i),X_i + Y_i> determine
   *    s_a_n_i = s_a_n domainrestricted to Y_i
   *    s_a_e_i = s_a_e domainrestricted to Y_i.
   * 2) For each Ps_i : -> <n,\vec X_i,X_i + Y_i> determine
   *    s_R_n_i = s_R_n domainrestricted to Y_i.
   *    s_R_e_i = s_R_e domainrestricted to Y_i.
   * 3) Using s_a_n_i, s_a_e_i, s_R_n_i, s_R_e_i,
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
   
   NOTE: THIS FUNCTION NEEDS REVISING to handle the case (1)
   
    a = /e e/{e1, e2} (K[e1] * L[e2])
    R = /f f/{f1, f2} (K[f1] * L[f2])
   
   (which it does now) but not the case (2)
   
    a = /e e/{e1, e2}            (K[e1] * L[e2])
    R = /{f1,f2} (f1/f1 * f2/f2) (K[f1] * L[f2]).
      
   Currently, the problem is that e in case (2) is renamed to 
   f1 when recursing into K[f1] and to f2 when recursing into
   L[f2].  The name must be the same, so after recursing into
   K[f1], which renames e to f1, e (now f1) must not be renamed
   to anything else when recursing into L[f2].   
   
   *)
  and matchPARn
      {matchE, ename, 
       s_a = {s_a_e, s_a_n}, s_R as {s_R_e, s_R_n},
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
      fun zipup [] [] [] [] [] [] = []
        | zipup (se :: ses) (sn :: sns) (Re :: Res) (Rn :: Rns)
                (e :: es) (Ps :: Pss)
        = {s_a = {s_a_e = se, s_a_n = sn}, s_R = {s_R_e = Re, s_R_n = Rn},
           e = e, Ps = Ps} :: zipup ses sns Res Rns es Pss
        | zipup ses sns Res Rns es Pss
        = raise UnequalLengths (ses, sns, Res, Rns, length es, Pss) 
      val sRePss = zipup s_a_es s_a_ns s_R_es s_R_ns es Pss
      val y = Name.fresh NONE
      fun submatches mslz ({s_a = {s_a_e, s_a_n},
                            s_R = {s_R_e, s_R_n}, e, Ps} :: rest)
        = let
            fun tosubmatches (ms, ename') =
              let
                val mlz = matchE {ename = ename',
                        s_a = {s_a_e = s_a_e, s_a_n = s_a_n},
                        s_R = {s_R_e = s_R_e, s_R_n = s_R_n},
                        e = e,
                        Ps = Ps}
                fun addms {E, qs, s_C, ename'}
                  = ({E = E, qs = qs, s_C = s_C} :: ms, ename')
              in
                lzmap addms mlz
              end
          in
            submatches (lzconcat (lzmap tosubmatches mslz)) rest 
          end
        | submatches mslz [] = mslz
      val mslz
        = submatches (lzCons (fn () => (([], ename), lzNil))) sRePss
      
      val Y_a_n = Wiring.introductions s_a_n
      val Y_R_e = Wiring.introductions s_R_e
      val Y_R_n = Wiring.introductions s_R_n
      val Y_a_n_empty_and_Y_R_nonempty
        = NameSet.isEmpty Y_a_n andalso
           not (NameSet.isEmpty Y_R_e andalso NameSet.isEmpty Y_R_n)
      fun toPARn (matches : {E : 'a bgbdnf, qs : P bgbdnf list,
                             s_C : wiring} list, ename) =
        let
          val matches = rev matches
          val s_a_e' = Wiring.id_0 (* REVISE! *)
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
          {ename' = ename, Y = NameSet.empty (* REVISE! Y *), s_C = s_C, Es = Es, qss = qss}
        end
      val matches = lzmap toPARn mslz
    in
      lzunmk matches
    end)

  (* Match a tensor product as a product of tensor products:
   * 1) Compute lengths n and m of agent and redex products.
   * 2) For each split of m into n parts, group Ps accordingly.
   * 3) Infer premise using the grouped Ps, yielding parameter
   *    lists of lists qss, etc.
   * 4) Concatenate the resulting qss and return the result.
   *)
  and matchPARe {matchE, ename, s_a, s_R, es, Ps}
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
          fun toPARe {ename', Y, s_C, Es, qss}
            = {ename' = ename', Y = Y, s_C = s_C, Es = Es,
               qs = List.concat qss}
          val matches
            = lzmap toPARe
               (matchPARn {matchE = matchE,
                           ename = ename,
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

  (* Match a context permutation:
   * 1) Compute the local inner faces Xss of the redex primes Qs.
   * 2) For each permutation pi of the redex primes Qs',
   *    2a) Push pi through Qs, yielding pibar.
   *    2b) Infer premise, yielding parameter list qs etc.
   *    2c) Permute qs by pibar and return the result.
   *)
  and matchPER {matchE, ename, s_a, s_R, es, Qs}
    = lzmake (fn () =>
    let
      val Xss = map (loc o innerface) Qs
      fun nextmatch (perm as (_, pi, _) : Mutable operm) =
        let
          val Qs' = permute pi Qs
          val pibar = pushthru pi Xss
          fun toPER {ename', Y, s_C, Es, qs}
            = {ename' = ename', Y = Y, s_C = s_C, Es = Es, pi = pi,
               qs = permute pibar qs}
          val matches
            = lzmap toPER
               (matchPARe {matchE = matchE,
                           ename = ename,
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
      lzunmk (nextmatch (firstperm (map (hd o loc o outerface) Qs)));
      Nil (* REVISE! *)
    end)

  (* Match a closure:
   * 1) Open w_a, yielding s_a = s_a_e * s_a_n
   * 2) Open w_R, yielding s_R = s_R_e * s_R_n and fresh outer names Y_R of s_R_e
   * 3) Using s_a, s_R, infer premise,
   *    yielding ename', Y, s_C, Qs, pi, qs,
   *    where ename' maps to range Y_R + Y_C
   * 4) Check that s_C = id_{Y_R} * s'_C
   * 5) Return a new w_C as s'_C where links Y_C are closed.
   *)
  fun matchCLO {w_a : Wiring.wiring, w_R, ps, Ps} = lzmake (fn () =>
    let
      open Wiring
      val {opened = s_a_e, rest = s_a_n, ...}
        = splitopen w_a
      val {opened = s_R_e, rest = s_R_n, newnames = Y_R}
        = splitopen w_R
      val matches
        = matchPER {matchE = matchABS,
                    ename = NameMap.empty,
                    s_a = {s_a_e = s_a_e, s_a_n = s_a_n},
                    s_R = {s_R_e = s_R_e, s_R_n = s_R_n},
                    es = ps,
                    Qs = Ps}
      val is_id_Y_R_x_sigma = Wiring.is_id_x_sigma Y_R
      fun toCLO ({ename', Y, s_C, Es = Qs, pi, qs}, rest) =
        if is_id_Y_R_x_sigma s_C then
           let
             val Y_C = NameSet.difference 
                       (NameSet.fromList (NameMap.range ename')) Y_R
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
            = BgBDNF.makeB w_C Xs (BgBDNF.makeD Wiring.id_0 Qs pi),
           redex = redex,
           parameter = BgBDNF.makeDR Wiring.id_0 qs}
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
                    (BgVal.info w_Rxid, wrongterm,
                     "matching w_Rxid in matches"))
      | wrongterm => 
         raise MalformedBDNF
                 (BgVal.info w_axid, wrongterm,
                  "matching w_axid in matches")
    end)
    
    (*********************************)
    (*                               *)
    (* MAIN MATCHING ENTRY HERE!     *)
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
