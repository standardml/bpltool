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
 
functor Match'(
  structure Info        : INFO
  structure Name        : NAME
  structure NameMap     : MONO_FINMAP
  structure Link        : LINK
  structure LinkSet     : MONO_SET
  structure Control     : CONTROL
  structure Permutation : PERMUTATION
  structure Wiring      : WIRING
  structure Ion         : ION
  structure Interface   : INTERFACE
  structure BgVal       : BGVAL
  structure BgBDNF      : BGBDNF
  structure Rule        : RULE
  structure NameSet     : MONO_SET
  structure ErrorHandler : ERRORHANDLER
      where type origin = Origin.origin
        and type ppstream = PrettyPrint.ppstream
  sharing type Permutation.nameset = NameSet.Set
  sharing type Permutation.permutation = BgBDNF.permutation
  sharing type Wiring.wiring = BgVal.wiring
                             = BgBDNF.wiring
  sharing type Control.control = Ion.control
  sharing type Ion.ion = BgVal.ion = BgBDNF.ion
  sharing type NameSet.Set = Link.nameset
                           = Wiring.nameset
                           = Interface.nameset
                           = BgBDNF.nameset
                           = BgVal.nameset
                           = Ion.nameset
  sharing type Name.name = Ion.name
                         = Link.name
                         = Wiring.name
                         = NameSet.elt
                         = BgVal.name
  sharing type NameMap.dom = Name.name
  sharing type NameMap.map = Wiring.namemap
  sharing type LinkSet.Set = Wiring.linkset
  sharing type LinkSet.elt = Link.link
                           = Wiring.link
  sharing type Interface.interface = BgBDNF.interface
  sharing type BgVal.bgval = BgBDNF.bgval
  sharing type BgBDNF.bgbdnf = Rule.bgbdnf
  sharing type BgBDNF.BR = Rule.BR
  sharing type BgVal.bgmatch = BgBDNF.bgmatch
  sharing type Info.info =
               BgBDNF.info =
               BgVal.info
  ) : MATCH
  where type info            = Info.info
    and type 'class bgbdnf   = 'class BgBDNF.bgbdnf
    and type BR              = BgBDNF.BR
    and type DR              = BgBDNF.DR
    and type nameset         = NameSet.Set =
struct
  open Debug
  open ErrorHandler

  val file_origin = Origin.mk_file_origin
                      "$BPL/src/kernel/match/match.sml"
                      Origin.NOPOS
  fun mk_explainer errtitle (explainer : exn -> explanation list) e =
      Exp (LVL_USER, Origin.unknown_origin, mk_string_pp errtitle,
           explainer e)

  (* FIXME move to functor argument? *)
  structure Partition = Partition(structure LazyList = LazyList)
  structure OrderedPartition
    = OrderedPartition(structure LazyList = LazyList
                       structure Partition = Partition)
  structure NameSetSubset
    = Subset(structure LazyList = LazyList
             structure Set = NameSet)

  open BgVal
  type info = Info.info
  type 'class bgbdnf  = 'class BgBDNF.bgbdnf
  type B        = BgBDNF.B
  type BR       = BgBDNF.BR
  type DR       = BgBDNF.DR
  type P        = BgBDNF.P
  type G        = BgBDNF.G
  type Mutable  = Permutation.Mutable
  type 'kind permutation = 'kind Permutation.permutation
  type nameset  = NameSet.Set
  type link     = Link.link
  type linkset  = LinkSet.Set
  type rule = Rule.rule
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

  datatype derivation = PAX'
                | ZAX'
                | ION' of derivation
                | PARn' of derivation list
                | PARe' of derivation
                | PER' of derivation
                | MER' of derivation
                | ABS' of derivation
                | SWX of derivation
                | PAX
                | ZAX
                | ION of derivation
                | PARn of derivation list
                | PARe of derivation
                | PER of derivation
                | MER of derivation
                | ABS of derivation
                | CLO of derivation

  type match = {context : B bgbdnf,
                rule : rule,
                parameter : DR bgbdnf,
                tree : derivation}

  fun unmk' m = m
  
  fun unmk {context, rule, parameter, tree}
    = {context = context, rule = rule, parameter = parameter}
  
  (* Signals that there are no more new ways of splitting. *)
  exception NoMoreSplits
  
  exception ThisCannotHappen
  
  exception WrongTree of derivation

  exception AgentNotGround of bgval * string
  fun explain_AgentNotGround (AgentNotGround (v, rule)) =
      Exp (LVL_USER, Info.origin (BgVal.info v),
           fn i =>
           fn pps => BgVal.ppWithIface i pps (BgVal.simplify v), [])
      :: [Exp (LVL_LOW, file_origin, mk_string_pp rule, [])]
    | explain_AgentNotGround _ = raise Match
  val _ = add_explainer
            (mk_explainer "agent not ground" explain_AgentNotGround)

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

  type Immutable = Permutation.Immutable
  type Mutable = Permutation.Mutable
  val toperm = Permutation.toperm
  val firstperm = Permutation.firstperm
  val firstperm_n = Permutation.firstperm_n
  val nextperm = Permutation.nextperm
  exception NoMorePerms = Permutation.NoMorePerms

  (* Signals that an error during grouping of tensor factors. *)
  exception GroupError of string * string * P bgbdnf list * int list

  (* Signals that a tuple of lists contained lists of unequal length. *)
  exception UnequalLengths
   of wiring list * wiring list * nameset list
    * wiring list * wiring list * int * P bgbdnf list list


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
   * - L          subset of outer names of s_a_n
   * - s_R_e      substitution representing redex edge links
   * - s_R_n      substitution representing redex name links
   *
   * They must return specific values, as well as
   * - ename'     extension of ename, renaming some of s_a_e's outer names
   * - Y          set of names to add as introductions to s_a_e
   * - s_C        substitution representing context links
   *
   * Links of s_R_e must only be matched with links of s_a_e, and
   * s_C must not map any global parameter of redex names to an
   * element of L.
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
   * - Y          set of names to add as introductions to s_a_e
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

  (* Checks that e(x) = s(x') and -- if necessary -- adjusts e and s
   * to make it true. The (possibly) changed e and s are returned.
   * If  x \in dom(e)  and  x' \in dom(s)  and  e(x) <> s(x')
   * then NoMatch is raised.
   *)
  local
    fun lookup m n =
        case NameMap.lookup m n of
          SOME n' => n'
        | NONE => raise ThisCannotHappen
  in
    fun check_adjust e s x x' =
        (case (NameMap.inDomain x e, NameMap.inDomain x' s) of
           (true, true)
           => if (lookup e x) = (lookup s x') then
                (e, s)
              else
                raise NoMatch
         | (true, false)
           => (e, NameMap.add (x', lookup e x, s))
         | (false, true)
           => (NameMap.add (x, lookup s x', e), s)
         | (false, false)
           => let
                val fresh = Name.fresh NONE
              in
                (NameMap.add (x, fresh, e), NameMap.add (x', fresh, s))
              end)
  end
  (* Checks that e(x) = x' and -- if necessary -- adjusts e
   * to make it true. The (possibly) changed e is returned.
   * If  x \in dom(e)  and  e(x) <> x'  then NoMatch is raised.
   *)
  fun check_adjust' e x x' =
      #1 (check_adjust e (NameMap.fromList [(x',x')]) x x')


  (* Apply a partial renaming ename : X -> to a substitution
   * s_a_e : -> X \uplus Y
   * I.e. compute  s_a_e' = (ename * id_Y) s_a_e *)
  fun app_ename ename s_a_e =
      let
        fun link_rename l =
          case Link.unmk l of
            {outer = SOME y, inner} =>
              (case NameMap.lookup ename y of
                 SOME y' => Link.make {outer = SOME y', inner = inner}
               | NONE => l)
          | _ => l
      in
        Wiring.make (LinkSet.map link_rename (Wiring.unmk s_a_e))
      end

  (* lazily generate all permutations of a list
   * FIXME inefficient! *)
  fun all_perms []   = lzCons (fn () => ([], lzNil))
    | all_perms list =
      let
        (* We consecutively place each element of the list in
         * front and permute the remaining elements recursively.*)
        fun next_permutation _ []          = lzNil
          | next_permutation pre (e::post) = lzmake (fn () =>
            lzunmk
              (lzappend
                 (lzmap
                    (fn l => e::l)
                    (all_perms (foldl (op ::) post pre)))
                 (next_permutation (e::pre) post)))
      in
        next_permutation [] list
      end

  (* Utility function for converting a partition generator to a lazy list
   *)
  fun partgen2lzlist partgen =
(*      (lzCons (fn () => (Partition.next partgen, partgen2lzlist partgen)))
      handle Partition.NoPartitions => lzNil*)
      lzmake
        (fn () =>
         Cons (Partition.next partgen, partgen2lzlist partgen)
         handle Partition.NoPartitions => Nil)
      
  (* Utility function for converting an ordered partition generator to a lazy list
   *)
  fun opartgen2lzlist opartgen =
      lzmake
        (fn () =>
         Cons (OrderedPartition.next opartgen, opartgen2lzlist opartgen)
         handle OrderedPartition.NoPartitions => Nil)
  (* same as above, except each part of a partition has at least one element*)
  fun opartgen2lzlist' opartgen =
      lzmake
        (fn () =>
         Cons (OrderedPartition.next' opartgen, opartgen2lzlist' opartgen)
         handle OrderedPartition.NoPartitions => Nil)
  (* Utility function for converting a nameset subset generator to a lazy list
   *)
  fun nssubsetgen2lzlist subsetgen =
      lzmake
        (fn () =>
         Cons (NameSetSubset.next subsetgen, nssubsetgen2lzlist subsetgen)
         handle NameSetSubset.NoSubsets => Nil)
  (* same as above, except the list only has elements of at least size m *)
  fun nssubsetgen2lzlist' subsetgen m =
      lzmake
        (fn () =>
         Cons (NameSetSubset.next' subsetgen m, nssubsetgen2lzlist' subsetgen m)
         handle NameSetSubset.NoSubsets => Nil)

  (* utility functions to make substitution links *)
  fun mk_slink (y, X) = Link.make {outer = SOME y, inner = X}
  fun mk_slink' (y, xs) = Link.make {outer = SOME y, inner = NameSet.fromList xs}

  (* Find all substitutions tau that satisfy the equation
   *
   *   sigma tau = upsilon
   * 
   * given subsitutions sigma and upsilon having the same outer names.
   * 
   * The algorithm is as follows:
   * 
   * 1) Lay out the links of sigma and upsilon as lists sorted by their
   *    outer names:
   *      sigma   = [y_1/X_1, ..., y_m/X_m]
   *      upsilon = [z_1/V_1, ..., z_n/V_n]
   * 2) If the lists are of different lengths return NoMatch.
   * 3) Let tau_0 = id_0
   * 4) For each pair of links (y_i/X_i, z_i/V_i)
   *      if y_i <> z_i or |X_i| = 0 <> |V_i| then
   *        return NoMatch
   *      else
   *        if |X_i| = 0 then
   *          tau_i = tau_i-1
   *        else
   *          assign an order to the elements of X_i = [x_i1, ..., x_ik]
   *          for each ordered partition [V_i1, ..., V_ik] of V_i into k groups
   *            tau_i = tau_i-1 * \bigox_{j \in {1,...,k}} x_ij/Vij
   *          is a solution
   * 5) return {tau = tau_m}
   * 
   * FIXME find a better name
   *)
  fun find_tau {sigma, upsilon} = lzmake (fn () =>
      let
        fun link_compare ({outer = SOME x1, inner = _},
                          {outer = SOME x2, inner = _}) =
            Name.compare (x1, x2)
          | link_compare _ = raise ThisCannotHappen
                             (* all links of substitutions have outer names *)
        val yXs = ListSort.sort
                    link_compare
                    (map Link.unmk (LinkSet.list (Wiring.unmk sigma)))
        val zVs = ListSort.sort
                    link_compare
                    (map Link.unmk (LinkSet.list (Wiring.unmk upsilon)))

        fun process_link_pair [] [] tau = lzCons (fn () => ({tau = tau}, lzNil))
          | process_link_pair ({outer = SOME y_i, inner = X_i}::yXs)
                              ({outer = SOME z_i, inner = V_i}::zVs)
                              tau =
            if not (Name.== (y_i, z_i)) orelse
               (NameSet.size X_i = 0 andalso NameSet.size V_i <> 0) then
              raise NoMatch
            else
              let
                val x_ijs = NameSet.list X_i
                val k     = NameSet.size X_i
                fun process_V_i_partition V_ijs =
                    let
                      val xVi_links
                        = ListPair.map
                            (fn (x_ij, V_ij) =>
                                Link.make {outer = SOME x_ij, inner = V_ij})
                            (x_ijs, V_ijs)
                    in
                      process_link_pair
                        yXs zVs
                        (Wiring.* (tau, Wiring.make' xVi_links))
                    end
              in
                lzconcat
                  (lzmap
                     process_V_i_partition
                     (lzmap
                        (map NameSet.fromList)
                        (opartgen2lzlist
                           (OrderedPartition.make (NameSet.list V_i) k)
                         handle OrderedPartition.NoPartitions => lzNil)))
              end
          | process_link_pair _ _ _ = raise NoMatch
      in
        lzunmk (process_link_pair yXs zVs Wiring.id_0)
      end
      handle NoMatch => Nil)

  (* Find all substitutions tau, some nameset Y, and all renamings ename that
   * satisfy the equation
   *
   *    sigma tau = ename (upsilon * Y)
   * 
   * given subsitutions sigma and upsilon.
   * 
   * The algorithm is as follows (could be more sophisticated...):
   *
   * 1) Lay out the links of sigma and upsilon as lists:
   *      sigma   = [s_1/X_1, ..., s_m/X_m]
   *      upsilon = [z_1/V_1, ..., z_n/V_n]
   * 2) If n > m then return NoMatch
   * 3) Let Y be a set of fresh names with |Y| = m - n
   *    and let  Y \cup {z_i} = [w_1, ..., w_m]
   * 4) For all permutaions pi of m elements
   *      let ename = {s_i/w_pi(i)}
   *      the solutions given by
   *        find_tau {sigma = sigma, upsilon = ename (upsilon * Y)}
   *      are also solutions to the above equation
   * 
   * FIXME might find several equivalent solutions as the permutation of two
   *       ws is insignificant if they are in Y.
   * FIXME find a better name
   *)
  fun find_tau' {sigma, upsilon} = lzmake (fn () =>
      let
        val sigma'   = Wiring.unmk sigma
        val upsilon' = Wiring.unmk upsilon
        val m = LinkSet.size sigma'
        val n = LinkSet.size upsilon'
      in
        if n > m then
          Nil
        else
          let
            val sXs = LinkSet.list sigma'
            val zVs = LinkSet.list upsilon'
            val ys = List.tabulate (m - n, fn _ => Name.fresh NONE)
            val Y = NameSet.fromList ys
            val wir_Y = Wiring.introduce Y
            val ws = foldr (fn (l, ws) => (valOf (#outer (Link.unmk l)))::ws)
                           ys zVs
            val ss = foldr (fn (l, ss) => (valOf (#outer (Link.unmk l)))::ss)
                           [] sXs
            fun process_ws_permutation ws' =
                let
                  val ename = NameMap.fromList (ListPair.zip (ss, ws'))
                  fun add_ename_Y {tau} = {tau = tau, ename = ename, Y = Y}
                in
                  lzmap
                    add_ename_Y
                    (find_tau {sigma = sigma,
                               upsilon = app_ename
                                           ename
                                           (Wiring.* (upsilon, wir_Y))})
                end
          in
            lzunmk (lzconcat (lzmap process_ws_permutation (all_perms ws)))
          end
      end)

  (* find all substitutions rho and tau and all namesets Z
   * satisfying the equation
   * 
   *   rho (id_Z * sigma tau) = upsilon
   *
   * given substitutions sigma and upsilon.
   * 
   * The algorithm is as follows:
   *
   * 1) deconstruct upsilon
   *      upsilon = [v_1/W_1, ..., v_m/W_m]
   * 2) let n be the number of links in sigma. 
   * 3) if m = 0 and n > 0 then return NoMatch
   * 4) let  rho_0 = tau_0 = id_0
   *         Z_0 = {}
   * 5) for each ordered partition  \bigox_{i \in {1,...,m}} s_i  of sigma into m substitutions
   *      for each link v_i/W_i
   *        let  s_i : Q_i -> U_i
   *             Q_i = [q_i1, ..., q_ik]
   *        if k = 0 then
   *          let S_i = W_i and X_i = {}
   *        else
   *          find all ordered partitions of W_i = S_i \cup X_i
   *        for each partition W_i = S_i \cup X_i
   *          Z_i   = Z_i-1 \cup S_i
   *          rho_i = rho_i-1 * v_i/(U_i \cup S_i)
   *          for each ordered partition \bigcup_{j \in {1,...,k}}X_ij of X_i in k groups
   *            tau_i = tau_i-1 * \bigox q_ij/X_ij
   * 6) return {rho = rho_m, tau = tau_m, Z = Z_m}
   * 
   * FIXME find a better name
   *)
  fun find_rho_tau_Z {sigma, upsilon} = lzmake (fn () =>
      let
        val sigma'   = Wiring.unmk sigma handle e => raise e
        val upsilon' = Wiring.unmk upsilon handle e => raise e
        val m = LinkSet.size upsilon'
        val n = LinkSet.size sigma'
      in
        if m = 0 andalso n > 0 then
          Nil
        else
          let 
            val vWs = LinkSet.list upsilon' handle e => raise e
            val sigma_link_list = LinkSet.list sigma' handle e => raise e

            fun process_link rho tau Z [] [] =
                lzCons (fn () => ({rho = rho, tau = tau, Z = Z}, lzNil))
              | process_link rho tau Z (vW_i::vWs) (s_i::s_is) = lzmake (fn () =>
                let
                  val (v_i, W_i) = case Link.unmk vW_i of
                                     {outer = SOME v_i, inner = W_i} => (v_i, W_i)
                                   | _ => raise ThisCannotHappen (*upsilon is not a substitution*)
                  val Q_i  = Wiring.innernames s_i handle e => raise e
                  val q_is = NameSet.list Q_i
                  val k    = NameSet.size Q_i
                  val U_i  = Wiring.outernames s_i handle e => raise e
                  (* construct suitable lazy lists of partitions
                   * of W_i depending on k *)
                  val W_i_partitions
                    = if k = 0 then
                        lzCons (fn () =>
                                   ({S_i = W_i, X_i = NameSet.empty}, lzNil))
                      else
                        lzmap
                          (fn [s_is, x_is] =>
                              {S_i = NameSet.fromList s_is handle e => raise e,
                               X_i = NameSet.fromList x_is handle e => raise e}
                            | _ => raise ThisCannotHappen)
                          (opartgen2lzlist
                             (OrderedPartition.make (NameSet.list W_i) 2)
                           handle OrderedPartition.NoPartitions => lzNil)
 handle e => raise e
 
                  (* find solutions for each ordered partition of X_i *)
                  fun process_X_i_partition rho_i Z_i X_ijs =
                      let
                        val tau_i
                          = Wiring.* (tau,
                                      Wiring.make'
                                        (ListPair.map
                                           (fn (q_ij, X_ij) =>
                                               Link.make {outer = SOME q_ij,
                                                          inner = X_ij})
                                           (q_is, X_ijs)))
                      in
                        process_link rho_i tau_i Z_i vWs s_is handle e => raise e
                      end handle e => raise e

                  (* find solutions for each partition of W_i *)
                  fun process_W_i_partition {S_i, X_i} =
                      let
                        val rho_i
                          = Wiring.* (rho,
                                      Wiring.make'
                                        [Link.make
                                           {outer = SOME v_i,
                                            inner = (NameSet.union U_i S_i) handle e => raise e}])
                        val Z_i = NameSet.union Z S_i handle e => raise e
                      in
                        lzconcat 
                          (lzmap
                             (process_X_i_partition rho_i Z_i)
                             (lzmap
                                (map (fn ls => (NameSet.fromList ls handle e => raise e)))
                                (opartgen2lzlist
                                   (OrderedPartition.make
                                      (NameSet.list X_i)
                                      k)
                                 handle OrderedPartition.NoPartitions => lzNil)))
                      end handle e => raise e
                in
                  lzunmk (lzconcat (lzmap process_W_i_partition W_i_partitions)) handle e => raise e
                end)
              | process_link _ _ _ _ _ = raise NoMatch
          in
            lzunmk
              (lzconcat
                 (lzmap
                    (process_link Wiring.id_0 Wiring.id_0 NameSet.empty vWs handle e => raise e)
                    (lzmap
                       (map Wiring.make')
                       (opartgen2lzlist
                          (OrderedPartition.make sigma_link_list m)
                        handle OrderedPartition.NoPartitions => lzNil))))
          end
      end
      handle NoMatch => Nil)

  (* find all substitutions rho and tau and all namesets Z and some namesets Y
   * with  sigma : -> Q  and  |Y| <= |Q|  satisfying the equation
   * 
   *   rho (id_Z * sigma tau) = upsilon * Y
   *
   * given substitutions sigma and upsilon.
   * 
   * The algorithm is as follows:
   *
   * 1) deconstruct upsilon
   *      upsilon = [v_1/W_1, ..., v_m/W_m]
   * 2) let n be the number of links in sigma. 
   * 3) let  rho_0 = tau_0 = id_0
   *         Z_0 = {}
   * 4) for each ordered partition  \bigox_{i \in {1,...,m,m+1}} s_i  of sigma into m + 1 substitutions
   *      for each link v_i/W_i
   *        let  s_i : Q_i -> U_i
   *             Q_i = [q_i1, ..., q_ik]
   *        if k = 0 then
   *          let S_i = W_i and X_i = {}
   *        else
   *          find all partitions of W_i = S_i \cup X_i
   *        for each partition W_i = S_i \cup X_i
   *          for each ordered partition \bigcup_{j \in {1,...,k}}X_ij of X_i in k groups
   *            rho_i = rho_i-1 * v_i/(U_i \cup S_i)
   *            tau_i = tau_i-1 * \bigox q_ij/X_ij
   *            Z_i   = Z_i-1 \cup S_i
   *      for each partition  \bigox_{j \in {m+1,...,m+t}} sm_j  of s_m+1  (into any number of partitions)
   *        let  Y_m = {}
   *        for each sm_j : Q_j -> U_j
   *          Y_j   = Y_j-1 \cup {y_j}     where y_j is fresh
   *          rho_j = rho_j-1 * y_j/U_j
   *          tau_j = tau_j-1 * Q_j
   * 5) return {rho = rho_m+t, tau = tau_m+t, Z = Z_m, Y = Y_m+t}
   * 
   * FIXME find a better name
   *)
  fun find_rho_tau_Z' {sigma, upsilon} = lzmake (fn () =>
      let
        val sigma'   = Wiring.unmk sigma
        val upsilon' = Wiring.unmk upsilon
        val m = LinkSet.size upsilon'
        val n = LinkSet.size sigma'
      in
        if m = 0 andalso n > 0 then
          Nil
        else
          let 
            val vWs = LinkSet.list upsilon'
            val sigma_link_list = LinkSet.list sigma'

            fun process_s_m_plus_1_partition rho tau Z sm_js =
                let
                  fun introduce_name (sm_j, r as {rho, tau, Z, Y}) =
                      if Wiring.is_id0 sm_j then
                        r
                      else
                        let
                          val y_j = Name.fresh NONE
                          val Q_j = Wiring.innernames sm_j
                          val U_j = Wiring.outernames sm_j
                        in
                          {Y = NameSet.insert y_j Y,
                           rho = Wiring.* (rho, Wiring.make' [mk_slink (y_j, U_j)]),
                           tau = Wiring.* (tau, Wiring.introduce Q_j),
                           Z = Z}
                        end
                in
                  foldr
                    introduce_name
                    {rho = rho, tau = tau, Z = Z, Y = NameSet.empty}
                    sm_js
                end

            fun process_link rho tau Z [] [s_m_plus_1] =
                lzmap
                  (process_s_m_plus_1_partition rho tau Z)
                  (lzmap
                     (map Wiring.make')
                     (partgen2lzlist
                        (Partition.make
                           (LinkSet.list (Wiring.unmk s_m_plus_1))
                           (* the number of outer names is the
                            * maximum number of non-empty partitions *)
                           (NameSet.size (Wiring.outernames s_m_plus_1)))
                      handle Partition.NoPartitions => lzNil))
              | process_link rho tau Z (vW_i::vWs) (s_i::s_is) = lzmake (fn () =>
                let
                  val (v_i, W_i) = case Link.unmk vW_i of
                                     {outer = SOME v_i, inner = W_i} => (v_i, W_i)
                                   | _ => raise ThisCannotHappen (*upsilon is not a substitution*)
                  val Q_i  = Wiring.innernames s_i
                  val q_is = NameSet.list Q_i
                  val k    = NameSet.size Q_i
                  val U_i  = Wiring.outernames s_i
                  (* construct suitable lazy lists of partitions
                   * of W_i depending on k *)
                  val W_i_partitions
                    = if k = 0 then
                        lzCons (fn () =>
                                   ({S_i = W_i, X_i = NameSet.empty}, lzNil))
                      else
                        lzmap
                          (fn [s_is, x_is] =>
                              {S_i = NameSet.fromList s_is,
                               X_i = NameSet.fromList x_is}
                            | _ => raise ThisCannotHappen)
                          (opartgen2lzlist
                             (OrderedPartition.make (NameSet.list W_i) 2)
                           handle OrderedPartition.NoPartitions => lzNil)

                  (* find solutions for each ordered partition of X_i *)
                  fun process_X_i_partition rho_i Z_i X_ijs =
                      let
                        val tau_i
                          = Wiring.* (tau,
                                      Wiring.make'
                                        (ListPair.map
                                           (fn (q_ij, X_ij) =>
                                               Link.make {outer = SOME q_ij,
                                                          inner = X_ij})
                                           (q_is, X_ijs)))
                      in
                        process_link rho_i tau_i Z_i vWs s_is
                      end

                  (* find solutions for each partition of W_i *)
                  fun process_W_i_partition {S_i, X_i} =
                      let
                        val rho_i
                          = Wiring.* (rho,
                                      Wiring.make'
                                        [Link.make
                                           {outer = SOME v_i,
                                            inner = NameSet.union U_i S_i}])
                        val Z_i = NameSet.union Z S_i
                      in
                        lzconcat 
                          (lzmap
                             (process_X_i_partition rho_i Z_i)
                             (lzmap
                                (map NameSet.fromList)
                                (opartgen2lzlist
                                   (OrderedPartition.make
                                      (NameSet.list X_i)
                                      k)
                                 handle OrderedPartition.NoPartitions => lzNil)))
                      end
                in
                  lzunmk (lzconcat (lzmap process_W_i_partition W_i_partitions))
                end)
              | process_link _ _ _ _ _ = raise NoMatch
          in
            lzunmk
              (lzconcat
                 (lzmap
                    (process_link Wiring.id_0 Wiring.id_0 NameSet.empty vWs)
                    (lzmap
                       (map Wiring.make')
                       (opartgen2lzlist
                          (OrderedPartition.make sigma_link_list (m + 1))
                        handle OrderedPartition.NoPartitions => lzNil))))
          end
      end
      handle NoMatch => Nil)

  (* Match a global discrete prime g to a context G using the PAX rule.
   * The main task of this rule is to split sigma_a = sigma (id_Z * alpha tau)
   * such that the it matches the wiring of s_C.
   * In terms of the names used in the code, we are looking for solutions to
   * the equation
   * 
   *    Y * (ename * ename'') s_a_e * s_a_n
   *  = (s_C' (id_Z * s_C_n) * s_C_e) (id_Z * alpha tau)
   *
   * where Y, ename', s_C', id_Z, and tau are unknown.
   *
   * The approach taken is to split the problem into four smaller problems
   * 
   * 1)  s_Ce  tau_e                    =  ename s_ae                
   * 2)  s_Ce' tau'_e                   =  ename'_e (Q_e * s_ae'_e)  
   * 3)  s_C'_1 (id_Z * s_Cn tau_n)     =  s_a_n                     
   * 4)  s_C'_2 (id_Z' * s_Cn' tau'_n)  =  Q_n * s_ae'_n
   * 
   * where
   * 
   *   ename''  =  ename'_n * ename'_e
   *   s_a_e    =  s_ae * Q''_n * s_a'n * s_a'e * Q''_e
   * 
   *   ename    :  Q_e  -> Y_e
   *   ename'_n :  Q''_n \uplus Q'_n  ->  Y'_n
   *   ename'_e :  Q''_e \uplus Q'_e  ->  Y'_e
   *   s_ae     :  U_e  -> Q_e
   *   s_a'n    :  U'_n -> Q'_n
   *   s_a'e    :  U'_e -> Q'_e
   *   s_a_n    :  U_n  -> Y_n
   * 
   *   tau      =  tau_n * tau'_n * tau_e * tau'_e
   *   alpha    =  alpha_n * alpha'_n * alpha_e * alpha'_e
   *   s_C_n    =  s_Cn * s_Cn'
   *   s_C'     =  s_C'1 * s_C'2
   *   s_C_e    =  s_Ce * s_Ce'
   * 
   * FIXME describe algoritm
   *)
  fun matchPAX' {ename, s_a as {s_a_e, s_a_n}, s_C as {s_C_e, s_C_n}, g, G} =
      lzmake (fn () => ((*print "PAX' ";*)
      case #Ss (unmkG G) of
        [S] =>
      (case unmkS S of
        BgBDNF.SCon (i, alpha) =>
        (let
          (* Remember to trim s_a to fit the outer face of g, as this is
           * not done explicitly in the PARn rule.
           *)
          val s_a_n = Wiring.restrict' s_a_n (glob (outerface g))
          val s_a_e = Wiring.restrict' s_a_e (glob (outerface g))
          (*val _ = print ("after : s_a_e = " ^ Wiring.toString s_a_e ^
                       ", s_a_n = " ^ Wiring.toString s_a_n ^ 
                       "s_C_e = " ^ Wiring.toString s_C_e ^
                       ", s_C_n = " ^ Wiring.toString s_C_n ^ "\n")*) 
          val Y_n  = Wiring.outernames s_a_n
          val Y_e  = NameSet.fromList (NameMap.range ename)

          (* Split s_a_e into the part which has already been matched s_ae
           * (specified by ename) and the rest s_a_e' *)
          val {inCod = s_ae, notInCod = s_a_e'}
            = Wiring.split_outer s_a_e (NameSet.fromList (NameMap.dom ename))

          (* Split alpha into the part related to s_C_n and the part
           * related to s_C_e
           * and compute the compositions s_C_n alpha_n and s_C_e alpha_e. *)
          val {inCod = alpha_n, notInCod = alpha_e}
            = Wiring.split_outer alpha (Wiring.innernames s_C_n)
          val s_C_n_alpha = Wiring.o (s_C_n, alpha_n)
          val s_C_e_alpha = Wiring.o (s_C_e, alpha_e)

          val {inCod = s_Ce, notInCod = s_Ce'}
            = Wiring.split_outer s_C_e_alpha Y_e
            
          (* Generate splits of s_a_e' and s_C_n_alpha
           * 
           *   s_a_e'       =  s_ae'_n * s_ae'_e and
           *   s_C_n_alpha  =  s_Cn * s_Cn'
           * 
           * using ordered partitions of their constituents.
           * Then try to solve the subproblems
           *
           * 1)  s_Ce  tau_e                    =  ename s_ae                and
           * 2)  s_Ce' tau'_e                   =  ename'_e (Q_e * s_ae'_e)  and
           * 3)  s_C'_1 (id_Z * s_Cn tau_n)     =  s_a_n                     and
           * 4)  s_C'_2 (id_Z' * s_Cn' tau'_n)  =  Q_n * s_ae'_n
           *
           * FIXME should we calculate all solutions to each and then generate combinations?
           * NB! We calculate the same solutions for each combination...
           *     Slow but easy and memory preserving...
					 * FIXME: What if s_ae or s_ae'_e are empty, but s_Ce or s_Ce', respectively,
					 *     are not?  Should we then add a name introduction to s_ae or s_ae'_e,
					 *     and return it in Y?
           *)
          val s_C_n_alpha_links = LinkSet.list (Wiring.unmk s_C_n_alpha)
          val s_a_e'_links      = LinkSet.list (Wiring.unmk s_a_e')

          (* solve (4) and construct result
           * taue = tau_e * tau'_e *)
          fun solve_4 s_ae'_n s_Cn' taue ename'_e Q_e
                      {rho = s_C'_1, tau = tau_n, Z} =
              let
                val tauen = Wiring.* (taue, tau_n)
                fun to_solution {rho = s_C'_2, tau = tau'_n, Z = Z', Y = Q_n} =
                    let
                      val id_Z = Wiring.id_X (NameSet.union Z Z')
                      val tau  = Wiring.* (tauen, tau'_n)
                    in
                      {ename' = NameMap.plus (ename, ename'_e),
                       Y      = NameSet.union Q_e Q_n,
                       s_C'   = Wiring.* (s_C'_1, s_C'_2),
                       qs     = [makeP tau (makeN (Wiring.innernames tau) g handle e => raise e)],
                       tree   = PAX'}
                    end
              in
              (*print ("s_ae'_n = " ^ Wiring.toString s_ae'_n ^
                     ", s_Cn' = " ^ Wiring.toString s_Cn' ^ "\n");*)
                lzmap
                  to_solution
                  (find_rho_tau_Z' {sigma   = s_Cn',
                                    upsilon = s_ae'_n})
              end
          (* solve (3) and then find solutions for (4) *)
          fun solve_34 s_ae'_n s_Cn s_Cn' tau_e
                       {tau = tau'_e, ename = ename'_e, Y = Q_e} =
              ((*print ("s_Cn = " ^ Wiring.toString s_Cn ^ ", ");*)
              lzconcat
                (lzmap
                   (solve_4 s_ae'_n s_Cn' (Wiring.* (tau_e, tau'_e)) ename'_e Q_e)
                   (find_rho_tau_Z {sigma   = s_Cn,
                                    upsilon = s_a_n})))
          (* solve (2) and then find solutions for (3) and (4) *)
          fun solve_234 s_ae'_n s_ae'_e s_Cn s_Cn' {tau = tau_e} =
              ((*print ("s_ae'_e = " ^ Wiring.toString s_ae'_e ^
                      ", s_Ce' = " ^ Wiring.toString s_Ce' ^ "\n");*)
              lzconcat
                (lzmap
                   (solve_34 s_ae'_n s_Cn s_Cn' tau_e)
                   (find_tau' {sigma   = s_Ce',
                               upsilon = s_ae'_e})))
          (* solve (1) and then find solutions for (2), (3), and (4) *)
          fun solve_1234 s_ae'_n s_ae'_e s_Cn s_Cn' =
              ((*print ("s_ae = " ^ Wiring.toString s_ae ^
                      ", app_ename ename s_ae = " ^
                      Wiring.toString (app_ename ename s_ae) ^ ", ");*)
              lzconcat
                (lzmap
                   (solve_234 s_ae'_n s_ae'_e s_Cn s_Cn')
                   (find_tau {sigma   = s_Ce,
                              upsilon = app_ename ename s_ae})))

          fun process_s_C_n_alpha_split s_ae'_n s_ae'_e [s_Cn, s_Cn'] =
              solve_1234 s_ae'_n s_ae'_e s_Cn s_Cn'
            | process_s_C_n_alpha_split _ _ _ = raise ThisCannotHappen

          fun process_s_a_e'_split [s_ae'_n, s_ae'_e] =
              lzconcat
                (lzmap
                   (process_s_C_n_alpha_split s_ae'_n s_ae'_e)
                   (lzmap
                     (map (fn ls => Wiring.make' ls handle e => raise e))
                     (opartgen2lzlist
                        (OrderedPartition.make s_C_n_alpha_links 2)
                      handle OrderedPartition.NoPartitions => lzNil)))
            | process_s_a_e'_split _ = raise ThisCannotHappen
        in
          lzunmk
            (lzconcat
               (lzmap
                  process_s_a_e'_split
                  (lzmap
                     (map (fn ls => Wiring.make' ls handle e => raise e))
                     (opartgen2lzlist
                        (OrderedPartition.make s_a_e'_links 2)
                      handle OrderedPartition.NoPartitions => lzNil))))
        end
        handle NoMatch => Nil)
      | _ => Nil)
  (* Match a global discrete prime g to a context G using the ZAX rule:
   *   Y, id_e, Y |- 1, [[id_0]]^P ~~> 1, [[id_0]]^P.
   * 1) Check that g = 1 and G = 1.
   * 2) Return ename' = ename, s_C' = {}, qs = [].
   *)
(*FIXME is this right? *)
      | [] (* This implies G = 1. *)
      => (case #Ss (unmkG g) of
            [] (* This implies g = 1. *)
          => Cons ({ename' = ename,
                  s_C' = Wiring.id_0,
                  Y = NameSet.empty,
                  qs = [],
                  tree = ZAX'}, lzNil)
          | _ => Nil)
      | _ => Nil))
  
  (* Match a global discrete prime to a context using the MER rule:
   * 1) Find agent width n and context width m.
   * 2) If n = 1, return NO MATCH (to avoid infinite recursion).
   * 3) Deconstruct agent g = (id * merge) (m_0 * ... * m_n-1)
   * 4) Deconstruct context G = (id * merge) (S_0 * ... * S_m-1)
   * 5) Compute a list Xss of local inner name set lists for S_i's
   * 6) For each partition rho(n,m) of n into m subsets
   *    and each m-permutation pi,
   *     6a) Compute pibar as pi pushed through Xss.
   *     6b) Construct es as a tensor product of merged partitions
   *         of agent m_i's
   *     6c) Construct Es as a tensor product of S_{pi(i)}'s
   *     6d) Infer premise using ename, s_a, s_C, es, Es and pibar,
   *         yielding ename', s_C', qs.
   *     6e) Return ename', s_C', qs.
   *
   * Function tryPis tries to generate matches with one partition
   * and all hitherto known permutation, while
   * function tryRhos tries to generate matches with one permutation
   * and all hitherto known partitions.
   * The primed versions are used when there are no more permutation
   * or partitions left, respectively
   *)
   (* NOT FULLY IMPLEMENTED YET! *)
  and matchMER' {ename, s_a, s_C, g, G} = lzmake (fn () =>
    let (*val _ = print "MER' ";*)
      val ms = #Ss (unmkG g)
      val n = length ms
      val Ss = #Ss (unmkG G)
	    val m = length Ss
    in
      if n <= 1 orelse (n > 0 andalso m = 0) then
        LazyList.Nil
      else
        let
		      val Xss = map (loc o innerface) Ss
		      fun try pi mss =
		          let
		            val pibar = Permutation.pushthru pi Xss
		            val es = map makeG mss
		            val Es = map (makeG o (fn S => [S])) (Permutation.permute pi Ss)
		            val premise_matches
		                = matchPER' {ename = ename,
		                             s_a = s_a, s_C = s_C,
		                             es = es, Es = Es,
		                             pi = pibar}
		            fun make_match {ename', s_C', Y, qs, tree} =
		                {ename' = ename', s_C' = s_C', Y = Y, qs = qs, tree = MER' tree}
		          in
		            lzmap make_match premise_matches
		          end
		      fun tryPis' allpis allmss mss rho perm (pi :: pis)
		        = lzappend
		            (try pi mss)
		            (tryPis' allpis allmss mss rho perm pis)
		        | tryPis' allpis allmss mss rho perm []
		        = tryPis'
		            allpis (mss :: allmss)
		            (Partition.next rho) rho perm allpis
		          handle Partition.NoPartitions => lzNil
		      fun tryRhos' allpis allmss (pi, perm) rho (mss :: msss)
		        = lzappend
		            (try pi mss)
		            (tryRhos' allpis allmss (pi, perm) rho msss)
		        | tryRhos' allpis allmss (pi, perm) rho []
		        = let
		            val perm = nextperm perm
                            val pi   = toperm perm
		          in
		            tryRhos'
		              allpis allmss
		              (Permutation.copy pi, perm) rho allmss
		          end
		          handle NoMorePerms => lzNil
		      fun tryPis allpis allmss mss rho perm (pi :: pis)
		        = lzappend
		            (try pi mss)
		            (tryPis allpis allmss mss rho perm pis)
		        | tryPis allpis allmss mss rho perm []
		        = let
		            val perm = nextperm perm
                            val pi   = toperm perm
		          in
		            tryRhos
		              allpis (mss :: allmss)
		              (Permutation.copy pi, perm) rho (mss :: allmss)
		          end
		          handle NoMorePerms =>
		            (tryPis'
		               allpis (mss :: allmss)
		               (Partition.next rho) rho perm allpis
		             handle Partition.NoPartitions => lzNil)
		      and tryRhos allpis allmss (pi, perm) rho (mss :: msss)
		        = lzappend
		            (try pi mss)
		            (tryRhos allpis allmss (pi, perm) rho msss)
		        | tryRhos allpis allmss (pi, perm) rho []
		        = tryPis
		            (pi :: allpis) allmss
		            (Partition.next rho) rho perm (pi :: allpis)
		          handle Partition.NoPartitions =>
		            (let
                               val perm = nextperm perm
                               val pi   = toperm perm
		             in
		               tryRhos'
		                 allpis allmss
		                 (Permutation.copy pi, perm) rho allmss
		             end
		             handle NoMorePerms => lzNil)
		      val perm = firstperm (map (fn _ => NameSet.empty) Xss)
                      val pi   = toperm perm
		    in
		      lzunmk
		        (tryRhos
		           [] []
		           (Permutation.copy pi, perm) (Partition.make ms m) [])
				end
    end handle e => raise e)

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
  and matchION' {ename, s_a as {s_a_e, s_a_n}, s_C as {s_C_e, s_C_n}, g, G} =
      lzmake (fn () => ((*print ("ION': s_a_e=" ^ Wiring.toString s_a_e ^ "\ns_a_n="
      ^ Wiring.toString s_a_n ^ "\ng=" ^ BgBDNF.toString g ^ "\ns_C_e="
      ^ Wiring.toString s_C_e ^ "\ns_C_n="
      ^ Wiring.toString s_C_n ^ "\nG=" ^ BgBDNF.toString G ^ "\n");*)
      case #Ss (unmkG g) of
        [s] =>
      (case unmkS s of
        BgBDNF.SMol m =>
      (case #Ss (unmkG G) of
        [S] =>
      (case unmkS S of
        BgBDNF.SMol M =>
        (let
          val {id_Z = _, KyX = KyX, N = n} = unmkM m
          val {id_Z = _, KyX = LuZ, N = N} = unmkM M
          val {ctrl = K, free = ys, bound = Xs} = Ion.unmk KyX
          val {ctrl = L, free = us, bound = Zs} = Ion.unmk LuZ

          val _ = if K <> L then raise NoMatch else ()

          val ename''
            = ListPair.foldlEq
                (fn (yi, ui, ename'') =>
                    if Wiring.in_domain ui s_C_e handle e => raise e then
                      check_adjust'
                        ename''
                        (Wiring.app_renaming_x s_a_e yi handle e => raise e)
                        (Wiring.app_renaming_x s_C_e ui handle e => raise e)
                    else
                      ename'')
                ename (ys, us)
              handle ListPair.UnequalLengths => raise NoMatch
          
          val (vX, vZ)
            = (fn (vXl, vZl) => (Wiring.make' vXl, Wiring.make' vZl))
              (ListPair.foldrEq
                 (fn (Xi, Zi, (vXl, vZl)) =>
                     let
                       val vi = SOME (Name.fresh NONE)
                     in
                       ((Link.make {outer = vi, inner = Xi}) :: vXl,
                        (Link.make {outer = vi, inner = Zi}) :: vZl)
                     end)
                 ([], []) (Xs, Zs))
              handle ListPair.UnequalLengths => raise NoMatch

          val premise_matches = matchABS' {ename = ename'',
                                           s_a = s_a, s_C = s_C,
                                           p = BgBDNF.makeP vX n,
                                           P = BgBDNF.makeP vZ N}

          fun make_match ({ename', s_C', Y, qs, tree}, rest) =
              let
                val (ename', s_C')
                  = ListPair.foldlEq
                      (fn (yi, ui, (ename', s_C')) =>
                          if not (Wiring.in_domain ui s_C_e) then
                            let
                              val u' = if Wiring.in_domain ui s_C_n then
                                         Wiring.app_renaming_x s_C_n ui
 handle e => raise e                                       else
                                         ui
                            in
                              if Wiring.in_domain yi s_a_e then
                                check_adjust
                                  ename' s_C'
                                  (Wiring.app_renaming_x s_a_e yi handle e => raise e) u'
                              else
                                (ename',
                                 check_adjust'
                                   s_C' u' (Wiring.app_renaming_x s_a_n yi handle e => raise e))
                            end
                          else
                            (ename', s_C'))
                      (ename', Wiring.unmk_sub s_C') (ys, us)
                    handle ListPair.UnequalLengths => raise NoMatch
              in
                lzCons
                  (fn () =>
		                ({ename' = ename',
		                  s_C' = Wiring.make_ren s_C',
		                  Y = Y,
		                  qs = qs,
		                  tree = ION' tree},
		                  rest ()))
              end handle NoMatch => lzNil
        in
          lzunmk (lzfoldr make_match lzNil premise_matches)
        end
        handle NoMatch => Nil)
      | _ => Nil)
      | _ => Nil)
      | _ => Nil)
      | _ => Nil) handle e => raise e)

  (* Match a global discrete prime using a PAX, MER or ION rule:
   * 1) If PAX rule matches return this match,
   *    else if ION rule matches return this match,
   *    else if the agent contains n > 1 top-level molecules
   *    and MER matching is allowed
   *    (to avoid infinite recursion via MER-PAR-PARe-PARn),
   *    return any MER rule matches.
   *)
  and matchDG' allowMER (args as {ename, s_a, s_C, g, G}) =
    lzmake (fn () =>
      case lzunmk (matchPAX' args) of
        mz as (LazyList.Cons _) => mz
      | _
      => case (lzunmk (matchION' args), allowMER) of
           (mz as (LazyList.Cons _), _) => mz
         | (_, true)
         => (case #Ss (unmkG g) of
               (_ :: _ :: _) => lzunmk (matchMER' args)
             | _ => Nil)
         | _ => Nil)
         
(*    
    lzappend (matchPAX' args)
        (lzappend (matchION' args)
          (if allowMER then
            lzmake (fn () =>
              case #Ss (unmkG g) of
                (_ :: _ :: _) => lzunmk (matchMER' args)
              | _ => Nil)
           else
             lzNil)) *)

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
  and matchABS' {ename, s_a as {s_a_e, s_a_n}, s_C as {s_C_e, s_C_n}, p, P} =
      lzmake (fn () =>
      let
        val {s = s_a_L, N = n, ...} = BgBDNF.unmkP p
        val {s = s_C_L, N = N, ...} = BgBDNF.unmkP P
        val {absnames = Z, G = g} = BgBDNF.unmkN n
        val {absnames = U, G = G} = BgBDNF.unmkN N

        val s_a_n' = Wiring.* (s_a_n, s_a_L)
        val s_C_n' = Wiring.* (s_C_n, s_C_L)

        val premise_matches = matchDG' true
                                       {ename = ename,
                                        s_a = {s_a_e = s_a_e, s_a_n = s_a_n'},
                                        s_C = {s_C_e = s_C_e, s_C_n = s_C_n'},
                                        g = g, G = G}

        fun make_match {ename', s_C', Y, qs, tree} =
            {ename' = ename',
             s_C' = Wiring.restrict s_C' (*FIXME be smarter...*)
                      (NameSet.difference
                         (Wiring.innernames s_C')
                         (Wiring.outernames s_C_L)),
             Y = Y,
             qs = qs,
             tree = ABS' tree}
      in
        lzunmk (lzmap make_match premise_matches)
      end handle e => raise e)

  (* Match a parallel composition to a context using the PARn rule:
   * 1) Let ename_0 = ename.
   * 2) Using ename_i, s_a, s_C, ei, Ei, infer premise i, yielding
   *    ename_i+1, s_C'i, qs_i.
   * 3) Compute s_C' by merging s_C'is using extension,
   *      returning NOMATCH if impossible.
   * 4) Return ename' = ename_n, s_C', qss = [qs_0, ..., qs_n-1]
   *)
  and matchPARn' {ename, s_a, s_C, es, Es} = 
      lzmake (fn () =>
      let
        fun build_matches [] [] result  = lzmake (fn () => Cons (result, lzNil))
          | build_matches _ [] _ = raise NoMatch
          | build_matches [] _ _ = raise NoMatch
          | build_matches (e_i::es) (E_i::Es)
              {ename' = ename_i, s_C' = s_C'_i, Y, qss, tree = PARn' trees} =
            let
              val premise_matches
                = matchDG' false 
                    {ename = ename_i, s_a = s_a, s_C = s_C, g = e_i, G = E_i}

              fun extend_result {ename', s_C', Y = Y', qs, tree} =
                  {ename' = ename', s_C' = s_C', Y = NameSet.union Y Y',
                   qss = qs :: qss, tree = PARn' (tree :: trees)}
            in
              lzconcat
                (lzmap (build_matches es Es o extend_result) premise_matches)
            end
          | build_matches _ _ {tree, ...} = raise WrongTree tree
      in
        lzunmk
          (build_matches es Es
             {ename' = ename, s_C' = Wiring.id_0, Y = NameSet.empty,
              qss = [], tree = PARn' []})
      end
      handle NoMatch => Nil)

  (* Match a tensor product to a context using the PARe rule:
   * 1) Infer premise, yielding ename', s_C', qss.
   * 2) Let qs = concat qss.
   * 3) Return ename', s_C', qs.
   *)
  and matchPARe' (args as {ename, s_a, s_C, es, Es}) =
      lzmake (fn () =>
      let
        fun toPARe' {ename', s_C', Y, qss, tree} =
            {ename' = ename',
             s_C'   = s_C',
             Y      = Y,
             qs     = List.concat qss,
             tree   = PARe' tree}
      in
        lzunmk (lzmap toPARe' (matchPARn' args))
      end handle e => raise e)
  
  (* Match a tensor product to a context permutation:
   * 1) Infer premise, yielding ename', s_C', qs.
   * 2) Permute qs by pi, yielding qs' so that qs'_i = qs_pi(i).
   * 3) Return ename', s_C, qs'.
   *)
  and matchPER' {ename, s_a, s_C, es, Es, pi} =
      lzmake (fn () =>
      let
        fun toPER' {ename', s_C', Y, qs, tree} =
            {ename' = ename',
             s_C'   = s_C',
             Y      = Y,
             qs     = Permutation.permute pi qs,
             tree   = PER' tree}
      in
        lzunmk (lzmap toPER' (matchPARe' {ename = ename,
                                          s_a = s_a, s_C = s_C,
                                          es = es, Es = Es}))
      end handle e => raise e)
  
  (* Match a global discrete prime using the SWX rule:
   * If Ps = [P], where P = (id_Z * ^s)(W)G, then
   * 1) Let s_C_e = s_R_e, s_C_n = s * s_R_n, and infer premise
   *    using s_a_e, s_a_n, s_C_e, s_C_n, g, G,
   *    yielding ename', s_C', Y, and qs
   * 2) Let Y_C_e = outernames s_C_e
   * 3) Let U = outernames s
   * 4) Let s_C = s_C' * id_Y_C_e  and  find L' such that  L = s_C(L')
   * 5) if  L' is a subset of U  then
   *      return ename', Y, s_C, G := "U", qs
   *    else
   *      return NoMatch
   *)
  fun matchSWX {ename, 
                s_a = {s_a_e, s_a_n}, L,
                s_R = {s_R_e, s_R_n}, e = g, Ps = [P]}
    = lzmake (fn () => ((*print "SWX ";*)
      let
        val {id_Z, Y = U, s, X = W, N} = unmkP P
        val {absnames = W, G} = unmkN N
        val s_C_e = s_R_e
        val s_C_n = Wiring.* (s, s_R_n)
        val id_Y_C_e = Wiring.id_X (Wiring.outernames s_C_e)
        val i = BgBDNF.info P
        fun toSWX ({ename', s_C', Y, qs, tree}, rest) = ((*print ("SWX: s_C'=" ^ Wiring.toString s_C'
        ^ "\ns_R_e=" ^ Wiring.toString s_R_e
        ^ "\ns_R_n=" ^ Wiring.toString s_R_n
        ^ "\nP=" ^ BgBDNF.toString P
        ^ "\nqs=[" ^ concat (map (fn q => BgBDNF.toString q ^ "\n") qs) ^ "]\n");*)
            let
              val s_C = Wiring.* (s_C', id_Y_C_e)
              val L' = Wiring.app_inverse s_C L
            in
              if NameSet.isEmpty (NameSet.difference L' U) then
                lzCons
                  (fn () =>
                      ({ename' = ename',
                        Y = Y,
                        s_C = s_C,
                        E = makeG [makeS (SCon (i, Wiring.id_X U))],
                        qs = qs,
                        tree = SWX tree},
                       rest ()))
              else
                rest ()
            end)
        val matches
          = matchDG' true 
                     {ename = ename,
                      s_a = {s_a_e = s_a_e,
                             s_a_n = s_a_n},
                      s_C = {s_C_e = s_C_e, s_C_n = s_C_n},
                      g = g, G = G}
      in
        lzunmk (lzfoldr toSWX lzNil matches)
      end) handle e => raise e)
    | matchSWX _ = lzNil

  (* Split a link l "horizontally" into a link, a substitution, and an
   * identity wiring:
   *
   *   l = l' (tau * id_Z) : X -> {y}
   *   l'                  : V \cup Z -> {y}
   *   tau                 : W -> V.
   *   X = W \cup Z
   *
   * Many splits may be possible so the result is a lazy list of all the
   * possible splits on the form {l', tau, id_Z}.
   *
   * The algorithm is as follows:
   *
   * 1) lay out the set V as [v_1, ..., v_m]
   * 2) for all subsets W of X
   *    a) let  id_Z = id_Z  where Z = X\W
   *       and  l'   = y/U   where U = V \cup Z 
   *    b) for each ordered partition [W_1, ..., W_m] of W into m sets
   *         tau = \bigox_{j \in {1,...,m}} v_j/W_j
   *         return {l', tau, id_Z}
   *)
  fun horizontally_split_link l V =
      let
        val m  = NameSet.size V
        val vs = NameSet.list V
        val {outer = y, inner = X} = Link.unmk l

        fun process_W_opartition Wparts =
            Wiring.make' (ListPair.map mk_slink' (vs, Wparts))

        fun process_X_subset W =
            let
              val Z    = NameSet.difference X W
              val id_Z = Wiring.id_X Z
              val U    = NameSet.union V Z
              val l'   = Wiring.make' [Link.make {outer = y, inner = U}]
              fun insert_tau tau = {tau = tau, id_Z = id_Z, l' = l'}
            in
              lzmap
                insert_tau
                (lzmap
                   process_W_opartition 
                   (opartgen2lzlist
                      (OrderedPartition.make (NameSet.list W) m)
                    handle OrderedPartition.NoPartitions => lzNil))
            end
      in
        lzconcat
          (lzmap
             process_X_subset
             (nssubsetgen2lzlist (NameSetSubset.make X)))
      end

  (* As the above except that Z = {}.
   *
   * The algorithm can thus be simplified to the following:
   *
   * 1) lay out the set V as [v_1, ..., v_m]
   * 2) let  l' = y/V
   *    for each ordered partition [X_1, ..., X_m] of X into m sets
   *      tau = \bigox_{j \in {1,...,m}} v_j/X_j
   *      return {l', tau}
   *)
  fun horizontally_split_link' l V =
      let
        val m  = NameSet.size V
        val vs = NameSet.list V
        val {outer = y, inner = X} = Link.unmk l

        fun process_X_opartition Xparts =
            Wiring.make' (ListPair.map mk_slink' (vs, Xparts))

        val l' = Wiring.make' [Link.make {outer = y, inner = V}]

        fun insert_tau tau = {tau = tau, l' = l'}
      in
        lzmap
          insert_tau
          (lzmap
             process_X_opartition
             (opartgen2lzlist
                (OrderedPartition.make (NameSet.list X) m)
              handle OrderedPartition.NoPartitions => lzNil))
      end

  (* Split a substitution "horizontally" into two substitutions and an
   * identity wiring, where the interface between the two substitutions
   * is given by V and some links must "pass through" both substitutions.
   *
   * More formal: given a substitution s, find sigma, tau, and id_Z such
   * that
   *
   *   s = sigma (tau * id_Z)
   *   sigma : V \cup Z ->
   *   tau   : -> V
   *
   * The substitution s is given in the form of two substitutions s_n
   * and s_e and a partial renaming ename of s_e's outer names. Name
   * introductions may be added to the set Y. I.e.
   *
   *   s = Y * (ename * id_Q) s_e * s_n
   *
   * where Q is the set of outer names of s_n not in the domain of
   * ename.
   *
   * The links of s_n that must pass through tau is identified by their
   * (outer) name being in L. All inner names of such links must be
   * inner names of tau.
   * 
   * Many splits may be possible so the result is a lazy list of all the
   * possible splits on the form {sigma, tau, id_Z, Y}.
   *
   * The algorithm is as follows:
   *
   * 1) let k = |L| and s_e' = (ename * id_Q) s_e
   * 2) split s_n = s_n_L * s_n_notL  where s_n_L : -> L
   * 3) lay out the links of s_n_L as        [l_L_1, ..., l_L_k]
   *    and the links of s_e' * s_n_notL as  [l_notL_1, ..., l_notL_p]
   * 4) for each subset W of V whith |W| >= |L|
   *      a) let j = |W]
   *             T = V\W
   *      b) for each ordered partition [W_1, ..., W_k] of W with |W_i| >= 1
   *           1) for each link l_L_i find all splits  l_L_i = l'_L_i tau_L_i
   *                                            where  tau_L_i : -> W_i
   *           2) for each combination of splits of the links of s_n_L 
   *                a) let tau_L   = \bigox_{i \in {1,...,k}} tau_L_i
   *                       sigma_L = \bigox_{i \in {1,...,k}} l'_L_i
   *                b) for each subset U of T
   *                     1) let S = T\U  and  tau_S = S
   *                     2) for each partition {S_1, ..., S_q} of S
   *                          a) let  Y = {y_1, ..., y_q}  where the y_i are fresh
   *                                  sigma_S = \bigox_{i \in {1,...,q}} y_i/S_i
   *                          b) for each ordered partition [U_1, ..., U_p] of U
   *                               1) for each link l_notL_i find all splits
   *                                    l_notL_i = l'_notL_i (tau_notL_i * id_Z_i)
   *                                  where  tau_notL_i : -> U_i
   *                               2) for each combination of splits of the links of s_e' * s_n_notL
   *                                    a) let tau_notL   = \bigox_{i \in {1,...,p}} tau_notL_i
   *                                           id_Z       = \bigox_{i \in {1,...,p}} id_Z_i
   *                                           sigma_notL = \bigox_{i \in {1,...,p}} l'_notL_i
   *                                    b) return {sigma = sigma_L * sigma_notL * sigma_S,
   *                                               tau   = tau_L * tau_notL * tau_S,
   *                                               id_Z  = id_Z,
   *                                               Y     = Y}
   *)
  fun horizontally_split_wiring {ename, s_e, s_n} V L =
      let
        open Wiring
        infix 5 *

        val k = NameSet.size L
        val {inCod = s_n_L, notInCod = s_n_notL} = split_outer s_n L
        val l_Ls = LinkSet.list (unmk s_n_L)
        (* Apply the partial renaming ename to each link of s_e *)
        fun link_rename l =
          case Link.unmk l of
            {outer = SOME y, inner} =>
              (case NameMap.lookup ename y of
                 SOME y' => Link.make {outer = SOME y', inner = inner}
               | NONE => l)
          | _ => l
        val l_notLs
          = (LinkSet.list (unmk s_n_notL))
            @ (map link_rename (LinkSet.list (unmk s_e)))
        val p = length l_notLs
          
        fun process_U_opartition {tau_LS, sigma_LS} Uparts =
            let
              (* Iterate through the links l_notLs and find
               * all combinations of all possible splits.
               * For each combination, calculate the corresponding
               * tau_notL * tau_LS, id_Z, and sigma_notL * sigma_LS *)
              fun link_splits [] [] tis = lzCons (fn () => (tis, lzNil))
                | link_splits
                    (l_notL_i::l_notLs) (Upart::Uparts) {tau, id_Z, sigma} =
                  let
                    (* Split and combine the remaining links recursively
                     * building tau and sigma on the way.
                     * NB! The recursive split-combinations are calculated
                     *     for each split of the current link... *)
                    fun combine_splits
                          {tau = tau_notL_i, id_Z = id_Z_i, l' = l'_notL_i} =
                        link_splits
                          l_notLs
                          Uparts
                          {tau   = tau * tau_notL_i,
                           id_Z  = id_Z * id_Z_i,
                           sigma = sigma * l'_notL_i}
                  in
                    lzconcat
                      (lzmap
                         combine_splits
                         (horizontally_split_link
                            l_notL_i
                            (NameSet.fromList Upart)))
                  end
                | link_splits _ _ _ = raise ThisCannotHappen
            in
              link_splits
                l_notLs
                Uparts
                {tau = tau_LS, id_Z = id_0, sigma = sigma_LS}
            end

        fun process_S_partition U {tau_LS, sigma_L} Sparts =
            let
              val (l_Ss, Y)
                = foldr
                    (fn (Spart, (l_Ss, Y)) =>
                       if null Spart then
                         (l_Ss, Y)
                       else
                         let
                           val y = Name.fresh NONE
                         in
                           (LinkSet.insert
                              (Link.make
                                 {outer = SOME y,
                                  inner = NameSet.fromList Spart})
                              l_Ss,
                            NameSet.insert y Y)
                         end)
                    (LinkSet.empty, NameSet.empty) Sparts
              val sigma_S = make l_Ss
              fun insert_Y {tau, id_Z, sigma} =
                  {tau = tau, id_Z = id_Z, sigma = sigma, Y = Y}
            in
              lzmap
                insert_Y
                (lzconcat
                   (lzmap
                      (process_U_opartition
                         {tau_LS = tau_LS, sigma_LS = sigma_L * sigma_S})
                      (opartgen2lzlist
                         (OrderedPartition.make (NameSet.list U) p)
                       handle OrderedPartition.NoPartitions => lzNil)))
            end

        fun process_T_subset T {tau_L, sigma_L} U =
            let
              (* the names of S = (V\W)\U must be introduced by
               * tau and substituted to fresh names by sigma.
               * the fresh names are returned in Y such that the
               * invented links are closed in matchCLO. *)
              val S = NameSet.difference T U
              val tau_S = introduce S
            in
              lzconcat
                (lzmap
                   (process_S_partition
                      U
                      {tau_LS = tau_L * tau_S, sigma_L = sigma_L})
                   (partgen2lzlist
                      (Partition.make (NameSet.list S) (NameSet.size S))
                    handle Partition.NoPartitions => lzNil))
            end

        fun process_W_opartition T Wparts =
            let
              (* Iterate through the links l_Ls and find
               * all combinations of all possible splits.
               * For each combination, calculate the
               * corresponding tau_L and sigma_L *)
              fun link_splits [] [] ts =
                  lzconcat 
                    (lzmap
                       (process_T_subset T ts)
                       (nssubsetgen2lzlist (NameSetSubset.make T)))
                | link_splits (l_L_i::l_Ls) (Wpart::Wparts) {tau_L, sigma_L} =
                  let
                    (* Split and combine the remaining links recursively
                     * building tau_L and sigma_L on the way.
                     * NB! The recursive split-combinations are calculated
                     *     for each split of the current link... *)
                    fun combine_splits {tau = tau_L_i, l' = l'_L_i} =
                        link_splits
                          l_Ls
                          Wparts
                          {tau_L   = tau_L * tau_L_i,
                           sigma_L = sigma_L * l'_L_i}
                  in
                    lzconcat
                      (lzmap
                         combine_splits
                         (horizontally_split_link'
                            l_L_i
                            (NameSet.fromList Wpart)))
                  end
                | link_splits _ _ _ = raise ThisCannotHappen
            in
              link_splits l_Ls Wparts {tau_L = id_0, sigma_L = id_0}
            end

        fun process_V_subset W =
            let
              val T = NameSet.difference V W
            in
              lzconcat
                (lzmap
                   (process_W_opartition T)
                   (opartgen2lzlist'
                      (OrderedPartition.make (NameSet.list W) k)
                    handle OrderedPartition.NoPartitions => lzNil))
            end
      in
        lzconcat
          (lzmap
             process_V_subset
             (nssubsetgen2lzlist' (NameSetSubset.make V) k))
      end

  (* Match a global discrete prime using the PAX rule:
   * If s_R_e = s_R_n = id_0 and Ps = id_(V).
   * 1) Split s_a into sigma, id_Z, and tau such that
   *      Y * ename s_a_e * s_a_n = sigma (id_Z * tau)
   *      tau : X -> V
   *    for some set of fresh names Y and such that inner names of s_a
   *    associated with L are in X.
   * 2) Return ename' = ename, Y, s_C = sigma,
   *           E = "V", qs = [(id_Z * ^tau)(X)g]
   *)
  fun matchPAX {ename,
                s_a = {s_a_e, s_a_n}, L,
                s_R = {s_R_e, s_R_n}, e = g, Ps = [P]}
    = lzmake (fn () => ((*print "PAX ";*)
      if Wiring.is_id0 s_R_e andalso Wiring.is_id0 s_R_n then
        let
          val {s, N, id_Z, Y = V, ...} = unmkP P
          val {G, ...} = unmkN N
        in
          case unmkG G of
            {Ss = [S], ...}
          => (case unmkS S of
                BgBDNF.SCon (i, a)
              => if Wiring.is_id0 id_Z andalso Wiring.is_id s
                   andalso NameSet.size V >= NameSet.size L
                 then
                   let
                     fun toPAX {tau, id_Z, sigma, Y} =
                         {ename' = ename,
                          s_C = sigma,
                          Y = Y,
                          E = makeG [makeS (SCon (i, Wiring.id_X V))],
                          qs = [makeP tau (makeN (Wiring.innernames tau) g)],
                          tree = PAX}
                   in
                     lzunmk
                       (lzmap
                          toPAX
                          (horizontally_split_wiring
                             {ename = ename, s_e = s_a_e, s_n = s_a_n}
                             V L))
                   end
                 else
                   Nil
              | _ => Nil)
          | _ => Nil
        end
      else
        Nil) handle e => raise e)
  (* Match a global discrete prime using the ZAX rule:
   * s, id_e, s |- g, id_0 ~~> g, id_0.
   * 1) Check that s_R_e = s_R_n = id_0 and Ps = []
   * 2) Compute X + Z so that g : <X + Z>
   * 3) Let s_C = s_a_n * s_a_e restricted to X + Z
   * 4) Return ename' = ename, Y = {}, s_C, E = g, qs = []
   *)
    | matchPAX {ename,
                s_a = {s_a_e, s_a_n}, L,
                s_R = {s_R_e, s_R_n}, e = g, Ps = []}
    = lzmake (fn () => ((*print "ZAX ";*)
      if Wiring.is_id0 s_R_e andalso Wiring.is_id0 s_R_n then
(* FIXME is this rignt? /Espen *)
	        LazyList.Cons 
	          ({ename' = ename,
	           s_C = Wiring.* (app_ename ename s_a_e, s_a_n),
	           Y = NameSet.empty,
	           E = g,
	           qs = [],
	           tree = ZAX},
	           lzNil)
(*        let
	        val XplusZ = Interface.glob (outerface g)
	      in
	        LazyList.Cons 
	          ({ename' = ename,
	           s_C = Wiring.* (Wiring.restrict s_a_n XplusZ,
	                           Wiring.restrict s_a_e XplusZ),
	           Y = NameSet.empty,
	           E = g,
	           qs = [],
	           tree = ZAX},
	           lzNil)
	      end
*)	    else
	      LazyList.Nil) handle e => raise e)
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

  (* Match a parallel composition:
   *  1) For each e_i : -> <1,(X_i),X_i + Y_i> determine
   *     s_a_n_i = s_a_n domainrestricted to Y_i
   *     s_a_e_i = s_a_e domainrestricted to Y_i
   *     L_i = L's intersection with the outer names of s_a_n_i
   *  2) For each Ps_i : -> <n,\vec X_i,X_i + Y_i> determine
   *     s_R_n_i = s_R_n domainrestricted to Y_i.
   *     s_R_e_i = s_R_e domainrestricted to Y_i.
   *  3) Let ename_0 = ename.
   *  4) Using ename_i, s_a_n_i, s_a_e_i, L_i, s_R_n_i, s_R_e_i,
   *     e_i, Ps_i, infer premise, 
   *     yielding ename_i+1, Y_i, s_C_i, E_i and ps_i.
   *  5) Let ename' = ename_n, compute Y by union and Es and pss by product.
   *  6) Determine names Y_a_e, Y_a_n, Y_R_e, Y_R_n
   *     introduced by (ename' s_a_e), s_a_n, s_R_e, and s_R_n, respectively.
   *  7) If Y = Y_a_e = Y_a_n = {} and Y_R_e + Y_R_n <> {}, then add a
   *     fresh name to Y.
   *  8) Construct substitution s_I : Y_R_e + Y_R_n -> Y_a_e' + Y_a_n + Y.
   *  9) Compute s_C as the extension* of s_C_i's and s_I.
   * 10) Return ename', Y, s_C, Es and pss.
   *
   * *=extension is a parallel product where inner name
   *   clash is allowed, providing such names map to the
   *   same link.  Internal edges are all "identical", causing
   *   edges in two factors to merge if they have a point
   *   in common.
   *
   * NOTE: This function allows matching
   *
   *  a = /e e/{e1, e2} (K[e1] * L[e2])
   *  R = /f f/{f1, f2} (K[f1] * L[f2])
   *
   * (which should be allowed) but not
   *
   *  a = /e e/{e1, e2}            (K[e1] * L[e2])
   *  R = /{f1,f2} (f1/f1 * f2/f2) (K[f1] * L[f2])
   *
   * (which should not be allowed) due to the threading of ename through
   * recursive calls.
   *)
  fun matchPARn
      {matchE, ename, 
       s_a = {s_a_e, s_a_n}, L, s_R as {s_R_e, s_R_n},
       es, Pss} = lzmake (fn () => ((*print "PARn ";*)
    let
      val Ys = map (glob o outerface) es
      val s_a_es = map (Wiring.restrict s_a_e) Ys
      val s_a_ns = map (Wiring.restrict s_a_n) Ys
      val Ls = map ((NameSet.intersect L) o Wiring.outernames) s_a_ns
      val Ys'
        = map (foldr (fn (Y_i, Y) => NameSet.union Y_i Y)
                       NameSet.empty
               o map (glob o outerface)) Pss
      val s_R_es = map (Wiring.restrict s_R_e) Ys'
      val s_R_ns = map (Wiring.restrict s_R_n) Ys'
      fun zipup [] [] [] [] [] [] [] = []
        | zipup (se :: ses) (sn :: sns) (L :: Ls) (Re :: Res) (Rn :: Rns)
                (e :: es) (Ps :: Pss)
        = {s_a = {s_a_e = se, s_a_n = sn}, L = L,
           s_R = {s_R_e = Re, s_R_n = Rn},
           e = e, Ps = Ps} :: zipup ses sns Ls Res Rns es Pss
        | zipup ses sns Ls Res Rns es Pss
        = raise UnequalLengths (ses, sns, Ls, Res, Rns, length es, Pss) 
      val sRePss = zipup s_a_es s_a_ns Ls s_R_es s_R_ns es Pss
      val y = Name.fresh NONE
      fun submatches mslz ({s_a = {s_a_e, s_a_n}, L,
                            s_R = {s_R_e, s_R_n}, e, Ps} :: rest)
        = let
            fun tosubmatches (ms, ename') =
              let
                val mlz = matchE {ename = ename',
                        s_a = {s_a_e = s_a_e, s_a_n = s_a_n},
                        L = L,
                        s_R = {s_R_e = s_R_e, s_R_n = s_R_n},
                        e = e,
                        Ps = Ps}
                fun addms {E, qs, s_C, Y, ename', tree}
                  = ({E = E, qs = qs, s_C = s_C, Y = Y, tree = tree} :: ms, ename')
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
      val Y_R_empty = NameSet.isEmpty Y_R_e andalso NameSet.isEmpty Y_R_n
      val Y_a_n_empty_and_Y_R_nonempty
        = NameSet.isEmpty Y_a_n andalso not Y_R_empty
      fun toPARn (matches, ename') =
        let
          val matches = rev matches
          val Es = map (fn {s_C, Y, E, qs, tree} => E) matches
          val qss = map #qs matches
          val trees = map #tree matches
          val Y = (foldr (fn (Y_i, Y) => NameSet.union Y_i Y) NameSet.empty
                   o map #Y)
                  matches
          val Y
            = if Y_a_n_empty_and_Y_R_nonempty
              andalso NameSet.isEmpty Y then
                NameSet.singleton y
              else
                Y
          val s_I
            = if Y_R_empty then
                Wiring.id_0
              else
                let
                  (* Pick an outer name from Y + Y_a_n *)
                  val theoutername
                    = case NameSet.foldUntil
                             (fn y => fn NONE => (true, SOME y) | y' => (true, y'))
                             (NameSet.foldUntil 
                                (fn y => fn NONE => (true, SOME y) | y' => (true, y'))
                                NONE Y)
                             Y_a_n of
                        SOME y => y
                      | NONE => raise ThisCannotHappen (* as Y + Y_a_n <> {} *)
                in
                  Wiring.*
                    (Wiring.make
                       (LinkSet.singleton
                          (Link.make {outer = SOME theoutername,
                                      inner = NameSet.union Y_R_e Y_R_n})),
                       Wiring.introduce
                         (NameSet.remove
                          theoutername (NameSet.union Y Y_a_n)))
                end
          val s_C_is = map #s_C matches
          val s_C = Wiring.++ (s_I :: s_C_is)
          (*val _ = map (fn w => print (Wiring.toString w ^ " ++ ")) s_C_is
          val _ = print (" = " ^ Wiring.toString s_C ^ "\n")*)
        in
          {ename' = ename, Y = Y, s_C = s_C, Es = Es, qss = qss, tree = PARn trees}
        end
      val matches = lzmap toPARn mslz
    in
      lzunmk matches
    end) handle e => raise e)

  (* Match a tensor product as a product of tensor products:
   * 1) Compute lengths n and m of agent and redex products.
   * 2) For each split of m into n parts, group Ps accordingly.
   * 3) Infer premise using the grouped Ps, yielding parameter
   *    lists of lists qss, etc.
   * 4) Concatenate the resulting qss and return the result.
   *)
  fun matchPARe {matchE, ename, s_a, L, s_R, es, Ps}
    = lzmake (fn () => ((*print "PARe ";*)
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
          fun toPARe {ename', Y, s_C, Es, qss, tree}
            = {ename' = ename', Y = Y, s_C = s_C, Es = Es,
               qs = List.concat qss, tree = PARe tree}
          val matches
            = lzmap toPARe
               (matchPARn {matchE = matchE,
                           ename = ename,
                           s_a = s_a,
                           L = L,
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
    end) handle e => raise e)

  (* Match a context permutation:
   * 1) Compute the local inner faces Xss of the redex primes Qs.
   * 2) For each permutation pi of the redex primes Qs',
   *    2a) Push pi through Qs, yielding pibar.
   *    2b) Infer premise, yielding parameter list qs etc.
   *    2c) Permute qs by pibar and return the result.
   *)
  fun matchPER {matchE, ename, s_a, L, s_R, es, Qs}
    = lzmake (fn () => ((*print "PER ";*)
    let
      val Xss = map (loc o innerface) Qs
      fun nextmatch perm =
        let
          val pi = Permutation.copy (toperm perm)
          val Qs' = permute pi Qs
          val pibar = pushthru pi Xss
          fun toPER {ename', Y, s_C, Es, qs, tree}
            = {ename' = ename', Y = Y, s_C = s_C, Es = Es, pi = pi,
               qs = permute pibar qs, tree = PER tree}
          val matches
            = lzmap toPER
               (matchPARe {matchE = matchE,
                           ename = ename,
                           s_a = s_a,
                           L = L,
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
    end) handle e => raise e)


  (* Match a global discrete prime using the MER rule:
   * 1) Deconstruct agent g into molecule list ms of length n.
	 * 2) Determine outer width r of redex Ps; let m = r + 1.
   * 3) For each partition rho(n,m) of n into m subsets
   *     3a) Construct tensor product gs of merged molecule subsets.
   *     3b) Infer premise using ename, s_a, L, s_R, es = mss, Ps,
   *         yielding ename', Y, s_C, Es, pi, qs.
   *     3c) Return ename', Y, s_C, merged Es, qs.
   *)
  fun matchMER (args as {ename, s_a, L, s_R, e = g, Ps}) =
    lzmake (fn () =>
    let (*val _ =  print "MER "*)
      val {idxmerge, Ss = ms} = unmkG g
      val m = length Ps + 1
      val rho = Partition.make ms m
      fun toMER {ename', Y, s_C, Es, pi, qs, tree} =
        {ename' = ename', Y = Y, s_C = s_C,
         E = makeG (List.concat (map (#Ss o unmkG) Es)),
         qs = qs, tree = MER tree}
      fun try rho =
        let
          val mss = Partition.next rho
          (*val _ = print ("match.sml: DEBUG: Partitioning " ^ Int.toString (length ms)
          ^ " into [ " ^ concat (map (fn ms => Int.toString (length ms) ^ " ") mss)
          ^ "].\n")*)
          val gs = map makeG mss
          val matches
            = matchPER {ename = ename, matchE = matchDG false,
                  s_a = s_a, L = L, s_R = s_R, es = gs, Qs = Ps}
        in
          lzappend (lzmap toMER matches) (try rho)
        end
      	handle Partition.NoPartitions => lzNil
    in
      lzunmk (try rho)
    end handle e => raise e)

  (* Match a global discrete prime using the ION rule, if possible:
   * If it contains 1 top-level molecule, match an ion:
   * 1) For agent ion K_yX, compute the set Y = {vec y}
   * 2) Compute s_Y_n || s_a_n_new = s_a_n where innerface s_Y_n = Y
   * 3) Compute s_Y_e || s_a_e_new = s_a_e where innerface s_Y_e = Y
   * 4) Compute L_new = L \cap (outernames s_a_n_new)
   * 5) Construct p = (id * (vec v)/(vec X))n and infer premise
   *     using s_a, L, s_R, p, and Ps, yielding
   *     ename', Y', s_C, P = (id * (vec v)/(vec Z))N, and qs
   * 6) Construct s_C = s_Y_n || s_Y_e || s_C
   *     and    ename' = ename' + {s_Y_e(y) |-> s_Y_e(y)
   *                               | y in Y n dom(s_Y_e)}
   *     and         G = (id * K_yZ)N
   * 7) Return ename', Y', s_C, G, qs
   *)
  and matchION (args as {ename, 
                         s_a as {s_a_e, s_a_n}, L, s_R, e = g, Ps})
    = lzmake (fn () => ((*print "ION ";*)
    case unmkG g of  {Ss = [s], ...} =>
      (case unmkS s of BgBDNF.SMol m =>
        let
          val {id_Z, KyX, N = n} = unmkM m
          val {ctrl, free = ys, bound = Xs} = Ion.unmk KyX
        in
          case Control.kind ctrl of
            Control.Passive => LazyList.Nil
          | _ =>
	          let
		          val Y = foldr (fn (y, Y) => NameSet.insert y Y) NameSet.empty ys
		          val {inDom = s_Y_n, notInDom = s_a_n_new} = Wiring.split s_a_n Y
		          val {inDom = s_Y_e, notInDom = s_a_e_new} = Wiring.split s_a_e Y
              val L_new = NameSet.intersect L (Wiring.outernames s_a_n_new)
		          val Y_e = NameSet.intersect Y (Wiring.innernames s_Y_e)
		          val s_Y_e_Y = Wiring.app s_Y_e Y_e handle e => raise e
		          val s_Y = Wiring.|| (s_Y_n, s_Y_e)
		          val vXs = makefreshlinks Xs
		          val p = makeP (Wiring.make' vXs) n
		          fun toION ({ename', Y = Y', s_C, E = P, qs, tree}, lzms) =
		            let
		              val {s = vZ, N, ...} = unmkP P
		              val vZs =  Wiring.unmk vZ
		              val Zs = sortlinksby vXs vZs
		              val s_C = Wiring.|| (s_Y, s_C)
		              val ename'
		                = NameSet.fold
		                    (fn y => fn ename => NameMap.add' (y, y, ename))
		                    ename'
		                    s_Y_e_Y
		              val KyZ = Ion.make {ctrl = ctrl, free = ys, bound = Zs}
		              val G = makeG [makeS (SMol (makeM KyZ N))]
		            in
		              lzCons
		                (fn () =>
		                 ({ename' = ename', Y = Y', s_C = s_C, E = G, qs = qs,
		                   tree = ION tree},
		                  lzms ()))
		            end
		            handle NameMap.DATACHANGED => lzms ()
		          val matches
		            = lzfoldr toION lzNil (matchABS
                                         {ename = ename,
                                          s_a = {s_a_n = s_a_n_new,
                                                 s_a_e = s_a_e_new},
                                          L = L_new,
                                          s_R = s_R,
                                          e = p,
                                          Ps = Ps})
		        in
		          lzunmk matches
		        end
	      end
       | _ => raise AgentNotGround (BgBDNF.unmk g, "in matchDS"))
      | _ => LazyList.Nil) handle e => raise e)
  
  (* Match a global discrete prime using a SWX, PAX, MER or ION rule:
   * 1) First
   *    if possible, return a PAX rule match,
   *    else, return any SWX rule matches,
   * 2) Then return any ION rule matches, unless PAX matched an id_0 redex
   * 3) Then if the agent contains n > 1 top-level molecules
   *    (to avoid infinite recursion via MER-PAR-PARe-PARn),
   *    return any MER rule matches.
   *)
  and matchDG allowMER (args as {ename, s_a, L, s_R, e = g, Ps}) =
    let
      val (paxswxz, paxmatch) = 
	        case lzunmk (matchPAX args) of
	          LazyList.Nil => (lzunmk (matchSWX args), false)
	        | mz as (LazyList.Cons _) => (mz, true)
    in
      lzappend
        (lzmake (fn () => paxswxz))
				(lzappend 
				  (case (paxmatch, Ps) of
				     (true, []) => lzNil
				   | _ =>  matchION args)
		    	(if allowMER then
			      (lzmake (fn () =>
		        	case #Ss (unmkG g) of
		          	(_ :: _ :: _) => lzunmk (matchMER args)
		        	| _ => LazyList.Nil))
		      else
		        lzNil))
		end

  (* Match an abstraction:
   * 1) Deconstruct p, yielding s_a_L : Z -> W and g.
   * 2) Compute s_a_n_new = s_a_L * s_a_n.
   * 3) Compute L_new = L \cup W
   * 4) Using s_a_n_new, s_a_e, L_new, s_R, g, Ps, infer premise,
   *    yielding ename', Y, s_Cnew, G, qs.
   * 5) Determine s_C_L : U -> W and s_C by outername restriction
   *    using W such that s_C * s_C_L = s_Cnew
   * 6) Construct and return ename', Y, s_C, (id * s_C_L)(U)G, and qs
   *)  
  and matchABS {ename,
                s_a = {s_a_e, s_a_n}, L, s_R as {s_R_e, s_R_n}, e = p, Ps}
    = lzmake (fn () => ((*print "ABS ";*)
    let
      val {Y = W, s = s_a_L, N = n, ...} = unmkP p
      val {absnames = Z, G = g} = unmkN n
      val W = Wiring.outernames s_a_L
      val L_new = NameSet.union L W
      val s_a_n_new = Wiring.* (s_a_L, s_a_n)
      fun toABS {ename', Y, s_C, E = G, qs, tree} =
        let
          val {inCod = s_C_L, notInCod = s_C}
            = Wiring.split_outer s_C W
          val U = Wiring.innernames s_C_L
          val P = makeP s_C_L (makeN U G)
        in
          {ename' = ename', Y = Y, s_C = s_C, E = P, qs = qs,
           tree = ABS tree}
           (* FIXME: tree might not be prime!  In that case,
              tree cannot create a match and this submatch must
              be skipped. *)
        end
      val matches
        = lzmap toABS
           (matchDG true 
                    {ename = ename,
                     s_a = {s_a_e = s_a_e, s_a_n = s_a_n_new},
                     L = L_new,
                     s_R = s_R,
                     e = g,
                     Ps = Ps})
    in
      lzunmk matches
    end) handle e => raise e)

  (* Match a closure:
   * 1) Open w_a, yielding s_a = s_a_e * s_a_n
   * 2) Open w_R, yielding s_R = s_R_e * s_R_n and fresh outer names Y_R of s_R_e
   * 3) Using s_a, s_R, infer premise,
   *    yielding ename', Y, s_C, Qs, pi, qs,
   *    where ename' maps to range Y_R + Y_C
   * 4) Check that s_C = id_{Y_R} * s'_C
   * 5) Return a new w_C as s'_C where links Y_C are closed.
   *)
  fun matchCLO {w_a, w_R, ps, Ps} = lzmake (fn () => ((*print "CLO ";*)
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
                    L = NameSet.empty,
                    s_R = {s_R_e = s_R_e, s_R_n = s_R_n},
                    es = ps,
                    Qs = Ps}
      val is_id_Y_R_x_sigma = Wiring.is_id_x_sigma Y_R
      fun toCLO ({ename', Y, s_C, Es = Qs, pi, qs, tree}, rest) =
        if is_id_Y_R_x_sigma s_C then
           let
             val Y_C = NameSet.difference 
                       (NameSet.fromList (NameMap.range ename')) Y_R
            val w_C = closelinks Y_C s_C
          in
            lzCons
              (fn () =>
                ({w_C = w_C, Qs = Qs, pi = pi, qs = qs, tree = CLO tree},
                  rest ()))
          end
        else
          rest ()
    in
      lzunmk (lzfoldr toCLO lzNil matches)
    end) handle e => raise e)
    
  fun matches {agent, rule} = lzmake (fn () =>
    let
      val _ = if width (innerface agent) > 0 then
                raise
                  AgentNotGround
                    (BgBDNF.unmk agent, "in matches")
              else
                ()
      val {name, redex, react, inst} = Rule.unmk rule
      val {wirxid = w_axid, D = D_a} = unmkBR agent
      val ps = #Ps (unmkDR D_a)
      val {wirxid = w_Rxid, D = D_R} = unmkBR redex
      val Ps = #Ps (unmkDR D_R)
      val Y = (glob o outerface) redex
      fun toMatch {w_C, Qs, pi, qs, tree} =
        let
          val Xs = map (hd o loc o outerface) Qs
          val Y
            = foldr
                (fn (Y', Y) => NameSet.union Y' Y)
                Y
                (map (glob o outerface) qs)
        in
          {context
            = BgBDNF.makeB w_C Xs (BgBDNF.makeD (Wiring.id_X Y) Qs pi)handle e=>raise e,
           rule = rule,
           parameter = BgBDNF.makeDR Wiring.id_0 qs,
           tree = tree}
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
    end handle e => raise e)
    
    (*********************************)
    (*                               *)
    (* MAIN MATCHING ENTRY HERE!     *)
    (*                               *)
    (*********************************)

  fun amatch agentredex =
    case lzunmk (matches agentredex) of
      Nil => NONE
    | Cons (m, ms) => SOME m

  val allmatches : {agent : BR bgbdnf, rule : rule}
                 -> match list = lztolist o matches
  
  fun ppTree indent pps tree =
  let
    open PrettyPrint
	  val show = add_string pps
	  fun << () = begin_block pps CONSISTENT 0
	  fun <<<() = begin_block pps INCONSISTENT 0
	  fun >> () = end_block pps
	  fun brk () = add_break pps (0, indent)
	  fun brk0() = add_break pps (0, 0)
	  fun showtrees [] = ()
      | showtrees [tree] = ppTree' tree	    
	    | showtrees (tree :: trees)
	    = (ppTree' tree; show ","; brk0(); showtrees trees)
	  and ppTree' PAX' = show "PAX'"
	    | ppTree' ZAX' = show "ZAX'"
	    | ppTree' (ION' tree)
	    = (<<(); show "ION'("; brk(); ppTree' tree; brk0(); show ")"; >>())
	    | ppTree' (PARn' trees)
	    = (<<(); show "PARn'("; brk();
	      <<<(); showtrees trees; >>();
	      brk0(); show ")"; >>())
	    | ppTree' (PARe' tree)
	    = (<<(); show "PARe'("; brk(); ppTree' tree; brk0(); show ")"; >>())
	    | ppTree' (PER' tree)
	    = (<<(); show "PER'("; brk(); ppTree' tree; brk0(); show ")"; >>())
	    | ppTree' (MER' tree)
	    = (<<(); show "MER'("; brk(); ppTree' tree; brk0(); show ")"; >>())
	    | ppTree' (ABS' tree)
	    = (<<(); show "ABS'("; brk(); ppTree' tree; brk0(); show ")"; >>())
	    | ppTree' (SWX tree)
	    = (<<(); show "SWX("; brk(); ppTree' tree; brk0(); show ")"; >>())
	    | ppTree' PAX = show "PAX"
	    | ppTree' ZAX = show "ZAX"
	    | ppTree' (ION tree)
	    = (<<(); show "ION("; brk(); ppTree' tree; brk0(); show ")"; >>())
	    | ppTree' (PARn trees)
	    = (<<(); show "PARn("; brk();
	      <<<(); showtrees trees; >>();
	      brk0(); show ")"; >>())
	    | ppTree' (PARe tree)
	    = (<<(); show "PARe("; brk(); ppTree' tree; brk0(); show ")"; >>())
	    | ppTree' (PER tree)
	    = (<<(); show "PER("; brk(); ppTree' tree; brk0(); show ")"; >>())
	    | ppTree' (MER tree)
	    = (<<(); show "MER("; brk(); ppTree' tree; brk0(); show ")"; >>())
	    | ppTree' (ABS tree)
	    = (<<(); show "ABS("; brk(); ppTree' tree; brk0(); show ")"; >>())
	    | ppTree' (CLO tree)
	    = (<<(); show "CLO("; brk(); ppTree' tree; brk0(); show ")"; >>())
  in
    ppTree' tree
  end  
  
  fun pp0 showTree ppBBDNF ppDRBDNF indent pps
          ({context, rule, parameter, tree} : match) =
    let
      open PrettyPrint
      val show = add_string pps
      fun << () = begin_block pps CONSISTENT 0
      fun >> () = end_block pps
      fun brk () = add_break pps (1, 1)
    in
      <<();
      show "{";
      <<(); show "context"; brk(); show "= ";
      ppBBDNF indent pps context;
      show ","; >>(); brk();
      <<(); show "parameter"; brk(); show "= ";
      ppDRBDNF indent pps parameter;
      (if showTree then
         (show ","; >>(); brk();
          <<(); show "tree"; brk(); show "= ";
          ppTree indent pps tree; >>())
       else
         >>());
      show "}";
      >>()
    end

  fun pp' indent = pp0 false indent

  val pp = pp' BgBDNF.pp BgBDNF.pp

  val ppWithTree = pp0 true BgBDNF.pp BgBDNF.pp

  val _ = Flags.makeIntFlag
            {name = "/misc/linewidth",
             default = 72,
             short = "w", long = "line-width",
             arg = "W",
             desc = "Set line width to W characters"}
    
  val _ = Flags.makeIntFlag
            {name = "/misc/indent",
             default = 2,
             short = "", long = "indent",
             arg = "N",
             desc = "Set extra indentation at each level when prettyprinting to N"}
             
  val treeToString
    = PrettyPrint.pp_to_string
        (Flags.getIntFlag "/misc/linewidth") 
        (ppTree (Flags.getIntFlag "/misc/indent"))

  val toString
    = PrettyPrint.pp_to_string
        (Flags.getIntFlag "/misc/linewidth") 
        (pp (Flags.getIntFlag "/misc/indent"))

  fun toString' ppBBDNF ppDRBDNF
    = PrettyPrint.pp_to_string
        (Flags.getIntFlag "/misc/linewidth") 
        (pp' ppBBDNF ppDRBDNF (Flags.getIntFlag "/misc/indent"))

  val toStringWithTree
    = PrettyPrint.pp_to_string
        (Flags.getIntFlag "/misc/linewidth") 
        (ppWithTree (Flags.getIntFlag "/misc/indent"))

  fun toStringWithTree' ppBBDNF ppDRBDNF
    = PrettyPrint.pp_to_string
        (Flags.getIntFlag "/misc/linewidth") 
        (pp0 true ppBBDNF ppDRBDNF (Flags.getIntFlag "/misc/indent"))
val revision
  = hd (String.tokens (not o Char.isDigit) "$LastChangedRevision$")
end


functor Match (
  structure Info        : INFO
  structure Name        : NAME
  structure NameMap     : MONO_FINMAP
  structure Link        : LINK
  structure LinkSet     : MONO_SET
  structure Control     : CONTROL
  structure Permutation : PERMUTATION
  structure Wiring      : WIRING
  structure Ion         : ION
  structure Interface   : INTERFACE
  structure BgVal       : BGVAL
  structure BgBDNF      : BGBDNF
  structure Rule        : RULE
  structure NameSet     : MONO_SET
  structure ErrorHandler : ERRORHANDLER
      where type origin = Origin.origin
        and type ppstream = PrettyPrint.ppstream
  sharing type Permutation.nameset = NameSet.Set
  sharing type Permutation.permutation = BgBDNF.permutation
  sharing type Wiring.wiring = BgVal.wiring
                             = BgBDNF.wiring
  sharing type Ion.control = Control.control
  sharing type Ion.ion = BgVal.ion = BgBDNF.ion
  sharing type NameSet.Set = Link.nameset
                           = Wiring.nameset
                           = Interface.nameset
                           = BgBDNF.nameset
                           = BgVal.nameset
                           = Ion.nameset
  sharing type Name.name = Ion.name
                         = Link.name
                         = Wiring.name
                         = NameSet.elt
                         = BgVal.name
  sharing type NameMap.dom = Name.name
  sharing type NameMap.map = Wiring.namemap
  sharing type LinkSet.Set = Wiring.linkset
  sharing type LinkSet.elt = Link.link
                           = Wiring.link
  sharing type Interface.interface = BgBDNF.interface
  sharing type BgVal.bgval = BgBDNF.bgval
  sharing type BgBDNF.bgbdnf = Rule.bgbdnf
  sharing type BgBDNF.BR = Rule.BR
  sharing type BgVal.bgmatch = BgBDNF.bgmatch
  sharing type Info.info =
               BgBDNF.info =
               BgVal.info
  ) :> MATCH
  where type info            = Info.info
    and type 'class bgbdnf   = 'class BgBDNF.bgbdnf
    and type BR              = BgBDNF.BR
    and type DR              = BgBDNF.DR
    and type nameset         = NameSet.Set =
struct
  structure Match = Match'(structure Info = Info
			   structure Name = Name
			   structure NameMap = NameMap
			   structure Link = Link
			   structure LinkSet = LinkSet
			   structure Control = Control
			   structure Permutation = Permutation
			   structure Wiring = Wiring
			   structure Ion = Ion
			   structure Interface = Interface
			   structure BgVal = BgVal
			   structure BgBDNF = BgBDNF
         structure Rule = Rule
			   structure NameSet = NameSet
			   structure ErrorHandler = ErrorHandler)
  open Match
end