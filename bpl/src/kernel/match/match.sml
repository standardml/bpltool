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

  structure Partition = Partition(structure LazyList = LazyList)

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
   of wiring list * wiring list * wiring list * wiring list * int * P bgbdnf list list


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


  (* Match a global discrete prime g to a context G using the PAX rule:
   * 1) Deconstruct context G = "alpha" : (X + U) -> W.
   * 2) Find X + Z = rg(g).
   * 3) Compute X, Z and U from (X + U) and (X + Z).
   * 4) Let ename' = ename, s_C' = {}, Y = {}.
   * 5) For each x in W + Z,
   *    5a) If x in W then x' = alpha^-1(x) else x' = x.
   *    5b) If x' in X + Z then
   *          If x in dom(s_C_e) then
   *            check/adjust ename' so that ename' s_a_e(x') = s_C_e(x).
   *          else
   *            if x in dom(s_C_n) then x'' = s_C_n(x) else x'' = x.
   *            if x in dom(s_a_e) then
   *              check/adjust ename' and s_C' so that
   *                ename' s_a_e(x') = s_C'(x'')
   *            else
   *              check/adjust s_C' so that s_a_n(x') = s_C'(x'').
   *        else (* x' in U (implying x in W) *)
   *          if x in dom(s_C_e) then
   *            let y = s_C_e(x)
   *            if y not in rg(ename') then add y to Y.
   *          else
   *            if x in dom(s_C_n) then x'' = s_C_n(x) else x'' = x.
   *            let y = s_C'(x''),
   *              if necessary by letting y be fresh and adjusting s_C'.
   *            if y not in (rg(ename') + rg(s_a_n)) then add y to Y.
   * 6) Return ename', s_C', Y, qs = [(id_Z * (U + X)(U/ * "X"))(X)g].
   *)
  fun matchPAX' {ename, s_a as {s_a_e, s_a_n}, s_C as {s_C_e, s_C_n}, g, G} =
      lzmake (fn () => ((*print "PAX' ";*)
      case #Ss (unmkG G) of
        [S] =>
      (case unmkS S of
        BgBDNF.SCon (i, a) =>
        (let
          val XU = Wiring.innernames a
          val XZ = Interface.glob (outerface g)
          val X  = NameSet.intersect XU XZ
          val Z  = NameSet.difference XZ X
          val U  = NameSet.difference XU X
          val W  = Wiring.outernames a
         
          fun match_name isinW x (ename', rg_ename', s_C', rg_s_C', Y) =
              let
                val x' = if isinW then Wiring.app_renaming_inverse_x a x else x
              in
                if NameSet.member x' XZ then
                  if Wiring.in_domain x s_C_e then
                    let
                      val s_C_e_x   = Wiring.app_renaming_x s_C_e x  handle e => raise e
                      val s_a_e_x'  = Wiring.app_renaming_x s_a_e x' handle e => raise e
                      val ename'    = check_adjust' ename' s_a_e_x' s_C_e_x
                      val rg_ename' = NameSet.insert' s_C_e_x rg_ename'
                    in
                      (ename', rg_ename', s_C', rg_s_C', Y)
                    end
                  else
                    let
                      val x'' = if Wiring.in_domain x s_C_n then
                                  Wiring.app_renaming_x s_C_n x
 handle e => raise e                                else
                                  x
                    in
                      if Wiring.in_domain x s_a_e then
                        let
                          val s_a_e_x' = Wiring.app_renaming_x s_a_e x' handle e => raise e
                          val (ename', s_C')
                            = check_adjust ename' s_C' s_a_e_x' x''
                          val rg_ename'
                            = NameSet.insert'
                                (valOf (NameMap.lookup ename' s_a_e_x'))
                                rg_ename'
                          val rg_s_C'
                            = NameSet.insert'
                                (valOf (NameMap.lookup s_C' x''))
                                rg_s_C'
                        in
                          (ename', rg_ename', s_C', rg_s_C', Y)
                        end
                      else
                        let
                          val s_a_n_x' = Wiring.app_renaming_x s_a_n x' handle e => raise e
                          val s_C'     = check_adjust' s_C' x'' s_a_n_x'
                          val rg_s_C'  = NameSet.insert' s_a_n_x' rg_s_C'
                        in
                          (ename', rg_ename', s_C', rg_s_C', Y)
                        end
                    end
                else
                  if Wiring.in_domain x s_C_e then
                    let
                      val y = Wiring.app_renaming_x s_C_e x
                    in
                      if not (NameSet.member y rg_ename') then
                        (ename', rg_ename', s_C', rg_s_C', NameSet.insert y Y)
                      else
                        (ename', rg_ename', s_C', rg_s_C', Y)
                    end
                  else
                    let
                      val x'' = if Wiring.in_domain x s_C_n then
                                  Wiring.app_renaming_x s_C_n x
                                else
                                  x
                      val (s_C', y, rg_s_C')
                        = if NameMap.inDomain x'' s_C' then
                            (s_C', valOf (NameMap.lookup s_C' x''), rg_s_C')
                          else
                            let
                              val y = Name.fresh NONE
                              val rg_s_C' = NameSet.insert' y rg_s_C'
                            in
                              (NameMap.add (x'', y, s_C'), y, rg_s_C')
                            end
                    in
                      if (not (NameSet.member y rg_ename'))
                        andalso (not (Wiring.in_codomain y s_a_n)) then
                        (ename', rg_ename', s_C', rg_s_C', NameSet.insert y Y)
                      else
                        (ename', rg_ename', s_C', rg_s_C', Y)
                    end
              end

          val rg_ename = NameSet.fromList (NameMap.range ename)

          val (ename', rg_ename', s_C', rg_s_C', Y)
            = NameSet.fold
                (match_name false)
                (NameSet.fold
                   (match_name true)
                   (ename, rg_ename, NameMap.empty, NameSet.empty, NameSet.empty)
                   W)
                Z
        in
          Cons ({ename' = ename',
                 s_C' = Wiring.make_ren s_C',
                 Y = Y,
                 qs = [makeP (Wiring.* ((Wiring.introduce U), (Wiring.id_X X)))
                             (makeN X g)],
                 tree = PAX'}, lzNil)
        end
        handle NoMatch => Nil)
      | _ => Nil)
  (* Match a global discrete prime g to a context G using the ZAX rule:
   *   Y, id_e, Y |- 1, [[id_0]]^P ~~> 1, [[id_0]]^P
   * 1) Check that g = 1 and G = 1.
   * 2) Return ename' = ename, s_C' = {}, qs = [].
   *)
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
    end)

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

          fun make_match {ename', s_C', Y, qs, tree} =
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
                      (ename', Wiring.unmk_ren s_C') (ys, us)
                    handle ListPair.UnequalLengths => raise NoMatch
              in
                {ename' = ename',
                 s_C' = Wiring.make_ren s_C',
                 Y = Y,
                 qs = qs,
                 tree = ION' tree}
              end
        in
          lzunmk (lzmap make_match premise_matches)
        end
        handle NoMatch => Nil)
      | _ => Nil)
      | _ => Nil)
      | _ => Nil)
      | _ => Nil))

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
      end)

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
      end)
  
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
      end)
  
  (* Match a global discrete prime using the SWX rule:
   * If Ps = [P], where P = (id_Z * ^s)(W)G, then
   * 1) Let s_C_e = s_R_e, s_C_n = s * s_R_n, and infer premise
   *    using s_a_e, s_a_n, s_C_e, s_C_n, g, G,
   *    yielding ename', s_C', Y, and qs
   * 2) Let Y_C_e = outernames s_C_e
   * 3) Let U = outernames s
   * 4) Return ename', Y, s_C := s_C' * id_Y_C_e, G := "U", qs
   *)
  fun matchSWX {ename, 
                s_a = {s_a_e, s_a_n}, 
                s_R = {s_R_e, s_R_n}, e = g, Ps = [P]}
    = lzmake (fn () => ((*print "SWX ";*)
      let
        val {id_Z, Y = U, s, X = W, N} = unmkP P
        val {absnames = W, G} = unmkN N
        val s_C_e = s_R_e
        val s_C_n = Wiring.* (s, s_R_n)
        val id_Y_C_e = Wiring.id_X (Wiring.outernames s_C_e)
        val i = BgBDNF.info P
        fun toSWX {ename', s_C', Y, qs, tree} = ((*print ("SWX: s_C'=" ^ Wiring.toString s_C'
        ^ "\ns_R_e=" ^ Wiring.toString s_R_e
        ^ "\ns_R_n=" ^ Wiring.toString s_R_n
        ^ "\nP=" ^ BgBDNF.toString P
        ^ "\nqs=[" ^ concat (map (fn q => BgBDNF.toString q ^ "\n") qs) ^ "]\n");*)
          {ename' = ename',
           Y = Y,
           s_C = Wiring.* (s_C', id_Y_C_e),
           E = makeG [makeS (SCon (i, Wiring.id_X U))],
           qs = qs,
           tree = SWX tree})
        val matches
          = lzmap toSWX
                  (matchDG' true 
                            {ename = ename,
                             s_a = {s_a_e = s_a_e,
                                    s_a_n = s_a_n},
                             s_C = {s_C_e = s_C_e, s_C_n = s_C_n},
                             g = g, G = G})
      in
        lzunmk matches
      end))
    | matchSWX _ = lzNil

  (* Match a global discrete prime using the PAX rule:
   * If s_R_e = s_R_n = id_0 and
   * Ps = id_(X) for some X subset of g's outer names, then
   * 1) Let X+Z = outernames (g)
   * 2) Inner-name restrict s_a to X+Z
   * 3) Let s_C = s_a_n * s_a_e
   * 4) Return ename' = ename, Y = {}, s_C, E = "X", qs = [(X)g]
   *)
  fun matchPAX {ename,
                s_a = {s_a_e, s_a_n},
                s_R = {s_R_e, s_R_n}, e = g, Ps = [P]}
    = lzmake (fn () => ((*print "PAX ";*)
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
                          ({ename' = ename,
                           s_C = Wiring.* (Wiring.restrict s_a_n XplusZ,
                                            Wiring.restrict s_a_e XplusZ),
                           Y = NameSet.empty,
                           E = makeG [makeS (SCon (i, alpha))],
                           qs = [makeP (Wiring.id_X XplusZ)
                                       (makeN X g)],
                           tree = PAX},
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
        LazyList.Nil))
  (* Match a global discrete prime using the ZAX rule:
   * s, id_e, s |- g, id_0 ~~> g, id_0.
   * 1) Check that s_R_e = s_R_n = id_0 and Ps = []
   * 2) Compute X + Z so that g : <X + Z>
   * 3) Let s_C = s_a_n * s_a_e restricted to X + Z
   * 4) Return ename' = ename, Y = {}, s_C, E = g, qs = []
   *)
    | matchPAX {ename,
                s_a = {s_a_e, s_a_n},
                s_R = {s_R_e, s_R_n}, e = g, Ps = []}
    = lzmake (fn () => ((*print "ZAX ";*)
      if Wiring.is_id0 s_R_e andalso Wiring.is_id0 s_R_n then
        let
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
	    else
	      LazyList.Nil))
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
   *     s_a_e_i = s_a_e domainrestricted to Y_i.
   *  2) For each Ps_i : -> <n,\vec X_i,X_i + Y_i> determine
   *     s_R_n_i = s_R_n domainrestricted to Y_i.
   *     s_R_e_i = s_R_e domainrestricted to Y_i.
   *  3) Let ename_0 = ename.
   *  4) Using ename_i, s_a_n_i, s_a_e_i, s_R_n_i, s_R_e_i,
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
       s_a = {s_a_e, s_a_n}, s_R as {s_R_e, s_R_n},
       es, Pss} = lzmake (fn () => ((*print "PARn ";*)
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
    end))

  (* Match a tensor product as a product of tensor products:
   * 1) Compute lengths n and m of agent and redex products.
   * 2) For each split of m into n parts, group Ps accordingly.
   * 3) Infer premise using the grouped Ps, yielding parameter
   *    lists of lists qss, etc.
   * 4) Concatenate the resulting qss and return the result.
   *)
  fun matchPARe {matchE, ename, s_a, s_R, es, Ps}
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
    end))

  (* Match a context permutation:
   * 1) Compute the local inner faces Xss of the redex primes Qs.
   * 2) For each permutation pi of the redex primes Qs',
   *    2a) Push pi through Qs, yielding pibar.
   *    2b) Infer premise, yielding parameter list qs etc.
   *    2c) Permute qs by pibar and return the result.
   *)
  fun matchPER {matchE, ename, s_a, s_R, es, Qs}
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
    end))


  (* Match a global discrete prime using the MER rule:
   * 1) Deconstruct agent g into molecule list ms of length n.
	 * 2) Determine outer width r of redex Ps; let m = r + 1.
   * 3) For each partition rho(n,m) of n into m subsets
   *     3a) Construct tensor product gs of merged molecule subsets.
   *     3b) Infer premise using ename, s_a, s_R, es = mss, Ps,
   *         yielding ename', Y, s_C, Es, pi, qs.
   *     3c) Return ename', Y, s_C, merged Es, qs.
   *)
  fun matchMER (args as {ename, s_a, s_R, e = g, Ps}) =
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
                  s_a = s_a, s_R = s_R, es = gs, Qs = Ps}
        in
          lzappend (lzmap toMER matches) (try rho)
        end
      	handle Partition.NoPartitions => lzNil
    in
      lzunmk (try rho)
    end)

  (* Match a global discrete prime using the ION rule, if possible:
   * If it contains 1 top-level molecule, match an ion:
   * 1) For agent ion K_yX, compute the set Y = {vec y}
   * 2) Compute s_Y_n || s_a_n_new = s_a_n by domain-restricting
   *     s_a_n to Y
   * 3) Compute s_Y_e || s_a_e_new = s_a_e by domain-restricting
   *     s_a_e to Y
   * 4) Construct p = (id * (vec v)/(vec X))n and infer premise
   *     using s_a, s_R, p, and Ps, yielding
   *     ename', Y', s_C, P = (id * (vec v)/(vec Z))N, and qs
   * 5) Construct s_C = s_Y_n || s_Y_e || s_C
   *     and    ename' = ename' + {s_Y_e(y) |-> s_Y_e(y)
   *                               | y in Y n dom(s_Y_e)}
   *     and         G = (id * K_yZ)N
   * 6) Return ename', Y', s_C, G, qs
   *)
  and matchION (args as {ename, 
                         s_a as {s_a_e, s_a_n}, s_R, e = g, Ps})
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
		          val s_Y_n = Wiring.restrict s_a_n Y
		          val s_Y_e = Wiring.restrict s_a_e Y
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
		            = lzfoldr toION lzNil (matchABS {ename = ename,
		                                     s_a = s_a,
		                                     s_R = s_R,
		                                     e = p,
		                                     Ps = Ps})
		        in
		          lzunmk matches
		        end
	      end
       | _ => raise AgentNotGround (BgBDNF.unmk g, "in matchDS"))
      | _ => LazyList.Nil))
  
  (* Match a global discrete prime using a SWX, PAX, MER or ION rule:
   * 1) First
   *    if possible, return a PAX rule match,
   *    else, return any SWX rule matches,
   * 2) Then return any ION rule matches, unless PAX matched an id_0 redex
   * 3) Then if the agent contains n > 1 top-level molecules
   *    (to avoid infinite recursion via MER-PAR-PARe-PARn),
   *    return any MER rule matches.
   *)
  and matchDG allowMER (args as {ename, s_a, s_R, e = g, Ps}) =
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
   * 3) Using s_a_n_new, s_a_e, s_R, g, Ps, infer premise,
   *    yielding ename', Y, s_Cnew, G, qs.
   * 4) Determine s_C_L : U -> W and s_C by outername restriction
   *    using W such that s_C * s_C_L = s_Cnew
   * 5) Construct and return ename', Y, s_C, (id * s_C_L)(U)G, and qs
   *)  
  and matchABS {ename,
                s_a = {s_a_e, s_a_n}, s_R as {s_R_e, s_R_n}, e = p, Ps}
    = lzmake (fn () => ((*print "ABS ";*)
    let
      val {Y = W, s = s_a_L, N = n, ...} = unmkP p
      val {absnames = Z, G = g} = unmkN n
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
        end
      val matches
        = lzmap toABS
           (matchDG true 
                    {ename = ename,
                     s_a = {s_a_e = s_a_e, s_a_n = s_a_n_new},
                     s_R = s_R,
                     e = g,
                     Ps = Ps})
    in
      lzunmk matches
    end))

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
    end))
    
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