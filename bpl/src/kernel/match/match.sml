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
 * information, see <ul><li>
 * Birkedal, Damgaard, Glenstrup, and Milner: <em>
 * Matching of bigraphs.</em>
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
  val unmkBR = BgBDNF.unmkBR
  val unmkDR = BgBDNF.unmkDR
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
   * All rules except CLO take 3 substitutions s_a', s_a, s_R
   * as well as other arguments, and return something, as well as 
   * s_C and s_a'', the latter being an outer-name-extended and renamed
   * version of s_a', i.e., s_a'' = Y/ x alpha s_a'.
   * The substitution in the theoretical note is
   * sigma^a = s_a'' x s_a.
   *)

  fun matchDS s_a' s_a s_R e Ps = lzNil
  
  fun matchDP s_a' s_a s_R e Ps = lzNil

  datatype primetype = DiscretePrime | GlobalDiscretePrime

  (* Match a parallel composition:
   * 1) For each e_i : -> <1,(X_i),X_i + Y_i> determine
   *    s_a_i = s_a domainrestricted to Y_i
   *    s_a'_i = s_a' domainrestricted to Y_i.
   * 2) For each Ps_i : -> <n,\vec X_i,X_i + Y_i> determine
   *    s_R_i = s_R domainrestricted to Y_i.
   * 3) Using s_a_i, s_a'_i, s_R_i, e_i, Ps_i, infer premise, 
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
   *   same link.  Internal edges with identical point
   *   sets are considered "the same link."
   *)
  fun matchPARn E s_a' s_a s_R es Pss = lzmake (fn () =>
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
      fun submatches
          (s_a' :: s_a's) (s_a :: s_as) (s_R :: s_Rs)
          (e :: es) (Ps :: Pss)
        = match s_a' s_a s_R e Ps
           :: submatches s_a's s_as s_Rs es Pss
        | submatches [] [] [] [] [] = []
        | submatches s_a's s_as s_Rs es Pss
        = raise UnequalLengths (s_a's, s_as, s_Rs, length es, Pss)
      val Y_a = Wiring.introductions s_a
      val Y_R = Wiring.introductions s_R
      val Y_a_empty_and_Y_R_nonempty
        = NameSet.isEmpty Y_a andalso not (NameSet.isEmpty Y_R)
      fun toPARn (matches : {E : 'a bgbdnf, ps : P bgbdnf list,
                             s_C : wiring, s_a'' : wiring} list) =
        let
          val s_a'' = Wiring.||| (map #s_a'' matches)
          val Es = map #E matches
          val pss = map #ps matches
          val Y_a'' = Wiring.introductions s_a''
          val Y_a''
            = if Y_a_empty_and_Y_R_nonempty
              andalso NameSet.isEmpty Y_a'' then
                NameSet.singleton (Name.make "x" (* SHOULD REALLY BE: afreshname () *)) 
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
               | NONE => raise ThisCannotHappen (* Y_a'' + Y_a <> {} *)
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
          {s_a'' = s_a'', s_C = s_C, Es = Es, pss = pss}
        end
      val matches
        = lzmap toPARn
            (lzcombine (submatches s_a's s_as s_Rs es Pss))
    in
      lzunmk matches
    end)

  fun matchPARe E s_a' s_a s_R es Ps = lzmake (fn () =>
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
	        fun toPARe {s_a'', s_C, Es, pss}
	          = {s_a'' = s_a'', s_C = s_C, Es = Es,
	             qs = List.concat pss}
    	    val matches
	          = lzmap toPARe (matchPARn E s_a' s_a s_R es P'ss)
          fun next () = lzunmk (nextmatch
                                (nextsplit split)
                                handle NoMoreSplits => lzNil)
	      in
	        lzappend matches (lzmake next)
	      end
	  in
	    lzunmk (nextmatch (firstsplit m n))
	  end)

  fun matchPER E s_a' s_a s_R es Qs = lzmake (fn () =>
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
            = lzmap toPER (matchPARe E s_a' s_a s_R es Qs')
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
   * 1) Open w_a, yielding s_a' x s_a
   * 2) Open w_R, yielding s_R and fresh outer names Y_R of s_R
   * 3) Using s_a', s_a, s_R, infer premise,
   *    yielding s_a'', s_C, Qs, pi, qs,
   *    where s_a'' = Y/ x alpha s_a' has outer names Y_R + Y_C
   * 4) Check that s_C = id_{Y_R} x s'_C
   * 5) Return a new w_C as s'_C where links Y_C are closed.
   *)
  fun matchCLO (w_a : Wiring.wiring) w_R ps Ps = lzmake (fn () =>
    let
      open Wiring
    	val {opened = s_a', rest = s_a, usednames} = splitopen NameSet.empty w_a
    	val {opened = s_R, newnames = Y_R, ...} = openup usednames w_R
	    val matches = matchPER DiscretePrime s_a' s_a s_R ps Ps
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
