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

(** Abstract data type for bigraph binding discrete normal forms (BDNF).
 * @version $Revision: 1.22 $
 *)
functor BgBDNF (type info
		structure Name : NAME
		structure NameSet : MONO_SET
		structure LinkSet : MONO_SET
		structure Interface : INTERFACE
		structure Ion : ION
		structure Permutation : PERMUTATION
		structure Link : LINK
		structure Wiring : WIRING
		structure BgVal : BGVAL
		structure PrettyPrint : PRETTYPRINT
		sharing type BgVal.interface = 
			     Interface.interface =
			     Permutation.interface
		sharing type BgVal.ion = Ion.ion
		sharing type BgVal.permutation =
			     Permutation.permutation
		sharing type BgVal.wiring = Wiring.wiring
		sharing type NameSet.Set =
			     Interface.nameset =
			     Ion.nameset =
			     Permutation.nameset =
			     Link.nameset =
			     Wiring.nameset =
			     BgVal.nameset
		sharing type LinkSet.Set = Wiring.linkset
		sharing type LinkSet.elt =
                             Link.link =
                             Wiring.link
		sharing type Link.name = NameSet.elt
		sharing type NameSet.elt =
                             Name.name =
                             Ion.name
                sharing type PrettyPrint.ppstream =
			     Wiring.ppstream =
			     BgVal.ppstream
			     ) :> BGBDNF 
  where type nameset = NameSet.Set
    and type info = BgVal.info 
    and type bgval = BgVal.bgval 
    and type bgmatch = BgVal.bgmatch
    and type ppstream = BgVal.ppstream =
struct
  open BgVal

  (* NOTE: Each BDNF form has a fixed structure (omitting infos and
   * interfaces) except for the SBDNF which has two possible structures:
   * 
   * M BDNF: Com (Ten [Wir w, Ion i], N)
   * S BDNF: Com (Ten [Wir a, Per id_1], Con X)
   *     or  M BDNF
   * G BDNF: Com (Ten [Wir w, Mer n], Ten [S_0, ..., S_n-1])
   * N BDNF: Abs (X, G)
   * P BDNF: Com (Ten [Wir w, Abs (Y, Com (Ten [Wir w, Per id_1],
   *                                       Con X))],
   *              N)
   * D BDNF: Ten [Wir w, Com (Ten [P_0, ..., P_n-1], Per pi)]
   * B BDNF: Com (Ten [Wir w, Per id_X], D)
   *
   *
   * The regularized bigraph representation differ only for D RBDNF:
   *
   * D RBDNF: Ten [Wir w, Ten [P_0, ..., P_n-1]]
   *)

  (* FIXME: document representations of notational shorthands
   *        (e.g. (y)/(X))
   * 
   *)
  (* FIXME: naming conventions should be documented (e.g. vwid_0)
   * 
   * 
   *)

  (** M BDNF molecule class phantom type. *)
  type M = unit
  (** S BDNF singular top-level node class phantom type. *)
  type S = unit
  (** G BDNF global discrete prime class phantom type. *)
  type G = unit
  (** N BDNF name-discrete prime class phantom type. *)
  type N = unit
  (** P BDNF discrete prime class phantom type. *)
  type P = unit
  (** D BDNF discrete bigraph class phantom type. *)
  type D = unit
  (** B BDNF general bigraph class phantom type. *)
  type B = unit
  (** The bgbdnf data type.  'class must be M, S, G, N, P, D or B. *)
  type 'class bgbdnf = bgval
  (** The bgrbdnf data type.  'class must be D or B. *)
  type 'class bgrbdnf = bgval
  (** Sum type for singular top-level nodes. *)
  datatype stlnode =
           SCon of bgval
         | SMol of M bgbdnf
  (** Signal that a BDNF does not represent a regular bigraph.
   * @params file i b errtxt
   * @param file    the file name in which the exception was raised.
   * @param b       the bigraph
   * @param errtxt  explanatory error text.
   *)
  exception IrregularBDNF of string * info * bgval * string
  (** Signal that some term was not expected not to be BDNF.
   * @params file b errtxt
   * @param file    the file name in which the exception was raised.
   * @param b       the term that caused the error.
   * @param errtxt  explanatory error text.
   *)
  exception MalformedBDNF of string * info * bgmatch * string
  (** Signal that some term was not expected not to be RBDNF.
   * @params i m errtxt
   * @param m       the match that caused the error.
   * @param errtxt  explanatory error text.
   *)
  exception MalformedRBDNF of string * info * bgmatch * string
  (** Signal that two lists unexpectedly are of unequal length.
   * @params file l1 l2 errtxt
   * @param file    the file name in which the exception was raised.
   * @param l1      the first list.
   * @param l2      the second list.
   * @param errtxt  explanatory error text.
   *)
  exception UnequalLength of string * bgval list * bgval list * string
  (** Signal that two lists unexpectedly are of unequal length.
   * @params file l1 l2 errtxt
   * @param file    the file name in which the exception was raised.
   * @param l1      the first list.
   * @param l2      the second list.
   * @param errtxt  explanatory error text.
   *)
  exception UnequalLength2
    of string * bgval list * (int * nameset) list * string

  val take = List.take
  val drop = List.drop
        
  fun unmkB B = 
      case match (PCom (PVar, PVar)) B of
        MCom (MVal wirxid, MVal D) =>
             {wirxid = wirxid, D = D}
      | wrongterm => 
          raise MalformedBDNF 
                  ("bgbdnf.sml", info B, wrongterm,
                   "matching B in unmkB")

  fun unmkD D =
      case match (PTen [PVar, PCom (PTns, PPer)]) D of
        MTen [MVal ren, MCom (MTns Ps, MPer pi)] =>
             {ren = ren, Ps = Ps, perm = Per (info D) pi}
      | wrongterm => 
          raise MalformedBDNF 
                  ("bgbdnf.sml", info D, wrongterm,
                   "matching D in unmkD")

  fun unmkP P =
      case match (PCom (PVar, PVar)) P of
        MCom (MVal idxlocsub, MVal N) =>
          {idxlocsub = idxlocsub, N = N}
      | wrongterm => 
          raise MalformedBDNF 
                  ("bgbdnf.sml", info P, wrongterm,
                   "matching P in unmkP")

  fun unmkN N =
      case match (PAbs (PVar)) N of
        MAbs (absnames, MVal G) =>
          {absnames = absnames, G = G}
      | wrongterm => 
          raise MalformedBDNF 
                  ("bgbdnf.sml", info N, wrongterm,
                   "matching N in unmkN")

  fun unmkG G =
      case match (PCom (PVar, PTns)) G of
        MCom (MVal idxmerge, MTns Ss) =>
          {idxmerge = idxmerge, Ss = Ss}
      | wrongterm =>
          raise MalformedBDNF 
                  ("bgbdnf.sml", info G, wrongterm,
                   "matching G in unmkG")

  fun unmkS S =
      let
        val i = info S
      in
        case match (PCom (PTen [PWir, PCns], PVar)) S of
          (MCom (MTen [MWir a, MPer id_1], MVal concx)) =>
            SCon (Com i (Ten i [Wir i a, Per i id_1], concx))
        | (MCom (MTen [MWir a, MIon KyX], MVal N)) =>
            SMol (Com i (Ten i [Wir i a, Ion i KyX], N))
        | wrongterm =>
            raise MalformedBDNF 
                    ("bgbdnf.sml", i, wrongterm,
                     "matching S in unmkS")
      end

  fun unmkM M =
      case match (PCom (PVar, PVar)) M of
        MCom (MVal idxion, MVal N) =>
          {idxion = idxion, N = N}
      | wrongterm =>
          raise MalformedBDNF 
                  ("bgbdnf.sml", info M, wrongterm,
                   "matching M in unmkM")

  fun unmkRB B = 
      case match (PCom (PVar, PVar)) B of
        MCom (MVal wirxid, MVal D) =>
             {wirxid = wirxid, D = D}
      | wrongterm => 
          raise MalformedRBDNF 
                  ("bgbdnf.sml", info B, wrongterm,
                   "matching B in unmkRB")

  fun unmkRD D =
      case match (PTen [PVar, PTns]) D of
        MTen [MVal ren, MTns Ps] =>
             {ren = ren, Ps = Ps}
      | wrongterm => 
          raise MalformedRBDNF 
                  ("bgbdnf.sml", info D, wrongterm,
                   "matching D in unmkRD")

  fun make v =
      let
        val i = BgVal.info v

        (* bgval constructors *)
        val Mer = BgVal.Mer i
        val Con = BgVal.Con i
        val Wir = BgVal.Wir i
        val Ion = BgVal.Ion i
        val Per = BgVal.Per i
        val Abs = BgVal.Abs i
        val Ten = BgVal.Ten i
        val Com = BgVal.Com i
        val LS  = BgVal.LS  i
        val WLS = BgVal.WLS i

        (* Tensor product of two bgvals, as operator *)
        fun xx (v1, v2) = Ten [v1, v2]
        infix 6 xx
        (* Composition constructor, as operator *)
        val oo = Com
        infix 7 oo

        (* id_0 wirings and bgval wirings *)
        val wid_0  = Wiring.id_0
        val vwid_0 = Wir wid_0
        val pid_0  = Permutation.id_0
        val vpid_0 = Per pid_0
        val vpid_1 = Per (Permutation.id_n 1)
 
        (* Rename global outer names in a discrete prime. *)
        fun renameGlobals beta P =
            let
              fun renM beta M =
                  case match (PCom (PTen [PWir, PIon], PVar)) M of
                    MCom (MTen [MWir wid_Z, MIon KyX], MVal N) =>
                    let
                      val Z  = Wiring.outernames wid_Z
                      val Z' = Wiring.app beta Z
                      val wid_Z' = Wiring.id_X Z'
                      val N' = renN (Wiring.restrict beta Z) N
                      val y' = Wiring.app beta (Ion.outernames KyX)
                      val {ctrl, free, bound} = Ion.unmk KyX
                      val Ky'X
                          = Ion.make {ctrl  = ctrl,
                                      free  = NameSet.list y',
                                      bound = bound}
                    in
                      (Wir wid_Z' xx Ion Ky'X) oo N'
                    end
                  | wrongterm => 
                    raise MalformedBDNF ("bgbdnf.sml", i, wrongterm,
                                         "matching M in renM")

              and renS beta S =
                  case unmkS S of
                    SCon renConc =>
                  (case match (PCom (PTen [PWir, PVar], PVar))
                          renConc of
                     MCom (MTen [MWir a, MVal id_1], MVal concX) =>
                     let
                       val a' = Wiring.o (beta, a)
                     in
                       (Wir a' xx id_1) oo concX
                     end
                   | wrongterm => 
                     raise MalformedBDNF ("bgbdnf.sml", i, wrongterm,
                                          "matching a in renS"))
                  | SMol M => renM beta M

              and renG beta G =
                  case match (PCom (PTen [PWir, PVar], PTns)) G of
                    MCom (MTen [MWir wid_Y, MVal mer_n], MTns Ss) =>
                    let
                      val Y  = Wiring.outernames wid_Y
                      val Y' = Wiring.app beta (Y)
                      val wid_Y' = Wiring.id_X Y'
                      val S's
                          = map
                              (fn S =>
                                  renS (Wiring.restrict
                                          beta
                                          (Interface.glob
                                             (outerface S)))
                                  S)
                              Ss
                    in
                      (Wir wid_Y' xx mer_n) oo Ten S's
                    end
                  | wrongterm => 
                    raise MalformedBDNF ("bgbdnf.sml", i, wrongterm,
                                         "matching G in renG")

              and renN beta N =
                  let
                    val {absnames, G} = unmkN N
                    val beta' = Wiring.* (beta, Wiring.id_X absnames)
                    val G' = renG beta' G
                  in
                    Abs (absnames, G')
                  end

              fun renP beta P =
                  case match (PCom (PTen [PWir, PVar], PVar)) P of
                    MCom (MTen [MWir wid_Z, MVal locsub], MVal N) =>
                    let
                      val N' = renN beta N
                      val Z  = Wiring.outernames wid_Z
                      val Z' = Wiring.app beta Z
                      val wid_Z' = Wiring.id_X Z'
                    in
                      (Wir wid_Z' xx locsub) oo N'
                    end
                  | wrongterm => 
                    raise MalformedBDNF ("bgbdnf.sml", i, wrongterm,
                                         "matching P in renP")
                    
            in
              renP beta P
            end

        fun bgvalCom2SBDNF v = (* SCom rules *)
            case match
                   (PCom (PTen [PWir, PCom (PTen [PWir, PCns], PVar)],
                          PVar))
                   v of
              MCom (MTen [MWir id_Z,
                          MCom (MTen [MWir a, MPer id_1], MVal ConcY)],
                    MVal barP)
                => (* Rule Ccom *)
                (case match
                        (PTen [PCom (PTen [PWir,
                                           PAbs (PCom (PTen [PWir,
                                                             PPer],
                                                       PCon))],
                                     PAbs (PCom (PTen [PWir, PMer],
                                           PVar)))])
                        barP of 
                   MTen [MCom (MTen [MWir id_V,
                                     MAbs (Y, MCom (MTen [MWir yX,
                                                          MPer id_1],
                                                    MCon X))],
                               MAbs (X', MCom (MTen [MWir id_U, MMer n],
                                               MVal barS)))]
                     =>
                     let
                       val op o = Wiring.o  infix 7 o
                       val x = Wiring.*  infix 6 x
                       val s = (id_Z x a) o (id_Z x yX)
                     in
                       (s, barS)
                     end
                 | wrongterm =>
                     raise MalformedBDNF ("bgbdnf.sml", i, wrongterm,
                              "matching barP in Ccom rule"))
            | MCom (MTen [MWir id_Z,
                          MCom (MTen [MWir id_Y, MIon KyX], MVal N)],
                    MVal barP)
                => (* Rule Mcom *)
                let
                  val (s, N') = bgvalCom2NBDNF ((Wir id_Z xx N) oo barP)
                  val {ctrl = K, free = ys, bound = Xss} = Ion.unmk KyX
		  val X'ss = map (Wiring.app_inverse s) Xss
                  val Z = Wiring.outernames id_Z
                  val Z' = Wiring.app_inverse s Z
                  val Y = Wiring.outernames id_Y
                  val Y' = Wiring.app_inverse s Y
                  val id_ys = Wiring.id_X (NameSet.fromList ys)
                  val ZY = NameSet.union Z Y
                  val s' = Wiring.* (id_ys, Wiring.restrict_outer s ZY)
                  val id_Z'Y' = Wiring.id_X (NameSet.union Z' Y')
                  val KyX'
                      = Ion.make {ctrl = K, free = ys, bound = X'ss}
                in
                  (s', Ten [(Wir id_Z'Y' xx Ion KyX') oo N'])
                end
            | wrongterm =>
                raise MalformedBDNF ("bgbdnf.sml", i, wrongterm,
                                     "in Scom rules")
      
        and bgvalCom2NBDNF v = (* Ncom rule *)
            case match
                   (PCom (PTen [PWir,
                                PAbs (PCom (PTen [PWir, PMer], PTns))],
                          PTns)) v of
              MCom (MTen [MWir id_Z,
                          MAbs (X,
                                MCom (MTen [MWir id_Y, MMer n],
                                      MTns Ss))],
                    MTns Ps)
                => (* Rule Ncom *)
                let
      	          (* Compose each Si with as many primes
                   * from Ps as its inner face requires.
                   *)
                  fun composeprimes [] [] = []
                    | composeprimes (Si :: Ss) Ps =
                      let
                        val m     = Interface.width (innerface Si)
                        val P'i   = take (Ps, m)
                        val Prest = drop (Ps, m)
                        val Zi 
             	              = foldl
             	                  (fn (P'ij, Zi) 
                                   => NameSet.union 
                                        Zi 
                                        (Interface.glob
                                           (outerface P'ij)))
                               NameSet.empty
                               P'i
                         in
                           ((Wir (Wiring.id_X Zi) xx Si) oo Ten P'i)
                           :: composeprimes Ss Prest
                         end
                       | composeprimes [] err2s =
                         raise UnequalLength 
                                 ("bgbdnf.sml", [], err2s,
                                  "composeprimes in rule Ncom")
                  (* Concat the tensor products barS_i and tensor the
                   * substitutions s_i resulting from the normalization
                   * of each of the S_i compositions.
                   *)
                  val (s, barSs)
                      = (foldr
                           (fn ((s_i, barS_i), (s, barS)) =>
                               case match PTns barS_i of
                                 MTns barS_is =>
                                   (Wiring.* (s_i, s), barS_is @ barS)
                               | wrongterm =>
                                   raise
                                     MalformedBDNF
                                       ("bgbdnf.sml", i,
                                        wrongterm,
                                        "matching barSi in Ncom rule"))
                            (Wiring.id_0, [])
                            (map
                               bgvalCom2SBDNF
                               (composeprimes Ss Ps)))
                  val barS = Ten barSs
                  val X' = Wiring.app_inverse s X
                  val Z  = Wiring.outernames id_Z
                  val Z' = Wiring.app_inverse s Z
                  val Y  = Wiring.outernames id_Y
                  val Y' = Wiring.app_inverse s Y
                  val id_Z'Y' = Wiring.id_X (NameSet.union Z' Y')
                  val n' = Interface.width (outerface barS)
                in
                  (s, Abs (X', (Wir id_Z'Y' xx Mer n') oo barS))
                end
            | wrongterm => 
                raise MalformedBDNF ("bgbdnf.sml", i, wrongterm,
                                     "in Ncom rule")
      
        fun bgval2PBDNF v = (* Pcom rule *)
            case match (PCom (PTen [PWir,
                                    PCom (PTen [PWir,
                                                PAbs (PCom (PTen [PWir,
                                                                  PPer],
                                                            PCon))],
                                          PVar)],
                              PVar)) v of
              MCom (MTen [MWir id_Z,
                          MCom (MTen [MWir id_Z',
                                      MAbs (Y, MCom (MTen [MWir yX,
                                                           MPer id_1],
                                                     MCon X))],
                                MVal N)],
                    MVal barP)
                => (* Rule Pcom *)
                let
                  val (s, N') = bgvalCom2NBDNF ((Wir id_Z xx N) oo barP)
                  val Z    = Wiring.outernames id_Z
                  val Z'   = Wiring.outernames id_Z'
                  val ZZ'  = NameSet.union Z Z'
                  val W    = Wiring.app_inverse s ZZ'
                  val id_W = Wiring.id_X W
                  val yX'
                      = Wiring.make'
                          (LinkSet.fold
                             ((fn {outer, inner} => fn ls =>
                                  Link.make
                                    {outer = outer,
                                     inner = Wiring.app_inverse s inner}
                                  :: ls) o Link.unmk)
                           [] (Wiring.unmk yX))
                  val s' = Wiring.restrict_outer s ZZ'
                in
                  (s', (Wir id_W xx LS yX') oo N')
                end
            | wrongterm => 
                raise MalformedBDNF ("bgbdnf.sml", i, wrongterm,
                                     "in Pcom rule")
      
        fun bgval2BBDNF v = (* Bxxx rules *)
            case match PCns v of
              MMer n => (* Rule Bmer *)
              let
                val cs = List.tabulate
                           (n,
                            (fn _ => (vwid_0 xx vpid_1) 
                                       oo Con NameSet.empty))
                val vpid_n = Per (Permutation.id_n n)
                val N = Abs (NameSet.empty,
                               (vwid_0 xx Mer n)
                                 oo Ten cs)
                val P = (vwid_0 xx LS wid_0) oo N
                val D = vwid_0 xx Ten [P] oo vpid_n
              in
                (vwid_0 xx vpid_1) oo D
              end
    
            | MCon X => (* Rule Bcon *)
              let
                val vwid_X = Wir (Wiring.id_X X)
                val vpid_X = Per (Permutation.id [X])
                val N = Abs (NameSet.empty,
                               (vwid_X xx Mer 1)
                                 oo Ten [(vwid_X xx vpid_1) oo Con X])
                val P = (vwid_X xx LS wid_0) oo N
                val D = vwid_0 xx Ten [P] oo vpid_X
              in
                (vwid_X xx vpid_1) oo D
              end
    
            | MWir w => (* Rule Bwir *)
              let
                val X = Wiring.innernames w
                val vwid_X = Wir (Wiring.id_X X)
              in
                (v xx vpid_0) oo (vwid_X xx Ten [] oo vpid_0)
              end
    
            | MIon KyX => (* Rule Bion *)
              let
                val X = Ion.innernames KyX
                val Y = Ion.outernames KyX
                val vwid_X = Wir (Wiring.id_X X)
                val vwid_Y = Wir (Wiring.id_X Y)
                val vpid_X = Per (Permutation.id [X])
                val M = (vwid_0 xx v)
                          oo Abs (X,
                                  (vwid_X xx Mer 1) 
                                    oo Ten [(vwid_X xx vpid_1)
                                              oo Con X])
                val N = Abs (NameSet.empty,
                               (vwid_Y xx Mer 1) oo Ten [M])
                val P = (vwid_Y xx LS wid_0) oo N
                val D = vwid_0 xx Ten [P] oo vpid_X
              in
                (vwid_Y xx vpid_1) oo D
              end
    
            | MPer pi => (* Rule Bper *)
              let
                fun makePi Yi =
                    let
                      val wid_Yi  = Wiring.id_X Yi
                      val vwid_Yi = Wir wid_Yi
                      val Ni = Abs (Yi,
                                    (vwid_Yi xx Mer 1)
                                      oo Ten [(vwid_Yi xx vpid_1)
                                               oo Con Yi])
                    in
                      (vwid_0 xx LS wid_Yi) oo Ni
                    end
                val Ys      = Interface.loc (Permutation.outerface pi)
                val vpid_Ys = Per (Permutation.id Ys)
                val Pis     = map makePi Ys
                val D       = vwid_0 xx Ten Pis oo Per pi
              in
                (vwid_0 xx vpid_Ys) oo D
              end
    
            | MAbs (X, MVal b) => (* Rule Babs *)
              (case match
                      (PCom
                         (PTen [PWir, PPer],
                          PTen
                            [PWir,
                             PCom (PTen [PCom
                                           (PTen
                                              [PWir,
                                               PAbs
                                                 (PCom
                                                    (PTen [PWir,
                                                           PPer],
                                                        PCon))],
                                                PAbs PVar)],
                                   PPer)]))
                      (bgval2BBDNF b) of
                 MCom (MTen [MWir wzW, MPer pid_Y],
                       MTen
                         [MWir wid_0,
                          MCom
                            (MTen 
                               [MCom (MTen
                                        [MWir wid_Z,
                                         MAbs (Y, MCom 
                                                    (MTen
                                                       [MWir wyX,
                                                        MPer pid_1],
                                                     MCon Xs))],
                                MAbs (W, MVal G))],
                             MPer pid_I)])
                 =>
                 let
                   fun split link (linkstoX, notlinkstoX) =
                       let
                         val {outer, inner} = Link.unmk link
                       in
                         case outer of
                           SOME z =>
                           if NameSet.member z X then
                              (LinkSet.insert link linkstoX,
                               notlinkstoX)
                           else
                              (linkstoX,
                               LinkSet.insert link notlinkstoX)
                         | NONE =>
                           (linkstoX, LinkSet.insert link notlinkstoX)
                       end
                   val linkszW = Wiring.unmk wzW
                   val (linkstoX, notlinkstoX)
                     = LinkSet.fold
                         split (LinkSet.empty, LinkSet.empty) linkszW
                   val vwzW' = Wir (Wiring.make notlinkstoX)
                   val linksyX = Wiring.unmk wyX
                   val wyzXW'
                       = Wiring.make (LinkSet.union linksyX linkstoX)
                   val LSyzXW' = LS wyzXW'
                   val vpid_U 
                       = Per (Permutation.id
                                [Wiring.outernames wyzXW'])
                   val WX
                     = LinkSet.fold
                         (fn link => 
                             fn WX => 
                                NameSet.union
                                  WX (Link.innernames link))
                         NameSet.empty
                         linkstoX
                   val WbarX
                     = LinkSet.fold
                         (fn link => 
                             fn WX => 
                                NameSet.union 
                                  WX (Link.innernames link))
                         NameSet.empty
                         notlinkstoX
                   val vwid_WbarX = Wir (Wiring.id_X WbarX)
                   val N = Abs (NameSet.union WX W, G)
                   val P = (vwid_WbarX xx LSyzXW') oo N
                 in
                   (vwzW' xx vpid_U)
                     oo (vwid_0 xx Ten [P] oo Per pid_I)
                 end
               | wrongterm =>
                 raise
                   MalformedBDNF ("bgbdnf.sml", i, wrongterm,
                                   "matching b in rule Babs"))
    
            | MTns bs => (* Rule Bten *)
              let
                fun unzip (b_i, (w, pid_Y, a, pi, Ps, X)) =
                    case match (PCom (PTen [PWir, PPer], PVar))
                               (bgval2BBDNF b_i) of
                      MCom (MTen [MWir w_i, MPer pid_Yi], MVal Di) =>
                   (case match (PTen [PWir, PCom (PTns, PPer)]) Di of
                      MTen [MWir a_i, MCom (MTns P_ijs, MPer pi_i)]
                      =>
                      let
                        val Yi
                            = foldr
                                (fn (y, Yi) => NameSet.union' y Yi)
                                NameSet.empty
                                (Interface.loc
                                   (Permutation.outerface pid_Yi))
                        val Xi  = NameSet.union' X Yi
                        val Zi  = Interface.glob (outerface Di)

                        fun fresh z n X =
                            let
                              val z' = Name.make (z ^ (Int.toString n))
                            in
                              if NameSet.member z' X then
                                fresh z (n + 1) X
                              else
                                z'
                            end
                        fun addRenaming z (beta, beta_inv, X) =
                            let
                              val w = fresh (Name.unmk z) 0 X
                              val X = NameSet.insert' w X
                              val l 
                                  = Link.make
                                      {outer = SOME w,
                                       inner = NameSet.fromList [z]}
                              val l_inv 
                                  = Link.make
                                      {outer = SOME z,
                                       inner = NameSet.fromList [w]}
                            in
                              (l :: beta, l_inv :: beta_inv, X)
                            end

                        val (ZinXi, ZnotinXi)
                            = NameSet.partition
                                (fn n => NameSet.member n Xi)
                                Zi
                        val (betainXi, betainXi_inv, X)
                            = NameSet.fold
                                addRenaming ([], [], NameSet.union' Xi Zi) ZinXi
                        val betanotinXi = Wiring.id_X ZnotinXi
                        val beta
                            = Wiring.* 
                                (betanotinXi, Wiring.make' betainXi)
                        val beta_inv
                            = Wiring.* 
                                (betanotinXi, Wiring.make' betainXi_inv)

                        val Z'_i = Wiring.outernames a_i
                        val a'_i = Wiring.o
                                     (Wiring.restrict beta Z'_i, a_i)

                        val P'_ijs
                            = map (fn P_ij =>
                                      renameGlobals
                                        (Wiring.restrict
                                           beta
                                           (Interface.glob
                                             (outerface P_ij)))
                                        P_ij)
                                  P_ijs

                        val w'_i = Wiring.o (w_i, beta_inv)

                        val w     = Wiring.* (w'_i, w)
                        val a     = Wiring.* (a'_i, a)
                        val pid_Y = Permutation.* (pid_Yi, pid_Y)
                        val pi    = Permutation.* (pi_i, pi)
                      in
                        (w, pid_Y, a, pi, P'_ijs @ Ps, X)
                      end
                    | wrongterm => raise MalformedBDNF
                         ("bgbdnf.sml", i, wrongterm,
                          "matching Di in rule Bten"))
                    | wrongterm => raise MalformedBDNF
                         ("bgbdnf.sml", i, wrongterm,
                          "matching b_i in rule Bten") 
                val (w, pid_Y, a, pi, Ps, _)
                    = foldr
                        unzip
                        (wid_0, pid_0, wid_0, pid_0, [], NameSet.empty)
                        bs
                val D = Wir a xx Ten Ps oo Per pi
              in
                (Wir w xx Per pid_Y) oo D
              end
    
            | MCom (MVal b1, MVal b2) => (* Rule Bcom *)
              (* match Bs *)
              (case match (PCom (PTen [PWir, PPer], PVar))
                          (bgval2BBDNF b1) of
                 MCom (MTen [MWir w_1, MPer pid_U1], MVal D1)
                 =>
              (case match (PCom (PTen [PWir, PPer], PVar))
                          (bgval2BBDNF b2) of
                 MCom (MTen [MWir w_2, MPer pid_U2], MVal D2)
                 =>
              (* match Ds*)
              (case match (PTen [PWir, PCom (PTns, PPer)]) D1 of
                 MTen [MWir a_1, MCom (MTns P_1s, MPer pi_1)]
                 =>
              (case match (PTen [PWir, PCom (PTns, PPer)]) D2 of
                 MTen [MWir a_2, MCom (MTns P_2s, MPer pi_2)]
                 =>
                 let
                   (* Compose each prime P_1i with as many primes
                    * from P_2s as its inner face requires.
                    *)
                   fun composeprimes [] [] = []
                     | composeprimes (P_1i::P_1s) P_2s =
                       let
                         val m'i = Interface.width (innerface P_1i)
                         val P_2is = take (P_2s, m'i)
                         val P_2srest = drop (P_2s, m'i)
                         val Z'_i 
           	               = foldl
                                   (fn (P_2i, Z'_i)
                                    => NameSet.union 
                                       Z'_i 
                                       (Interface.glob
                                          (outerface P_2i)))
                               NameSet.empty
                               P_2is
                         val vwid_Z'_i = Wir (Wiring.id_X Z'_i)
                       in
                         ((vwid_Z'_i xx P_1i) oo Ten P_2is)
                         :: composeprimes P_1s P_2srest
                       end
                     | composeprimes [] err2s =
                       raise UnequalLength 
                               ("bgbdnf.sml", [], err2s,
                                "composeprimes in rule Bcom")
                   val (ss, Ps)
                     = ListPair.unzip
                         (map bgval2PBDNF 
                              (composeprimes P_1s
                                 (Permutation.permute
                                    (Permutation.invert pi_1) P_2s)))
                   val U = Interface.glob (innerface D2)
                   val wid_U = Wiring.id_X U
                   val s = Wiring.* (wid_U, Wiring.** ss)
                   val X''ss = map (Interface.loc o innerface) P_2s
                   val pi = Permutation.o 
                              (Permutation.pushthru pi_1 X''ss, pi_2)
                   val D = Wir wid_U xx Ten Ps oo Per pi
                   val V1 = Interface.glob (outerface (Ten P_1s))
                   val wid_V1 = Wiring.id_X V1
                   val V2 = Interface.glob (outerface (Ten P_2s))
                   val wid_V2 = Wiring.id_X V2
                   val op o = Wiring.o  infix 7 o
                   val x = Wiring.*  infix 6 x
                   val w 
                       = w_1 o (a_1 o w_2 o (a_2 x wid_V2) x wid_V1) o s
                 in
                   (Wir w xx Per pid_U1) oo D
                 end
               | wrongtermD2 =>
                 raise MalformedBDNF ("bgbdnf.sml", i, wrongtermD2,
                                       "matching D2 in rule Bcom"))
               | wrongtermD1 =>
                 raise MalformedBDNF ("bgbdnf.sml", i, wrongtermD1,
                                       "matching D1 in rule Bcom"))
               | wrongtermb2 =>
                 raise MalformedBDNF ("bgbdnf.sml", i, wrongtermb2,
                                       "matching b2 in rule Bcom"))
               | wrongtermb1 =>
                 raise MalformedBDNF ("bgbdnf.sml", i, wrongtermb1,
                                       "matching b1 in rule Bcom"))
            | wrongtermB =>
              raise MalformedBDNF ("bgbdnf.sml", i, wrongtermB,
                                   "matching in Bxxx rules")
  in
    bgval2BBDNF v
  end (* fun make v *)

  fun regularize B =
      let
        val {wirxid, D} = unmkB B

        val i = BgVal.info B

        (* bgval constructors *)
        val Mer = BgVal.Mer i
        val Con = BgVal.Con i
        val Wir = BgVal.Wir i
        val Ion = BgVal.Ion i
        val Per = BgVal.Per i
        val Abs = BgVal.Abs i
        val Ten = BgVal.Ten i
        val Com = BgVal.Com i

        (* Tensor product of two bgvals, as operator *)
        fun xx (v1, v2) = Ten [v1, v2]
        infix 6 xx
        (* Composition constructor, as operator *)
        val oo = Com
        infix 7 oo

        fun regM M pi =
            let
              val {idxion, N} = unmkM M
              val N' = regN N pi
            in
              Com (idxion, N')
            end
              
        and regS (S, pi) =
            case unmkS S of
              (* FIXME: test if pi is an identity permutation *)
              SCon a => a
            | SMol M => regM M pi
            
        and regN N pi =
            let
              val {absnames, G}  = unmkN N
              val {idxmerge, Ss} = unmkG G
              val Xss = map (Interface.loc o innerface) Ss
              val {major, minors} = Permutation.split pi Xss
              val Ss' = map regS (ListPair.zip (Ss, minors))
            in
              Abs (absnames,
                   Com (idxmerge, Ten (Permutation.permute major Ss')))
            end

        fun regD D =
            let
              fun regPs [] [] = []
                | regPs (P_i::Ps) pis =
                  let
                    val {idxlocsub, N} = unmkP P_i
                    val m_i  = Interface.width (innerface P_i)
                    val pi_i = Permutation.make (take (pis, m_i))
                    val pisrest = drop (pis, m_i)
                    val N'_i = regN N pi_i
                  in
                    Com (idxlocsub, N'_i)
                    :: (regPs Ps pisrest)
                  end
                | regPs [] err2s =
                  raise UnequalLength2 ("bgbdnf.sml", [], err2s,
                                       "regPs in regularise")

              val {ren, Ps, perm} = unmkD D
              val pi = case match PPer perm of
                         MPer pi => pi
                       | wrongterm =>
                           raise MalformedBDNF 
                             ("bgbdnf.sml", info perm, wrongterm,
                              "matching perm in regD")
              val pis = Permutation.unmk pi
            in
              Ten [ren, Ten (regPs Ps pis)]
            end
            
      in
        Com (wirxid, regD D)
      end

  fun unmk bdnf = bdnf : bgval

  val info = BgVal.info

  fun pp indent pps
    = BgVal.pp indent pps o unmk
end
