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
 * @version $LastChangedRevision$
 *)
functor BgBDNF'(structure Info : INFO
		structure Name : NAME
		structure NameSet : MONO_SET
                structure NameBijectionConstraints : BIJECTION_CONSTRAINTS
		structure LinkSet : MONO_SET
		structure Interface : INTERFACE
		structure Ion : ION
		structure Permutation : PERMUTATION
		structure Link : LINK
		structure Wiring : WIRING
		structure BgVal : BGVAL
		structure ErrorHandler : ERRORHANDLER
                  where type ppstream    = PrettyPrint.ppstream
                    and type break_style = PrettyPrint.break_style
                    and type origin      = Origin.origin
		structure NameSetPP : COLLECTIONPRETTYPRINT
                  where type ppstream    = PrettyPrint.ppstream
		structure ListPP : POLYCOLLECTIONPRETTYPRINT
                  where type ppstream      = PrettyPrint.ppstream
                    and type 'a collection = 'a list
		sharing type NameSet.Set = NameSetPP.collection
		sharing type BgVal.interface = 
			     Interface.interface =
			     Permutation.interface
		sharing type BgVal.ion = Ion.ion
		sharing type BgVal.permutation =
			     Permutation.permutation
                sharing type BgVal.Immutable =
			     Permutation.Immutable
		sharing type BgVal.wiring = Wiring.wiring
		sharing type NameSet.Set =
                             NameBijectionConstraints.set =
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
		sharing type NameSet.elt =
		             Link.name =
                             Name.name =
                             Ion.name
                sharing type NameBijectionConstraints.constraints =
                             Ion.nameconstraints =
                             Permutation.nameconstraints =
                             Wiring.nameconstraints
                sharing type Info.info =
                             BgVal.info
			     ) : BGBDNF 
  where type nameset        = NameSet.Set
    and type info           = Info.info 
    and type interface      = BgVal.interface
    and type wiring         = BgVal.wiring
    and type ion            = Ion.ion
    and type Immutable      = BgVal.Immutable
    and type 'a permutation = 'a BgVal.permutation
    and type bgval          = BgVal.bgval 
    and type bgmatch        = BgVal.bgmatch =
struct
  open BgVal
  type Immutable = BgVal.Immutable
  type ion = Ion.ion
  open Debug
  open ErrorHandler
  val noinfo = Info.noinfo

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
   * The regularized bigraph representation differ only for DR BDNF:
   *
   * DR BDNF: Ten [Wir w, Ten [P_0, ..., P_n-1]]
   * BR BDNF: Com (Ten [Wir w, Per id_X], DR)
   *)

  type M = unit
  type S = unit
  type G = unit
  type N = unit
  type P = unit
  type D = unit
  type B = unit
  type DR = unit
  type BR = unit
  type 'class bgbdnf = bgval

  datatype stlnode =
           SCon of info * wiring
         | SMol of M bgbdnf

  datatype stlnode' =
           SCon' of bgval
         | SMol' of M bgbdnf

  val file_origin = Origin.mk_file_origin
                      "$BPL/src/kernel/ast/bgbdnf.sml"
                      Origin.NOPOS
  fun mk_explainer errtitle (explainer : exn -> explanation list) e =
      Exp (LVL_USER, Origin.unknown_origin, mk_string_pp errtitle,
           explainer e)

  exception IrregularBDNF of info * bgval * Immutable permutation
                             * nameset list list * string
  fun explain_IrregularBDNF (IrregularBDNF (i, b, pi, Xss, errtxt)) =
      [Exp (LVL_USER, Info.origin i, pack_pp_with_data BgVal.pp b, []),
       Exp (LVL_LOW, file_origin, mk_string_pp errtxt,
            [Exp (LVL_LOW, Origin.unknown_origin,
                  pack_pp_with_data Permutation.pp pi, []),
             Exp (LVL_LOW, Origin.unknown_origin,
                  mk_string_pp "Xss = ",
                  map (fn Xs =>
                       Exp (LVL_LOW, Origin.unknown_origin,
                            pack_pp_with_data 
                            (fn indent =>
                             ListPP.pp NameSetPP.pp indent)
                            Xs, [])) Xss)])]
    | explain_IrregularBDNF _ = raise Match
  val _ = add_explainer
            (mk_explainer "bigraph is not regular" explain_IrregularBDNF)

  exception MalformedBDNF of info * bgmatch * string
  fun explain_MalformedBDNF (MalformedBDNF (i, b, errtxt)) =
      [Exp (LVL_USER, Info.origin i,
            pack_pp_with_data BgVal.pp_match b, []),
       Exp (LVL_LOW, file_origin, mk_string_pp errtxt, [])]
    | explain_MalformedBDNF _ = raise Match
  val _ = add_explainer
            (mk_explainer
               "bgval is not on an appropriate BDNF form"
               explain_MalformedBDNF)

  exception MalformedRBDNF of info * bgmatch * string
  fun explain_MalformedRBDNF (MalformedRBDNF (i, b, errtxt)) =
      [Exp (LVL_USER, Info.origin i,
            pack_pp_with_data BgVal.pp_match b, []),
       Exp (LVL_LOW, file_origin, mk_string_pp errtxt, [])]
    | explain_MalformedRBDNF _ = raise Match
  val _ = add_explainer
            (mk_explainer
               "bgval is not on an appropriate RBDNF form"
               explain_MalformedRBDNF)

  exception UnequalLength of bgval list * bgval list * string
  fun explain_UnequalLength (UnequalLength (vs1, vs2, errtxt)) =
      let
        fun bgval2exp v =
            Exp (LVL_USER, Info.origin (BgVal.info v),
                 pack_pp_with_data BgVal.pp v, [])
      in
        [Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "vs1",
              map bgval2exp vs1),
         Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "vs2",
              map bgval2exp vs2),
         Exp (LVL_LOW, file_origin, mk_string_pp errtxt, [])]
      end
    | explain_UnequalLength _ = raise Match
  val _ = add_explainer
            (mk_explainer
               "bgval lists of unequal length"
               explain_UnequalLength)

  exception LogicalError of string
  fun explain_LogicalError (LogicalError errtxt) =
      [Exp (LVL_LOW, file_origin, mk_string_pp errtxt, [])]
    | explain_LogicalError _ = raise Match
  val _ = add_explainer
            (mk_explainer
               "an internal error occurred"
               explain_LogicalError)

  val take = List.take
  val drop = List.drop

	fun makeB w Xs D =
	  let
	    val i = info D
	    val Wir = Wir i
	    val Per = Per i
	    val Ten = Ten i
	    fun ** (v1, v2) = Ten [v1, v2]  infix 6 **
	    val oo = Com i                  infix 7 oo
	  in
	    (Wir w ** Per (Permutation.id Xs)) oo D
	  end
	val makeBR = makeB
	fun makeD w Ps pi =
	  let
	    val i = case Ps of P :: _ => info P | _ => noinfo
	    val Wir = Wir i
	    val Per = Per i
	    val Ten = Ten i
	    fun ** (v1, v2) = Ten [v1, v2]  infix 6 **
	    val oo = Com i                  infix 7 oo
	  in
	     Wir w ** Ten Ps oo Per pi
	  end
  fun makeDR w Ps =
	  let
	    val i = case Ps of P :: _ => info P | _ => noinfo
	    val Wir = Wir i
	    val Ten = Ten i
	    fun ** (v1, v2) = Ten [v1, v2]  infix 6 **
	    val oo = Com i                  infix 7 oo
	  in
	     Wir w ** Ten Ps
	  end
	fun makeP s N =
		let
			val i = info N
	    val Wir = Wir i
	    val Ten = Ten i
	    val Per = Per i
	    val Abs = Abs i
	    val Con = Con i
	    fun ** (v1, v2) = Ten [v1, v2]  infix 6 **
	    val oo = Com i                  infix 7 oo
	    val Z = Interface.glob (outerface N)
	    val Y = Wiring.outernames s
	    val X = Wiring.innernames s
	    val id_Z = Wiring.id_X Z
	    val id_1 = Permutation.id_n 1
		in
			(Wir id_Z ** Abs (Y, (Wir s ** Per id_1) oo Con X)) oo N
		end
	fun makeN X G = Abs (info G) (X, G)

	fun makeG Ss =
		let
			val i = case Ss of (S :: _) => info S | [] => noinfo
	    val Wir = Wir i
	    val Ten = Ten i
	    val Mer = Mer i
	    fun ** (v1, v2) = Ten [v1, v2]  infix 6 **
	    val oo = Com i                  infix 7 oo
            val Y = foldr (fn (X, Y) => NameSet.union X Y) NameSet.empty
                          (map (Interface.glob o outerface) Ss)
	    val id_Y = Wiring.id_X Y
	    val n = length Ss
		in
			(Wir id_Y ** Mer n) oo Ten Ss
		end	

  fun makeS (SCon (i, a)) =
			let
		    val Wir = Wir i
		    val Ten = Ten i
		    val Per = Per i
		    val Con = Con i
		    fun ** (v1, v2) = Ten [v1, v2]  infix 6 **
		    val oo = Com i                  infix 7 oo
		    val idp_1 = Permutation.id_n 1
		    val X = Wiring.innernames a
		  in
		  	(Wir a ** Per idp_1) oo Con X
		  end
    | makeS (SMol M) = M

	fun makeM KyX N =
	  let
			val i = info N
	    val Wir = Wir i
	    val Ten = Ten i
	    val Ion = Ion i
	    fun ** (v1, v2) = Ten [v1, v2]  infix 6 **
	    val oo = Com i                  infix 7 oo
			val Z = Interface.glob (outerface N)
			val id_Z = Wiring.id_X Z
		in
			(Wir id_Z ** Ion KyX) oo N
		end
          
  fun unmkB B = 
      case match (PCom (PVar, PVar)) B of
        MCom (MVal wirxid, MVal D) =>
             {wirxid = wirxid, D = D}
      | wrongterm => 
          raise MalformedBDNF 
                  (info B, wrongterm, "matching B in unmkB")

  fun unmkD D =
      case match (PTen [PVar, PCom (PTns, PPer)]) D of
        MTen [MVal ren, MCom (MTns Ps, MPer pi)] =>
             {ren = ren, Ps = Ps, perm = Per (info D) pi}
      | wrongterm => 
          raise MalformedBDNF 
                  (info D, wrongterm, "matching D in unmkD")

  fun unmkP' P =
      case match (PCom (PVar, PVar)) P of
        MCom (MVal idxlocsub, MVal N) =>
          {idxlocsub = idxlocsub, N = N}
      | wrongterm => 
          raise MalformedBDNF 
                  (info P, wrongterm, "matching P in unmkP'")

  fun unmkP P =
      case match (PCom
       	           (PTen
       	             [PWir,
                      PAbs (PCom (PTen [PWir, PPer], PCon))],
                            PVar)) P of
        MCom
         (MTen
           [MWir id_Z, MAbs (Y, MCom (MTen [MWir s, MPer id], MCon X))],
          MVal N) =>
          {id_Z = id_Z, Y = Y, s = s, X = X, N = N}
      | wrongterm => 
          raise MalformedBDNF 
                  (info P, wrongterm, "matching P in unmkP")

  fun unmkN N =
      case match (PAbs (PVar)) N of
        MAbs (absnames, MVal G) =>
          {absnames = absnames, G = G}
      | wrongterm => 
          raise MalformedBDNF 
                  (info N, wrongterm, "matching N in unmkN")

  fun unmkG G =
      case match (PCom (PVar, PTns)) G of
        MCom (MVal idxmerge, MTns Ss) =>
          {idxmerge = idxmerge, Ss = Ss}
      | wrongterm =>
          raise MalformedBDNF 
                  (info G, wrongterm, "matching G in unmkG")

  fun unmkS S =
      let
        val i = info S
      in
        case match (PCom (PTen [PWir, PCns], PVar)) S of
          (MCom (MTen [MWir a, MPer id_1], MVal concx)) =>
            SCon (i, a)
        | (MCom (MTen [MWir a, MIon KyX], MVal N)) =>
            SMol (Com i (Ten i [Wir i a, Ion i KyX], N))
        | wrongterm =>
            raise MalformedBDNF 
                    (i, wrongterm, "matching S in unmkS")
      end

  fun unmkS' S =
      let
        val i = info S
      in
        case match (PCom (PTen [PWir, PCns], PVar)) S of
          (MCom (MTen [MWir a, MPer id_1], MVal concx)) =>
            SCon' (Com i (Ten i [Wir i a, Per i id_1], concx))
        | (MCom (MTen [MWir a, MIon KyX], MVal N)) =>
            SMol' (Com i (Ten i [Wir i a, Ion i KyX], N))
        | wrongterm =>
            raise MalformedBDNF 
                    (i, wrongterm, "matching S in unmkS")
      end

  fun unmkM M =
      case match (PCom (PTen [PWir, PIon], PVar)) M of
        MCom (MTen [MWir id_Z, MIon KyX], MVal N) =>
          {id_Z = id_Z, KyX = KyX, N = N}
      | wrongterm =>
          raise MalformedBDNF 
                  (info M, wrongterm, "matching M in unmkM")

  fun unmkM' M =
      case match (PCom (PVar, PVar)) M of
        MCom (MVal idxion, MVal N) =>
          {idxion = idxion, N = N}
      | wrongterm =>
          raise MalformedBDNF 
                  (info M, wrongterm, "matching M in unmkM'")

  fun unmkBR B = 
      case match (PCom (PVar, PVar)) B of
        MCom (MVal wirxid, MVal D) =>
             {wirxid = wirxid, D = D}
      | wrongterm => 
          raise MalformedRBDNF 
                  (info B, wrongterm, "matching B in unmkRB")

  fun unmkDR D =
      case match (PTen [PVar, PTns]) D of
        MTen [MVal ren, MTns Ps] =>
             {ren = ren, Ps = Ps}
      | wrongterm => 
          raise MalformedRBDNF 
                  (info D, wrongterm, "matching D in unmkRD")

  fun innerface (b : 'class bgbdnf) = BgVal.innerface b

  fun outerface (b : 'class bgbdnf) = BgVal.outerface b

  fun bgvalCom2SBDNF v = (* SCom rules *)
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
        fun ** (v1, v2) = Ten [v1, v2]
        infix 6 **
        (* Composition constructor, as operator *)
        val oo = Com
        infix 7 oo

        (* id_0 wirings and bgval wirings *)
        val wid_0  = Wiring.id_0
        val vwid_0 = Wir wid_0
        val pid_0  = Permutation.id_0
        val vpid_0 = Per pid_0
        val vpid_1 = Per (Permutation.id_n 1)
      in
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
                 raise MalformedBDNF (i, wrongterm,
                          "matching barP in Ccom rule"))
        | MCom (MTen [MWir id_Z,
                      MCom (MTen [MWir id_Y, MIon KyX], MVal N)],
                MVal barP)
            => (* Rule Mcom *)
            let
              val (s, N') = bgvalCom2NBDNF ((Wir id_Z ** N) oo barP)
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
              (s', Ten [(Wir id_Z'Y' ** Ion KyX') oo N'])
            end
        | wrongterm =>
            raise MalformedBDNF (i, wrongterm, "in Scom rules")
      end

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
            fun ** (v1, v2) = Ten [v1, v2]
            infix 6 **
            (* Composition constructor, as operator *)
            val oo = Com
            infix 7 oo

            (* id_0 wirings and bgval wirings *)
            val wid_0  = Wiring.id_0
            val vwid_0 = Wir wid_0
            val pid_0  = Permutation.id_0
            val vpid_0 = Per pid_0
            val vpid_1 = Per (Permutation.id_n 1)

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
                     ((Wir (Wiring.id_X Zi) ** Si) oo Ten P'i)
                     :: composeprimes Ss Prest
                   end
                 | composeprimes [] err2s =
                   raise UnequalLength 
                           ([], err2s, "composeprimes in rule Ncom")
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
                                 (i, wrongterm,
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
            (s, Abs (X', (Wir id_Z'Y' ** Mer n') oo barS))
          end
      | wrongterm => 
          raise MalformedBDNF (BgVal.info v, wrongterm, "in Ncom rule")

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
            fun ** (v1, v2) = Ten [v1, v2]
            infix 6 **
            (* Composition constructor, as operator *)
            val oo = Com
            infix 7 oo

            (* id_0 wirings and bgval wirings *)
            val wid_0  = Wiring.id_0
            val vwid_0 = Wir wid_0
            val pid_0  = Permutation.id_0
            val vpid_0 = Per pid_0
            val vpid_1 = Per (Permutation.id_n 1)
 
            val (s, N') = bgvalCom2NBDNF ((Wir id_Z ** N) oo barP)
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
            (s', (Wir id_W ** LS yX') oo N')
          end
      | wrongterm => 
          raise MalformedBDNF (BgVal.info v, wrongterm, "in Pcom rule")

  fun bgval2BBDNF v = (* Bxxx rules *)
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
        fun ** (v1, v2) = Ten [v1, v2]
        infix 6 **
        (* Composition constructor, as operator *)
        val oo = Com
        infix 7 oo

        (* id_0 wirings and bgval wirings *)
        val wid_0  = Wiring.id_0
        val vwid_0 = Wir wid_0
        val pid_0  = Permutation.id_0
        val vpid_0 = Per pid_0
        val vpid_1 = Per (Permutation.id_n 1)
      in
        case match PCns v of
          MMer n => (* Rule Bmer *)
          let
            val cs = List.tabulate
                       (n,
                        (fn _ => (vwid_0 ** vpid_1) 
                                   oo Con NameSet.empty))
            val vpid_n = Per (Permutation.id_n n)
            val N = Abs (NameSet.empty,
                           (vwid_0 ** Mer n)
                             oo Ten cs)
            val P = (vwid_0 ** LS wid_0) oo N
            val D = vwid_0 ** Ten [P] oo vpid_n
          in
            (vwid_0 ** vpid_1) oo D
          end
      
        | MCon X => (* Rule Bcon *)
          let
            val vwid_X = Wir (Wiring.id_X X)
            val vpid_X = Per (Permutation.id [X])
            val N = Abs (NameSet.empty,
                           (vwid_X ** Mer 1)
                             oo Ten [(vwid_X ** vpid_1) oo Con X])
            val P = (vwid_X ** LS wid_0) oo N
            val D = vwid_0 ** Ten [P] oo vpid_X
          in
            (vwid_X ** vpid_1) oo D
          end
      
        | MWir w => (* Rule Bwir *)
          let
            val X = Wiring.innernames w
            val vwid_X = Wir (Wiring.id_X X)
          in
            (v ** vpid_0) oo (vwid_X ** Ten [] oo vpid_0)
          end
      
        | MIon KyX => (* Rule Bion *)
          let
            val X = Ion.innernames KyX
            val Y = Ion.outernames KyX
            val vwid_X = Wir (Wiring.id_X X)
            val vwid_Y = Wir (Wiring.id_X Y)
            val vpid_X = Per (Permutation.id [X])
            val M = (vwid_0 ** v)
                      oo Abs (X,
                              (vwid_X ** Mer 1) 
                                oo Ten [(vwid_X ** vpid_1)
                                          oo Con X])
            val N = Abs (NameSet.empty,
                           (vwid_Y ** Mer 1) oo Ten [M])
            val P = (vwid_Y ** LS wid_0) oo N
            val D = vwid_0 ** Ten [P] oo vpid_X
          in
            (vwid_Y ** vpid_1) oo D
          end
      
        | MPer pi => (* Rule Bper *)
          let
            fun makePi Yi =
                let
                  val wid_Yi  = Wiring.id_X Yi
                  val vwid_Yi = Wir wid_Yi
                  val Ni = Abs (Yi,
                                (vwid_Yi ** Mer 1)
                                  oo Ten [(vwid_Yi ** vpid_1)
                                           oo Con Yi])
                in
                  (vwid_0 ** LS wid_Yi) oo Ni
                end
            val Ys      = Interface.loc (Permutation.outerface pi)
            val vpid_Ys = Per (Permutation.id Ys)
            val Pis     = map makePi Ys
            val D       = vwid_0 ** Ten Pis oo Per pi
          in
            (vwid_0 ** vpid_Ys) oo D
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
               val P = (vwid_WbarX ** LSyzXW') oo N
             in
               (vwzW' ** vpid_U)
                 oo (vwid_0 ** Ten [P] oo Per pid_I)
             end
           | wrongterm =>
             raise
               MalformedBDNF (i, wrongterm, "matching b in rule Babs"))
      
        | MTns bs => (* Rule Bten *)
          let
            fun unzip (b_i, (w, pid_Y, a, pi, Ps)) =
                case match (PCom (PTen [PWir, PPer], PVar))
                           (bgval2BBDNF b_i) of
                  MCom (MTen [MWir w_i, MPer pid_Yi], MVal Di) =>
               (case match (PTen [PWir, PCom (PTns, PPer)]) Di of
                  MTen [MWir a_i, MCom (MTns P_ijs, MPer pi_i)]
                  =>
                  let
                    val w     = Wiring.* (w_i, w)
                    val a     = Wiring.* (a_i, a)
                    val pid_Y = Permutation.* (pid_Yi, pid_Y)
                    val pi    = Permutation.* (pi_i, pi)
                  in
                    (w, pid_Y, a, pi, P_ijs @ Ps)
                  end
                | wrongterm => raise MalformedBDNF
                     (i, wrongterm, "matching Di in rule Bten"))
                | wrongterm => raise MalformedBDNF
                     (i, wrongterm, "matching b_i in rule Bten") 
            val (w, pid_Y, a, pi, Ps)
                = foldr
                    unzip
                    (wid_0, pid_0, wid_0, pid_0, [])
                    bs
            val D = Wir a ** Ten Ps oo Per pi
          in
            (Wir w ** Per pid_Y) oo D
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
                     ((vwid_Z'_i ** P_1i) oo Ten P_2is)
                     :: composeprimes P_1s P_2srest
                   end
                 | composeprimes [] err2s =
                   raise UnequalLength 
                           ([], err2s, "composeprimes in rule Bcom")
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
               val D = Wir wid_U ** Ten Ps oo Per pi
               val V1 = Interface.glob (outerface (Ten P_1s))
               val wid_V1 = Wiring.id_X V1
               val V2 = Interface.glob (outerface (Ten P_2s))
               val wid_V2 = Wiring.id_X V2
               val op o = Wiring.o  infix 7 o
               val x = Wiring.*  infix 6 x
               val w 
                   = w_1 o (a_1 o w_2 o (a_2 x wid_V2) x wid_V1) o s
             in
               (Wir w ** Per pid_U1) oo D
             end
           | wrongtermD2 =>
             raise MalformedBDNF (i, wrongtermD2, "matching D2 in rule Bcom"))
           | wrongtermD1 =>
             raise MalformedBDNF (i, wrongtermD1, "matching D1 in rule Bcom"))
           | wrongtermb2 =>
             raise MalformedBDNF (i, wrongtermb2, "matching b2 in rule Bcom"))
           | wrongtermb1 =>
             raise MalformedBDNF (i, wrongtermb1, "matching b1 in rule Bcom"))
        | wrongtermB =>
          raise MalformedBDNF (i, wrongtermB, "matching in Bxxx rules")
      end

  fun make v = bgval2BBDNF (BgVal.rename_internally v)

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
        fun ** (v1, v2) = Ten [v1, v2]
        infix 6 **
        (* Composition constructor, as operator *)
        val oo = Com
        infix 7 oo

        fun regM M pi =
            let
              val {idxion, N} = unmkM' M
              val N' = regN N pi
            in
              Com (idxion, N')
            end
              
        and regS (S, pi) =
            case unmkS' S of
              SCon' a =>
		if Permutation.is_id pi then
                  S
		else
		  raise IrregularBDNF
                    (i, B, pi, [], "irregular bigraph detected in regS")
            | SMol' M => regM M pi
            
        and regN N pi =
            let
              val {absnames, G}  = unmkN N
              val {idxmerge, Ss} = unmkG G
              val Xss = map (Interface.loc o innerface) Ss
              val {major, minors} = Permutation.split pi Xss
              handle Permutation.NotRegularisable _ =>
                raise IrregularBDNF
                  (i, B, pi, Xss, "irregular bigraph detected during split in regN")
              val Ss' = map regS (ListPair.zip (Ss, minors))
              val Ss'' = Permutation.permute major Ss'
              handle Subscript =>
                raise IrregularBDNF
                  (i, B, pi, Xss, "wrong major permutation detected during permute in regN: "
                  ^ "major=" ^ Permutation.toString major)
            in
              Abs (absnames, Com (idxmerge, Ten Ss''))
            end

        fun regD D =
            let
              fun regPs [] [] _ = []
                | regPs (P_i::Ps) pis pi_offset =
                  let
                    val {idxlocsub, N} = unmkP' P_i
                    val m_i   = Interface.width (innerface P_i)
                    val pis_i = map
                                  (fn (j, ns) => (j - pi_offset, ns))
                                  (take (pis, m_i))
                    val pi_i  = Permutation.make (pis_i)
                    val pisrest = drop (pis, m_i)
                    val N'_i  = regN N pi_i handle e => raise e
                  in
                    ((Com (idxlocsub, N'_i) handle e => raise e)
                    :: (regPs Ps pisrest (pi_offset + m_i)) )handle e => raise e
                  end
                | regPs [] err2s _ =
                  raise LogicalError
                          ("the function regPs in regularize \
                           \was called with uncomposable arguments")

              val {ren, Ps, perm} = unmkD D
              val pi = case match PPer perm of
                         MPer pi => pi
                       | wrongterm =>
                           raise MalformedBDNF 
                             (info perm, wrongterm, "matching perm in regD")
              val pis = Permutation.unmk pi
            in
              Ten [ren, Ten (regPs Ps pis 0)]
              handle Permutation.NotPermutation _ =>
                raise IrregularBDNF
                  (i, B, pi, [], "irregular bigraph detected in regD")
            end
            
      in
        Com (wirxid, regD D)
      end

  fun unmk bdnf = bdnf : bgval

  val info = BgVal.info

  structure Constraints = NameBijectionConstraints

  fun eqM C b1 pi1 b2 pi2 =
      case match (PCom (PTen [PWir, PIon], PVar)) b1 of
        MCom (MTen [MWir idZ1, MIon KyX1], MVal N1) =>
     (case match (PCom (PTen [PWir, PIon], PVar)) b2 of
        MCom (MTen [MWir idZ2, MIon KyX2], MVal N2) =>
     (case eqN C N1 pi1 N2 pi2 of
        SOME CN' =>
        let
          val idZ1_inner_ns = Wiring.innernames idZ1
          val idZ2_inner_ns = Wiring.innernames idZ2
          val KyX1_inner_ns = Ion.innernames KyX1
          val KyX2_inner_ns = Ion.innernames KyX2
        in
          case Constraints.restrict (CN', (idZ1_inner_ns, idZ2_inner_ns)) of
            SOME CidZ =>
         (case Wiring.eq' CidZ idZ1 idZ2 of
            SOME CidZ' =>
         (case Constraints.restrict (CN', (KyX1_inner_ns, KyX2_inner_ns)) of
            SOME CKyX =>
         (case Ion.eq' CKyX KyX1 KyX2 of
            SOME CKyX' => SOME (Constraints.plus (CidZ', CKyX'))
          | NONE => NONE)
          | NONE => NONE)
          | NONE => NONE)
          | NONE => NONE
        end
      | NONE => NONE)
      | wrongterm => NONE) (* b2 is not on M BDNF form *)
      | wrongterm =>
        raise MalformedBDNF
                (info b1, wrongterm, "matching M in eqM")
  and eqS C b1 pi1 b2 pi2 =
      case match (PCom (PTen [PWir, PVar], PCon)) b1 of
        MCom (MTen [MWir a1, _], MCon _) =>
     (case match (PCom (PTen [PWir, PVar], PCon)) b2 of
        MCom (MTen [MWir a2, _], MCon _) =>
        Wiring.eq' C a1 a2
      | wrongterm => NONE) (* b2 is not on (the same) S BDNF form *)
      | _ => eqM C b1 pi1 b2 pi2 (* b1 must be on M BDNF form *)
  and eqG C b1 pi1 b2 pi2 =
      case match (PCom (PTen [PWir, PMer], PTns)) b1 of
        MCom (MTen [MWir idY1, MMer n1], MTns Ss1) =>
     (case match (PCom (PTen [PWir, PMer], PTns)) b2 of
        MCom (MTen [MWir idY2, MMer n2], MTns Ss2) =>
        (* try all permutations of Ss1 *)
        let
          (* test equality of a single permutation *)
          fun eqSs _ [] [] [] [] CSs' = SOME CSs'
            | eqSs Cpi (S1::Ss1) (pi1::pis1) (S2::Ss2) (pi2::pis2) CSs' =
              let
                val S1_inner_ns = Interface.names (innerface S1)
                val S2_inner_ns = Interface.names (innerface S2)
              in
                case Constraints.restrict
                       (Cpi, (S1_inner_ns, S2_inner_ns)) of
                  SOME CS =>
               (case eqS CS S1 pi1 S2 pi2 of
                  SOME CS' => eqSs Cpi Ss1 pis1 Ss2 pis2
                                   (Constraints.plus (CSs', CS'))
                | NONE => NONE)
                | NONE => NONE
              end
            | eqSs _ _ _ _ _ _ = NONE

          val Xss1 = map (Interface.loc o innerface) Ss1
          val Xss2 = map (Interface.loc o innerface) Ss2
          val {group = grouping_pi2, minors = minor_pis2}
            = Permutation.general_split pi2 Xss2

          val perm = Permutation.firstperm_n (length Ss1)

          (* try each permutation in succession *)
          fun try_perm perm =
              let
                val pi    = Permutation.toperm perm
                val Ss1'  = Permutation.permute pi Ss1
                val Xss1' = map (Interface.loc o innerface) Ss1'
                val pi1'  = Permutation.o (Permutation.prod Xss1 pi, pi1)
                val {group = grouping_pi1', minors = minor_pis1'}
                  = Permutation.general_split pi1' Xss1'
              in
                case Permutation.eq' C grouping_pi1' grouping_pi2 of
                  SOME C' => 
               (case eqSs C' Ss1' minor_pis1' Ss2 minor_pis2 Constraints.empty of
                  SOME C'' => SOME C''
                | NONE => try_perm (Permutation.nextperm perm))
                | NONE => try_perm (Permutation.nextperm perm)
              end
              handle Permutation.NoMorePerms => NONE
        in
          try_perm perm
        end
      | wrongterm => NONE) (* b2 is not on G BDNF form *)
      | wrongterm =>
        raise MalformedBDNF
                (info b1, wrongterm, "matching G in eqG")
  and eqN C b1 pi1 b2 pi2 =
      case match (PAbs PVar) b1 of
        MAbs (X1, MVal G1) =>
     (case match (PAbs PVar) b2 of
        MAbs (X2, MVal G2) =>
     (case eqG C G1 pi1 G2 pi2 of
        SOME C' =>
        let
          val allns1 = Interface.names (outerface b1)
          val notX1  = NameSet.difference allns1 X1
          val allns2 = Interface.names (outerface b2)
          val notX2  = NameSet.difference allns1 X2
          val C''    = Constraints.from_list [(X1, X2), (notX1, notX2)]
        in
          if NameSet.size X1 = NameSet.size X2 then
            Constraints.combine (C', C'')
          else 
            NONE
        end
      | NONE => NONE)
      | wrongterm => NONE) (* b2 is not on N BDNF form *)
      | wrongterm =>
        raise MalformedBDNF
                (info b1, wrongterm, "matching N in eqN")
  fun eqP C b1 pi1 b2 pi2 =
      case match (PCom (PTen [PWir, PAbs (PCom (PTen [PWir, PVar], PCon))],
                        PVar)) b1 of
        MCom (MTen [MWir idZ1, MAbs (_, MCom (MTen [MWir yX1, _], MCon X1))],
              MVal N1) =>
     (case match (PCom (PTen [PWir, PAbs (PCom (PTen [PWir, PVar], PCon))],
                        PVar)) b2 of
        MCom (MTen [MWir idZ2, MAbs (_, MCom (MTen [MWir yX2, _], MCon X2))],
              MVal N2) =>
     (case eqN C N1 pi1 N2 pi2 of
        SOME CN' =>
        let
          val idZ1_inner_ns = Wiring.innernames idZ1
          val idZ2_inner_ns = Wiring.innernames idZ2
        in
          case Constraints.restrict (CN', (idZ1_inner_ns, idZ2_inner_ns)) of
            SOME CidZ =>
         (case Wiring.eq' CidZ idZ1 idZ2 of
            SOME CidZ' =>
         (case Constraints.restrict (CN', (X1, X2)) of
            SOME CyX =>
         (case Wiring.eq' CyX yX1 yX2 of
            SOME CyX' => SOME (Constraints.plus (CidZ', CyX'))
          | NONE => NONE)
          | NONE => NONE)
          | NONE => NONE)
          | NONE => NONE
        end
      | NONE => NONE)
      | wrongterm => NONE) (* b2 is not on P BDNF form *)
      | wrongterm =>
        raise MalformedBDNF
                (info b1, wrongterm, "matching P in eqP")
  fun eqD C b1 b2 =
      case match (PTen [PWir, PCom (PTns, PPer)]) b1 of
        MTen [MWir a1, MCom (MTns Ps1, MPer pi1)] =>
     (case match (PTen [PWir, PCom (PTns, PPer)]) b2 of
        MTen [MWir a2, MCom (MTns Ps2, MPer pi2)] =>
        let
          val a1_inner_ns = Wiring.innernames a1
          val a2_inner_ns = Wiring.innernames a2
          val pi1_ns = Interface.names (Permutation.innerface pi1)
          val pi2_ns = Interface.names (Permutation.innerface pi2)
          val {group = grouping_pi1, minors = minor_pis1}
            = Permutation.general_split pi1 (map (Interface.loc o innerface) Ps1)
          val {group = grouping_pi2, minors = minor_pis2}
            = Permutation.general_split pi2 (map (Interface.loc o innerface) Ps2)
        in
          case Constraints.restrict (C, (a1_inner_ns, a2_inner_ns)) of
            SOME Ca =>
         (case Wiring.eq' Ca a1 a2 of
            SOME Ca' =>
         (case Constraints.restrict (C, (pi1_ns, pi2_ns)) of
            SOME Cpi =>
         (case Permutation.eq' Cpi grouping_pi1 grouping_pi2 of
            SOME Cpi' =>
            let
              fun eqPs [] [] [] [] CPs' = SOME CPs'
                | eqPs (P1::Ps1) (pi1::pis1) (P2::Ps2) (pi2::pis2) CPs' =
                  let
                    val P1_inner_ns = Interface.names (innerface P1)
                    val P2_inner_ns = Interface.names (innerface P2)
                  in
                    case Constraints.restrict
                           (Cpi', (P1_inner_ns, P2_inner_ns)) of
                      SOME CP =>
                   (case eqP CP P1 pi1 P2 pi2 of
                      SOME CP' => eqPs Ps1 pis1 Ps2 pis2
                                       (Constraints.plus (CPs', CP'))
                    | NONE => NONE)
                    | NONE => NONE
                  end
                | eqPs _ _ _ _ _ = NONE
            in
              case eqPs Ps1 minor_pis1 Ps2 minor_pis2 Constraints.empty of
                SOME CPs' => SOME (Constraints.plus (Ca', CPs'))
              | NONE => NONE
            end
          | NONE => NONE)
          | NONE => NONE)
          | NONE => NONE)
          | NONE => NONE
        end
      | wrongterm => NONE) (* b2 is not on D BDNF form *)
      | wrongterm =>
        raise MalformedBDNF
                (info b1, wrongterm, "matching D in eqD")
  fun eqB C b1 b2 =
      case match (PCom (PTen [PWir, PPer], PVar)) b1 of
        MCom (MTen [MWir w1, MPer id_X1], MVal D1) =>
     (case match (PCom (PTen [PWir, PPer], PVar)) b2 of
        MCom (MTen [MWir w2, MPer id_X2], MVal D2) =>
        (case eqD C D1 D2 of
           SOME C' =>
           let
             (* restrict C' to the inner names of w1, w2 and id_X1, id_X2
              * before comparing the pairs *)
             val w1_inner_ns = Wiring.innernames w1
             val w2_inner_ns = Wiring.innernames w2
             val id_X1_inner_ns = Interface.names (Permutation.innerface id_X1)
             val id_X2_inner_ns = Interface.names (Permutation.innerface id_X2)
           in
             case Constraints.restrict (C', (w1_inner_ns, w2_inner_ns)) of
               SOME Cw =>
            (case Wiring.eq' Cw w1 w2 of
               SOME Cw' =>
            (case Constraints.restrict (C', (id_X1_inner_ns, id_X2_inner_ns)) of
               SOME Cid_X =>
            (case Permutation.eq' Cid_X id_X1 id_X2 of
               SOME Cid_X' => SOME (Constraints.plus (Cw', Cid_X'))
             | NONE => NONE)
             | NONE => NONE)
             | NONE => NONE)
             | NONE => NONE
           end
         | NONE => NONE)
      | wrongterm => NONE) (* b2 is not on B BDNF form *)
      | wrongterm =>
        raise MalformedBDNF
                (info b1, wrongterm, "matching B in eqB")
  fun eqDR C b1 b2 =
      (* convert to, and compare on, D form *)
      case match (PTen [PWir, PTns]) b1 of
        MTen [MWir a1, MTns Ps1] =>
     (case match (PTen [PWir, PTns]) b2 of
        MTen [MWir a2, MTns Ps2] =>
        let
          val Ps1_loc_names
            = (List.concat o map (Interface.loc o innerface)) Ps1
          val Ps2_loc_names
            = (List.concat o map (Interface.loc o innerface)) Ps2
          val pi1 = Permutation.id Ps1_loc_names
          val pi2 = Permutation.id Ps2_loc_names
          val i = noinfo
          val D1 = Ten i [Wir i a1, Com i (Ten i Ps1, Per i pi1)]
          val D2 = Ten i [Wir i a2, Com i (Ten i Ps2, Per i pi2)]
        in
          eqD C D1 D2
        end
      | wrongterm => NONE) (* b2 is not on D BDNF form *)
      | wrongterm =>
        raise MalformedRBDNF
                (info b1, wrongterm, "matching DR in eqDR")
  fun eqBR C b1 b2 =
      case match (PCom (PTen [PWir, PPer], PVar)) b1 of
        MCom (MTen [MWir w1, MPer id_X1], MVal DR1) =>
     (case match (PCom (PTen [PWir, PPer], PVar)) b2 of
        MCom (MTen [MWir w2, MPer id_X2], MVal DR2) =>
        (case eqDR C DR1 DR2 of
           SOME C' =>
           let
             (* restrict C' to the inner names of w1, w2 and id_X1, id_X2
              * before comparing the pairs *)
             val w1_inner_ns = Wiring.innernames w1
             val w2_inner_ns = Wiring.innernames w2
             val id_X1_inner_ns = Interface.names (Permutation.innerface id_X1)
             val id_X2_inner_ns = Interface.names (Permutation.innerface id_X2)
           in
             case Constraints.restrict (C', (w1_inner_ns, w2_inner_ns)) of
               SOME Cw =>
            (case Wiring.eq' Cw w1 w2 of
               SOME Cw' =>
            (case Constraints.restrict (C', (id_X1_inner_ns, id_X2_inner_ns)) of
               SOME Cid_X =>
            (case Permutation.eq' Cid_X id_X1 id_X2 of
               SOME Cid_X' => SOME (Constraints.plus (Cw', Cid_X'))
             | NONE => NONE)
             | NONE => NONE)
             | NONE => NONE)
             | NONE => NONE
           end
         | NONE => NONE)
      | wrongterm => NONE) (* b2 is not on BR BDNF form *)
      | wrongterm =>
        raise MalformedBDNF
                (info b1, wrongterm, "matching BR in eqBR")

  (* determine which normal form b1 matches and call the right function *)
  fun eq' C b1 b2 =
      case match PCns b1 of
        MCom (MVal com1, MVal com2) => (* M, S, G, P, B, or BR *)
        (case match PCns com2 of
           MCon _ => eqS C b1 (Permutation.id (Interface.loc (innerface b1)))
                           b2 (Permutation.id (Interface.loc (innerface b2)))
         | MAbs _ => (* M or P *)
           (case match (PTen [PWir, PCns]) com1 of
              MTen [_, MIon _] =>
              eqM C b1 (Permutation.id (Interface.loc (innerface b1)))
                    b2 (Permutation.id (Interface.loc (innerface b2)))
            | MTen [_, MAbs _] =>
              eqP C b1 (Permutation.id (Interface.loc (innerface b1)))
                    b2 (Permutation.id (Interface.loc (innerface b2)))
            | wrongterm =>
              raise MalformedBDNF
                      (info b1, wrongterm, "matching M or P in eq"))
         | MTns tns => (* G, B, or BR *)
           (case match (PTen [PWir, PCns]) com1 of
              MTen [_, MMer _] =>
              eqG C b1 (Permutation.id (Interface.loc (innerface b1)))
                    b2 (Permutation.id (Interface.loc (innerface b2)))
            | MTen [_, MPer _] => (* B or BR *)
              (case match PCns (List.nth (tns, 1)) of
                 MCom _ => eqB C b1 b2  (* B *)
               | MTns _ => eqBR C b1 b2 (* BR *)
               | wrongterm =>
                 raise MalformedBDNF
                         (info b1, wrongterm, "matching B or BR in eq"))
            | wrongterm =>
              raise MalformedBDNF
                      (info b1, wrongterm, "matching G, B or BR in eq"))
         | wrongterm =>
           raise MalformedBDNF
                   (info b1, wrongterm, "matching M, S, G, P, B, or BR in eq"))
      | MAbs _ => eqN C b1 (Permutation.id (Interface.loc (innerface b1)))
                        b2 (Permutation.id (Interface.loc (innerface b2)))
      | MTns tns => (* D, DR *)
        (case match PCns (List.nth (tns, 1)) of
           MCom _ => eqD C b1 b2  (* D *)
         | MTns _ => eqDR C b1 b2 (* DR *)
         | wrongterm =>
           raise MalformedBDNF
                   (info b1, wrongterm, "matching D or DR in eq"))
      | wrongterm =>
        raise MalformedBDNF (info b1, wrongterm, "matching in eq")

  fun eq b1 b2 =
      let
        val iface1 = innerface b1
        val iface2 = innerface b2
        val oface1 = outerface b1
        val oface2 = outerface b2
        val X      = Interface.names iface1
        val Y      = Interface.names oface1
        val Ci     = NameSet.fold
                       (fn x => fn Ci =>
                        let val X = NameSet.singleton x
                        in Constraints.add ((X,X), Ci) end)
                       Constraints.empty X
        val Co     = NameSet.fold
                       (fn y => fn Co =>
                        let val Y = NameSet.singleton y
                        in Constraints.add ((Y,Y), Co) end)
                       Constraints.empty Y
      in
        if Interface.eq (iface1, iface2)
          andalso Interface.eq (oface1, oface2)
        then
          case eq' Ci b1 b2 of
            SOME Co' => Constraints.are_combineable (Co, Co')
          | NONE     => false
        else 
          false
      end

  fun pp indent pps
    = BgVal.pp indent pps o unmk
  fun oldpp indent pps
    = BgVal.oldpp indent pps o unmk

  fun ppWithIface indent pps
    = BgVal.ppWithIface indent pps o unmk

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
             
  val toString
    = PrettyPrint.pp_to_string
        (Flags.getIntFlag "/misc/linewidth") 
        (pp (Flags.getIntFlag "/misc/indent"))

  val size = BgVal.size o unmk
  val revision
    = hd (String.tokens (not o Char.isDigit) "$LastChangedRevision$")
    
end

functor BgBDNF (structure Info : INFO
		structure Name : NAME
		structure NameSet : MONO_SET
                structure NameBijectionConstraints : BIJECTION_CONSTRAINTS
		structure LinkSet : MONO_SET
		structure Interface : INTERFACE
		structure Ion : ION
		structure Permutation : PERMUTATION
		structure Link : LINK
		structure Wiring : WIRING
		structure BgVal : BGVAL
		structure ErrorHandler : ERRORHANDLER
                  where type ppstream    = PrettyPrint.ppstream
                    and type break_style = PrettyPrint.break_style
                    and type origin      = Origin.origin
		structure NameSetPP : COLLECTIONPRETTYPRINT
                  where type ppstream    = PrettyPrint.ppstream
		structure ListPP : POLYCOLLECTIONPRETTYPRINT
                  where type ppstream      = PrettyPrint.ppstream
                    and type 'a collection = 'a list
		sharing type NameSet.Set = NameSetPP.collection
		sharing type BgVal.interface = 
			     Interface.interface =
			     Permutation.interface
		sharing type BgVal.ion = Ion.ion
		sharing type BgVal.permutation =
			     Permutation.permutation
                sharing type BgVal.Immutable =
			     Permutation.Immutable
		sharing type BgVal.wiring = Wiring.wiring
		sharing type NameSet.Set =
                             NameBijectionConstraints.set =
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
		sharing type NameSet.elt =
		             Link.name =
                             Name.name =
                             Ion.name
                sharing type NameBijectionConstraints.constraints =
                             Ion.nameconstraints =
                             Permutation.nameconstraints =
                             Wiring.nameconstraints
                sharing type Info.info =
                             BgVal.info
			     ) :> BGBDNF 
  where type nameset        = NameSet.Set
    and type info           = Info.info 
    and type interface      = BgVal.interface
    and type wiring         = BgVal.wiring
    and type ion            = Ion.ion
    and type Immutable      = BgVal.Immutable
    and type 'a permutation = 'a BgVal.permutation
    and type bgval          = BgVal.bgval 
    and type bgmatch        = BgVal.bgmatch =
struct
  structure BgBDNF = BgBDNF'(structure Info = Info
			     structure Name = Name
			     structure NameSet = NameSet
			     structure NameBijectionConstraints = NameBijectionConstraints
			     structure LinkSet = LinkSet
			     structure Interface = Interface
			     structure Ion = Ion
			     structure Permutation = Permutation
			     structure Link = Link
			     structure Wiring = Wiring
			     structure BgVal = BgVal
			     structure ErrorHandler = ErrorHandler
			     structure NameSetPP = NameSetPP
			     structure ListPP = ListPP)
  open BgBDNF
end