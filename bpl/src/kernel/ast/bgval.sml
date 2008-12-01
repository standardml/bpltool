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

(** Abstract data type for bigraph values, i.e., well-formed bigraph
 * terms with interfaces.
 * @version $LastChangedRevision$
 *)
functor BgVal'(structure Info : INFO
	       structure Name : NAME
	       structure NameSet : MONO_SET
               structure NameBijectionConstraints : BIJECTION_CONSTRAINTS
	       structure Link : LINK
	       structure LinkSet : MONO_SET
	       structure Control : CONTROL
	       structure Ion : ION
	       structure Permutation : PERMUTATION
	       structure Wiring : WIRING
	       structure Interface : INTERFACE
	       structure BgTerm : BGTERM
	       structure ErrorHandler : ERRORHANDLER
                 where type ppstream    = PrettyPrint.ppstream
                   and type break_style = PrettyPrint.break_style
                   and type origin      = Origin.origin
	       structure NameSetPP : COLLECTIONPRETTYPRINT
                 where type ppstream    = PrettyPrint.ppstream
	       sharing type Name.name = 
			    NameSet.elt = 
			    Link.name =
			    Ion.name =
			    Wiring.name
	       sharing type NameSet.Set =
			    Name.NameSet.Set =
                            NameBijectionConstraints.set =
			    Link.nameset =
			    Ion.nameset =
			    Permutation.nameset =
			    Wiring.nameset =
			    Interface.nameset =
			    BgTerm.nameset =
                            NameSetPP.collection
               sharing type Control.control =
                            Ion.control
               sharing type Link.link = 
			    LinkSet.elt =
			    Wiring.link
               sharing type LinkSet.Set = Wiring.linkset
	       sharing type Ion.ion = BgTerm.ion
	       sharing type Permutation.permutation =
			    BgTerm.permutation
	       sharing type Permutation.Immutable =
			    BgTerm.Immutable
	       sharing type Wiring.wiring = BgTerm.wiring
               sharing type NameBijectionConstraints.constraints =
                            Ion.nameconstraints =
                            Permutation.nameconstraints =
                            Wiring.nameconstraints
	       sharing type Interface.interface =
			    Permutation.interface
               sharing type Info.info =
                            BgTerm.info
			    ) : BGVAL 
  where type nameset = NameSet.Set
    and type name = Name.name
    and type wiring = Wiring.wiring 
    and type 'kind permutation = 'kind Permutation.permutation
    and type Immutable = Permutation.Immutable
    and type ion = Ion.ion
    and type interface = Interface.interface 
    and type bgterm = BgTerm.bgterm
    and type info = Info.info =
struct
  type info = Info.info
  type name = Name.name
  type nameset = NameSet.Set
  type bgterm = BgTerm.bgterm
  type interface = Interface.interface
  type Immutable = Permutation.Immutable
  type 'kind permutation = 'kind Permutation.permutation
  type control = Control.control
  type ion = Ion.ion
  type wiring = Wiring.wiring

  open Debug
  open ErrorHandler

  val file_origin = Origin.mk_file_origin
                      "$BPL/src/kernel/ast/bgval.sml"
                      Origin.NOPOS
  fun mk_explainer errtitle (explainer : exn -> explanation list) e =
      Exp (LVL_USER, Origin.unknown_origin, mk_string_pp errtitle,
           explainer e)

  (* just a shorthand (never raised) *)
  exception DuplicatesRemoved = NameSet.DuplicatesRemoved

  (** The bgval data type. *)
  datatype bgval =
	 (** merge_n = one root containing n sites. *)
	   VMer of int * info
	 (** "X" = a concretion of a set of names. *)
	 | VCon of nameset * info
	 (** w = a general wiring. *)
	 | VWir of wiring * info
	 (** K_yX = an ion. *)
	 | VIon of ion * info
	 (** pi = a permutation. *)
	 | VPer of Immutable permutation * info
	 (** (X)P = an abstraction. *)
	 | VAbs of nameset * bgval * (interface * interface * info)
	 (** b_1 x...x b_n-1 = a tensor product of n bigraphs. *)
	 | VTen of bgval list * (interface * interface * info)
	 (** b_1 b_2 = a composition of a pair of bigraphs. *)
	 | VCom of bgval * bgval * (interface * interface * info)
   (* NB: VPar and VPri are only for internal use - the must not leave the
    *     module!
    *     They are used when converting to the bgterm datatype for
    *     prettyprinting. *)
   (** b_1 ||...|| b_n-1 = a parallel product of n bigraphs. *)
   | VPar of bgval list * (interface * interface * info)
   (** b_1 |...| b_n-1 = a prime product of n bigraphs. *)
   | VPri of bgval list * (interface * interface * info)

  fun info (VMer (_, i))            = i
    | info (VCon (_, i))            = i
    | info (VWir (_, i))            = i
    | info (VIon (_, i))            = i
    | info (VPer (_, i))            = i
    | info (VAbs (_, _, (_, _, i))) = i 
    | info (VTen (_, (_, _, i)))    = i
    | info (VPar (_, (_, _, i)))    = i
    | info (VPri (_, (_, _, i)))    = i
    | info (VCom (_, _, (_, _, i))) = i


  fun innerface (VMer (n, i))   = Interface.m n
    | innerface (VCon (X, i)) 
      = Interface.make {loc = [X], glob = NameSet.empty}
    | innerface (VWir (w, i))   = Interface.X (Wiring.innernames w)
    | innerface (VIon (KyX, i)) 
      = Interface.make {loc = [Ion.innernames KyX], glob = NameSet.empty}
    | innerface (VPer (pi, _))  = Permutation.innerface pi
    | innerface (VAbs (_, _, (innf, _, _))) = innf
    | innerface (VTen (_, (innf, _, _)))    = innf
    | innerface (VPar (_, (innf, _, _)))    = innf
    | innerface (VPri (_, (innf, _, _)))    = innf
    | innerface (VCom (_, _, (innf, _, _))) = innf

  fun outerface (VMer (_, i))   = Interface.one
    | outerface (VCon (X, i)) 
      = Interface.make {loc = [NameSet.empty], glob = X}
    | outerface (VWir (w, i))   = Interface.X (Wiring.outernames w)
    | outerface (VIon (KyX, i)) =
      let
	val {free, ...} = Ion.unmk KyX
	val y = NameSet.fromList' free
      in
	Interface.make {loc = [NameSet.empty], glob = y}
      end
    | outerface (VPer (pi, _))  = Permutation.outerface pi
    | outerface (VAbs (_, _, (_, outf, _))) = outf
    | outerface (VTen (_, (_, outf, _)))    = outf
    | outerface (VPar (_, (_, outf, _)))    = outf
    | outerface (VPri (_, (_, outf, _)))    = outf
    | outerface (VCom (_, _, (_, outf, _))) = outf

  fun arecomposable v1 v2 = Interface.eq (innerface v1, outerface v2)

  val unmk =
      let
	fun unmk' (v as (VMer (n, i)))
	    = ((BgTerm.Mer (n, (info v)), Interface.m n, Interface.one) handle e => raise e)
	  | unmk' (v as (VCon (X, i)))
	    = ((BgTerm.Con (X, (info v)), 
	       Interface.make {loc = [X], 
			       glob = NameSet.empty},
	       Interface.make {loc = [NameSet.empty], glob = X}) handle e => raise e)
	  | unmk' (v as (VWir (w, i)))
	    = ((BgTerm.Wir (w, (info v)),
	       Interface.X (Wiring.innernames w),
	       Interface.X (Wiring.outernames w)) handle e => raise e)
	  | unmk' (v as (VIon (KyX, i)))
	    = ((BgTerm.Ion (KyX, (info v)),
	       Interface.make {loc = [Ion.innernames KyX],
			       glob = NameSet.empty},
	       Interface.make {loc = [NameSet.empty], glob = Ion.outernames KyX}) handle e => raise e)
	  | unmk' (v as (VPer (pi, i)))
	    = ((BgTerm.Per (pi, (info v)), 
	       Permutation.innerface pi,
	       Permutation.outerface pi) handle e => raise e)
	  | unmk' (v as (VAbs (X, v', (innf, outf, i)))) 
	    = ((BgTerm.Abs (X, #1 (unmk' v'), (info v)), innf, outf) handle e => raise e)
	  | unmk' (v as (VTen (vs, (innf, outf, i)))) 
	    = ((BgTerm.Ten (List.map (#1 o unmk') vs, (info v)), innf, outf) handle e => raise e)
	  | unmk' (v as (VPar (vs, (innf, outf, i)))) 
	    = ((BgTerm.Par (List.map (#1 o unmk') vs, (info v)), innf, outf) handle e => raise e)
	  | unmk' (v as (VPri (vs, (innf, outf, i)))) 
	    = ((BgTerm.Pri (List.map (#1 o unmk') vs, (info v)), innf, outf) handle e => raise e)
	  | unmk' (v as (VCom (v1, v2, (innf, outf, i)))) 
	    = ((BgTerm.Com (#1 (unmk' v1), #1 (unmk' v2), (info v)),
	       innf,
	       outf) handle e => raise e)
      in
	unmk'
      end

exception NotImplemented of string
fun explain_NotImplemented (NotImplemented errtxt) =
    [Exp (LVL_LOW, file_origin, mk_string_pp errtxt, [])]
  | explain_NotImplemented _ = raise Match
val _ = add_explainer
          (mk_explainer "feature not implemented" explain_NotImplemented)

(* Determine whether a (X)v is the identity. *)
fun is_concretion_of X (VMer (1, _))      = NameSet.isEmpty X
  | is_concretion_of X (VMer _)           = false
  | is_concretion_of X (VCon (Y, _))      = NameSet.eq X Y
  | is_concretion_of X (VWir _)           = false
  | is_concretion_of X (VIon _)           = false
  | is_concretion_of X (VPer (pi, _))     = NameSet.isEmpty X andalso
					                                  Permutation.is_id pi
  | is_concretion_of X (VAbs (Y, v, _))  
    = is_concretion_of (NameSet.union X Y) v
  | is_concretion_of X (VTen ([v], _))    = is_concretion_of X v
  | is_concretion_of _ (VTen _)           = false
  | is_concretion_of X (VPar ([v], _))    = is_concretion_of X v
  | is_concretion_of _ (VPar _)           = false
  | is_concretion_of X (VPri ([v], _))    = is_concretion_of X v
  | is_concretion_of _ (VPri _)           = false
  | is_concretion_of X (VCom (v1, v2, _))
    = raise NotImplemented "is_concretion_of for composition"

and is_concretion_of' X v 
  = is_concretion_of X v handle NotImplemented _ => false


and is_id (VMer (1, _))      = true
  | is_id (VMer _)           = false
  | is_id (VCon (X, _))      = NameSet.isEmpty X
  | is_id (VWir (w, _))      = Wiring.is_id w
  | is_id (VIon _)           = false
  | is_id (VPer (pi, _))     = Permutation.is_id pi
  | is_id (VAbs (X, v, _))   = is_concretion_of' X v
  | is_id (VTen (vs, _))     = List.all is_id vs
  | is_id (VPar (vs, _))     = List.all is_id vs
  | is_id (VPri ([v], _))    = is_id v
  | is_id (VPri _)           = false
  | is_id (VCom (v1, v2, _))
    = raise NotImplemented "is_id for composition"

fun is_id' v = is_id v handle NotImplemented _ => false

fun is_id0 (VMer (1, _))      = false
  | is_id0 (VMer _)           = false
  | is_id0 (VCon (X, _))      = false
  | is_id0 (VWir (w, _))      = Wiring.is_id0 w
  | is_id0 (VIon _)           = false
  | is_id0 (VPer (pi, _))     = Permutation.is_id0 pi
  | is_id0 (VAbs (X, v, _))   = false
  | is_id0 (VTen (vs, _))     = List.all is_id0 vs
  | is_id0 (VPar (vs, _))     = List.all is_id0 vs
  | is_id0 (VPri _)           = false
  | is_id0 (VCom (v1, v2, _))
    = raise NotImplemented "is_id0 for composition"

fun is_id0' v = is_id0 v handle NotImplemented _ => false

  fun simplify (VAbs (X, v, ioi as (_, _, i))) = 
    (* ({})P -> P
     * (X)(X')P -> (X u X')P
     * (X)"X" -> id_(X)
     *)
    if NameSet.isEmpty X then
      simplify v
    else
	  	(case simplify v of
	  	   VAbs (X', v', ioi') => VAbs (NameSet.union' X X', v', ioi)
	  	 | v' as (VCon (X', i'))
	  	 => if NameSet.eq X X' then
	  	      VPer (Permutation.id [X], i)
	  	    else
	  	      VAbs (X, v', ioi)
	  	 | v' => VAbs (X, v', ioi))
	  | simplify (VTen (vs, ioi as (_, _, i))) =
	  (* vsl * Ten vs' * vsr -> vsl * vs' * vsr
	   * vsl * id_0 * vsr -> vsl * vsr
	   * vs0 * w1 * vs1 * w2 * ... * wn * vsn
	   *  -> [[w1 * ... * wn]] * vs0 * ... * vsn
	   * vsl * pi1 * pi2 * vsr -> vsl * [[pi1 * pi2]] * vsr
     * Ten [v] -> v
	   *)
	    let
	      val vs = map simplify vs
	      val vs
	        = foldr
	            (fn (VTen (vs', _), vs) => vs' @ vs
	              | (v, vs) => v :: vs)
	            []
	            vs
	      val vs = List.filter (not o is_id0') vs
	      fun extractwiring [] = (Wiring.id_0, NONE, [])
	        | extractwiring (v :: vs) =
	          let
	            val (w', i', vs') = extractwiring vs
	          in
	            case v of
	              VWir (w, i'')
	            => (Wiring.* (w, w'),
	                SOME (case i' of SOME _ => i | NONE => i''),
	                vs')
	            | v => (w', i', v :: vs')
	          end
	      val (w, iw, vs) = extractwiring vs
	      val vs
	       = if Wiring.is_id0 w then
	           vs
	         else
	           VWir (w, case iw of SOME i' => i' | NONE => i) :: vs
	      fun mergepis [] = []
	        | mergepis ((v as (VPer (pi, i))) :: vs) =
	          (case mergepis vs of
	             (VPer (pi', i') :: vs') => (VPer (Permutation.* (pi, pi'), i) :: vs')
	           | vs' => v :: vs')
	        | mergepis (v :: vs) = v :: mergepis vs
	      val vs = mergepis vs
	    in
	      case vs of
	        [v] => v
	      | vs => VTen (vs, ioi)
	    end
    | simplify (VPar vs) =
      (case simplify (VTen vs) of
         VTen vs' => VPar vs'
       | v'       => v')
	  | simplify (VCom (v1, v2, ioi as (_, _, i))) =
	  (* "X"(X)P -> P
	   * w1 w2 -> [[w1 w2]]
	   * pi1 pi2 -> [[pi1 pi2]]
	   * id v -> v
	   * v id -> v
	   * (v1 * v2)(v1' * v2') -> v1 v1' * v2 v2', if vi : Ii ->, vi' : -> Ii
		 * (alpha * id_1) K_y(X) -> K_{alpha(y)}(X)
		 * (alpha * id_1) (beta * K_y(X)) -> alpha' beta * K_{alpha(y)}(X) [not implemented]
		 * (w1 * id_n) (w2 * v) -> w1 w2 * v, if v : -> n        [not implemented]
		 * (A o B) o C -> A o (B o C)    (for nicer prettyprinting)
	   *)
	    let
	      val v1' = simplify v1
	      val v2' = simplify v2
	    in
	      if is_id' v1' then
	        v2'
	      else if is_id' v2' then
	        v1'
	      else case (v1', v2') of
	        (VCon (X, _), VAbs (X', v, _))
	      => if NameSet.eq X X' then v else VCom (v1', v2', ioi)
	      | (VWir (w1, _), VWir (w2, _)) => VWir (Wiring.o (w1, w2), i)
	      | (VPer (pi1, _), VPer (pi2, _))
	      => VPer (Permutation.o (pi1, pi2), i)
	      | (VTen (vs1, (innf1, _, _)), VTen (vs2, _)) =>
          (case (vs1, vs2) of
             ([VWir (w1, _), v1], [VWir (w2, _), v2]) =>
             if is_id' v1
             andalso NameSet.isEmpty (Interface.names (outerface v2)) then
               VTen ([VWir (Wiring.o (w1, w2), i), v2], ioi)
             else
               if NameSet.eq
                    (Wiring.innernames w1) (Wiring.outernames w2) then
                 simplify
                   (VTen ([VWir (Wiring.o (w1, w2), i),
                           VCom (v1, v2, (innerface v2, outerface v1, i))],
                          ioi))
               else
                 VCom (v1', v2', ioi)
           | _ =>
	           if NameSet.isEmpty (Interface.glob innf1) then
		           let
		             fun mkiface is = foldr Interface.* Interface.zero (rev is)
	               fun mkinterface face = foldr Interface.* Interface.zero o map face 
	               fun add (vs1, is1, os1) (vs2, is2, os2) rest =
	                 let
	                   val i2 = mkiface is2
	                   val o1 = mkiface os1
	                 in
	                   simplify
	                     (VCom (VTen (rev vs1, (mkiface is1, o1, i)),
	                            VTen (rev vs2, (i2, mkiface os2, i)),
	                            (i2, o1,i)))
	                   :: rest
	                 end
	               (* distrib first (vs1, m1, X1, is1, os1) (vs2, n2, Y2, is2, os2) vs1' vs2'
	                * takes the shortest sublists of vs1' and vs2' whose interfaces
	                * match, and composes them.
	                * vs1 accumulates elements from vs1' not yet composed with
	                * elements from vs2', its interfaces is described by is1 and os1;
	                * m1 is the width of vs1, X1 the global inner names not yet
	                * matched by global outer names of vs2'.
	                * vs2 is used analogously.
	                *)
		             fun distrib first (vs1, _, _, is1, os1) (vs2, _, _, is2, os2) [] []
			             = let
			                 val i2 = mkiface is2
			                 val o1 = mkiface os1
			               in
			                 (if first then (fn x => x) else simplify) (* avoid looping *)
			                 (VCom (VTen (rev vs1, (mkiface is1, o1, i)),
			                        VTen (rev vs2, (i2, mkiface os2, i)),
			                        (i2, o1, i)))
			                 :: []
			               end
		               | distrib _ (vs1, m1, X1, is1, os1) (vs2, n2, Y2, is2, os2)
		                           (v1 :: vs1') (v2 :: vs2')
		               = let
		                   val iface1' = innerface v1
		                   val oface2' = outerface v2
		                   val m1' = Interface.width (iface1')
		                   val n2' = Interface.width (oface2')
                       val X1' = Interface.glob (iface1') 
                       val Y2' = Interface.glob (oface2')
                       val X1''= NameSet.union' X1 X1' 
                       val Y2''= NameSet.union' Y2 Y2'
                       val X1''minusY2 = NameSet.difference X1'' Y2
                       val Y2minusX1'' = NameSet.difference Y2 X1''
		                 in
		                   if m1 + m1' < n2 then
		                     distrib false 
		                       (v1 :: vs1, m1 + m1', X1''minusY2,
		                        iface1' :: is1, outerface v1 :: os1)
		                       (vs2, n2, Y2minusX1'', is2, os2) vs1' (v2 :: vs2')
		                   else if m1 + m1' = n2 (* Will faces match if we add v1? *)
		                        andalso NameSet.isEmpty X1''minusY2
		                        andalso NameSet.isEmpty Y2minusX1'' then
		                     add (v1 :: vs1, iface1' :: is1, outerface v1 :: os1)
		                         (vs2, is2, os2) 
		                         (distrib false ([], 0, NameSet.empty, [], [])
		                                        ([], 0, NameSet.empty, [], [])
		                                        vs1' (v2 :: vs2'))
		                   else 
		                     let
                           val X1minusY2'' = NameSet.difference X1 Y2''
                           val Y2''minusX1 = NameSet.difference Y2'' X1
		                     in
		                       if n2 + n2' < m1 then
		                         distrib false 
		                           (vs1, m1, X1minusY2'', is1, os1)
		                           (v2 :: vs2, n2 + n2', Y2''minusX1,
		                            innerface v2 :: is2, oface2' :: os2)
		                           (v1 :: vs1') vs2'
		                       else if n2 + n2' = m1 (* Facematch if we add v2? *)
		                            andalso NameSet.isEmpty X1minusY2''
		                            andalso NameSet.isEmpty Y2''minusX1 then
		                         add (vs1, is1, os1)
		                             (v2 :: vs2, innerface v2 :: is2, oface2' :: os2)
		                             (distrib false ([], 0, NameSet.empty, [], []) 
		                                            ([], 0, NameSet.empty, [], [])
		                                            (v1 :: vs1') vs2')
		                       else
		                         let
                               val X1''minusY2'' = NameSet.difference X1'' Y2''
                               val Y2''minusX1'' = NameSet.difference Y2'' X1''
                             in
 		                           if n2 + n2' = m1 + m1' (* Facematch adding both? *)
		                           andalso NameSet.isEmpty X1''minusY2''
		                           andalso NameSet.isEmpty Y2''minusX1'' then
		                             add (v1 :: vs1, iface1' :: is1, outerface v1 :: os1)
		                                 (v2 :: vs2, innerface v2 :: is2, oface2' :: os2)
		                                 (distrib false
		                                   ([], 0, NameSet.empty, [], [])
		                                   ([], 0, NameSet.empty, [], []) vs1' vs2')
		                           else
		                             distrib false
		                               (v1 :: vs1, m1 + m1', X1''minusY2'',
		                                iface1' :: is1, outerface v1 :: os1)
		                               (v2 :: vs2, n2 + n2', Y2''minusX1'',
		                                innerface v2 :: is2, oface2' :: os2)
		                               vs1' vs2'
		                         end
		                     end
	                   end
		               | distrib _ (vs1, _, _, is1, os1) (vs2, _, _, is2, os2) [] vs2'
		               = let
		                   val iface2' = mkinterface innerface (rev vs2')
		                   val oface2' = mkinterface outerface (rev vs2')
		                 in
	                     add (vs1, is1, os1)
	                         (rev vs2' @ vs2, iface2' :: is2, oface2' :: os2)
	                         []
	                   end
		               | distrib _ (vs1, _, _, is1, os1) (vs2, _, _, is2, os2) vs1' []
		               = let
		                   val iface1' = mkinterface innerface (rev vs1')
		                   val oface1' = mkinterface outerface (rev vs1')
		                 in
	                     add (rev vs1' @ vs1, iface1' :: is1, oface1' :: os1)
	                         (vs2, is2, os2)
	                         []
	                   end
	               val vs = distrib true ([], 0, NameSet.empty, [], [])
	                                     ([], 0, NameSet.empty, [], []) vs1 vs2
		           in
		             simplify (* Does this loop??? *)
		               (VTen (vs, (mkinterface innerface vs,
		                           mkinterface outerface vs,
	                             i)))
		           end
	           else
	             VCom (v1', v2', ioi))
	      | (VTen ([VWir (w, _), v], _), VIon (KyX, _))
	      => if is_id' v andalso Wiring.is_renaming w then
	           let
	             val {ctrl, free, bound} = Ion.unmk KyX
	             val free = map (Option.valOf o Wiring.app_x w) free
	             val KyX
	               = Ion.make {ctrl = ctrl, free = free, bound = bound}
	           in
	             VIon (KyX, i)
	           end
	         else
	           VCom (v1', v2', ioi)
          (* FIXME somethings wrong with the interfaces: *)
	      | (VCom (v1a, v1b, _), v2') => VCom (v1a, VCom (v1b, v2', ioi), ioi)
	      | (v1', v2') => VCom (v1', v2', ioi)
	    end   
	  | simplify (VWir (w, i)) = VWir (Wiring.removeidles w, i)              
    | simplify v = v

  (* t2p'' : (bgval, substitution) -> ((bgval, isNewId?), newSubstitution)
   *
   * Note: Take care - I am using a global wiring (substition) s as a
   * general namemap, that may apply to both global and local wiring.
   * 
   * FIXME the bgvals produced are not necessarilly "legal" bgvals - e.g. an ion
   *       can have the same name connected to more than one port.
   *       This is not a problem as long as we don't let these bgvals leave the
   *       module - e.g. by converting to bgterm where the terms would be legal.
   *
   * TODO: NOT FULLY TESTED. *)
  fun t2p'' (v as (VMer (n, i)), s) =
      ((v, false), s) (* s = id_e*)
     | t2p'' (VCon (X, i), s) =
      (* FIXME this is unsound!
       * E.g. x//[y, z] o (`[y]` * `[z]`)   =>   `[x]` || `[x]`
       *)
       (((VCon (Wiring.app s X, i), false), s) handle e => raise e)
    | t2p'' (VWir (w, i), s) =
      (* w = y_1/X_1 * ... * y_n/X_n * /Z_1 * ... * /Z_m
         s = z_1/y_1 || ... || z_n/y_n

         cases:  X_i = Ø  =>  l_i = z_i/       l'_i = id0
               | _        =>  l_i = z_i/z_i    l'_i = z_i/X_i

         w' = ... || l_i  || ... * /Z_1 * ... * /Z_m
         s' = ... || l'_i || ... * id_Z_1 * ... * id_Z_m       *)
      let
        fun rename_link l (w', s') =
            case Link.unmk l of
              {outer = NONE, inner = Z}   =>
              (Wiring.* (w', Wiring.make' [l]), Wiring.* (s', Wiring.id_X Z))
            | {outer = SOME y, inner = X} =>
              let
                val z = valOf (Wiring.app_x s y)
              in
                if NameSet.isEmpty X then
                  (Wiring.|| (w', Wiring.introduce (NameSet.singleton z)), s')
                else
                  (Wiring.|| (w', Wiring.id_X (NameSet.singleton z)),
                   Wiring.|| (s', Wiring.make'
                                    [Link.make {outer = SOME z, inner = X}]))
              end
                
        val (w', s') = LinkSet.fold
                         rename_link
                         (Wiring.id_0, Wiring.id_0)
                         (Wiring.unmk w)
      in
        (* Only new identities should be removed - FIXME right?*)
        if Wiring.is_id w' andalso not (Wiring.is_id w) then
          ((VWir (w', i), true), s')
        else
          ((VWir (w', i), false), s')
      end
    | t2p'' (VIon (KyX, i), s) =
      let
        val {ctrl, free, bound} = Ion.unmk KyX
        (* FIXME note that we are allowing the same name to occur more than once:
         *       E.g. x//[x,y] o K[x,y]  =>  K[x,x]
         *       Should this be allowed? *)
        val free'      = (List.map (fn y => getOpt (Wiring.app_x s y, y)) free) handle e => raise e
        (* FIXME one name pr binding port should be enough?
         *       I.e. instead of letting s' be an identity, we could choose a
         *       representative name for each binding port and let s' be a
         *       substitution of the names of that binding port to the
         *       representative. *)
        val innernames = Ion.innernames KyX handle e => raise e
      in
        ((VIon (Ion.make {ctrl = ctrl, free= free', bound = bound}, i), false),
         Wiring.id_X innernames) handle e => raise e
      end
    | t2p'' (VPer (pi, i), s) =
      (* Renaming names of a permutation; should perhaps be a Permutation function.*)
      let
        val mathpi  = Permutation.unmk pi (* mathpi : (int * nameset) list *)
        val mathpi' = List.map (fn (i,X) => (i,Wiring.app s X)) mathpi
        val pi'     = Permutation.make mathpi'
      in
        ((VPer (pi', i), false), s)
      end
    | t2p'' (VAbs (X, P, (_, _, i)), s) = 
      let
        val ((P', _), s') = t2p'' (P, s) (* (X) P is an id iff it was before, *)
        val inf'  = innerface P' handle e => raise e
        val outf' = outerface P' handle e => raise e
        val X' = Wiring.app s X                    (* and we only aim to weed out new ids. *)
      in
        ((VAbs (X', P', (inf', Interface.abs X' outf', i)), false), s') handle e => raise e
      end
    | t2p'' (VTen (Gs, (_, _, i)), s) =
      let
        val ress       = Wiring.restrict'' s (* Note: Is it restrict'' I want here? *)
        val outernames = Interface.names o outerface
        val (GsandIdQs', Ss')
          = (ListPair.unzip o List.map (fn G => t2p'' (G, ress (outernames G)))) Gs handle e => raise e
        val (Gs', isNewId)
          = (foldr
               (fn ((G', QId), (Gs', QidAcc)) => (G'::Gs', QId andalso QidAcc))
               ([], true) GsandIdQs') handle e => raise e
      in
        ((VPar (Gs', (Interface.||| (List.map innerface Gs') handle e => raise e,
                      Interface.||| (List.map outerface Gs') handle e => raise e,
                      i)),
          isNewId), Wiring.||| Ss') handle e => raise e
      end
    | t2p'' (VCom (G, H, (_, _, i)), s) = 
      let
        val ((G', GisNewId), s') = t2p'' (G, s) handle e => raise e
        val Ginf'  = innerface G' handle e => raise e
        val Goutf' = outerface G' handle e => raise e
        val ((H', HisNewId), s'') = t2p'' (H, s') handle e => raise e
        val Hinf'  = innerface H' handle e => raise e
        val Houtf' = outerface H' handle e => raise e
      in
        if GisNewId then ((H', HisNewId), s'')
        (* Hmmm - check the next two lines... *)
        else if HisNewId then ((G', GisNewId), s'')
        else ((VCom(G', H', (Hinf', Goutf', i)), false), s'')
      end
    | t2p'' _ = raise NotImplemented
                        "t2p'' for parallel and prime product"



  (* tensor2parallel : bgval -> bgval
   *
   * Substitutes || for ** by removal of y//X's.
   * 
   * TODO: NOT FULLY TESTED.*)
  fun t2p' G =
      let
        val Y = (Interface.names o outerface) G
        val ((G', isId), s) = t2p'' (G, Wiring.id_X Y)
        val innf = innerface G'
        val outf = outerface G'
        fun localize (s, iface) = 
            (* substitution wiring s as a product of local and global wiring, 
             * s.t. it has outerface = iface. *)
          let
            val {width, loc, glob} = Interface.unmk iface
            val ress               = Wiring.restrict_outer s
            val globwir            = ress glob
            val locwirs            = map ress loc

						fun WLS ws =
							let
								fun tensor (w, (vs, Xs, ys, innernames, outernames)) =
								  let
										val X = Wiring.innernames w
										val y = Wiring.outernames w
										val v
                      = VAbs
                          (y,
													 VCom
                             (VTen ([VWir (w, Info.noinfo), 
																		 VPer (Permutation.id_n 1,  Info.noinfo)],
																		(Interface.make
                                         {loc = [NameSet.empty], glob = X},
																		 Interface.make
                                         {loc = [NameSet.empty], glob = y},
																		 Info.noinfo)), 
															VCon (X, Info.noinfo),
															(Interface.make {loc = [X], glob = NameSet.empty},
															 Interface.make {loc = [NameSet.empty], glob = y},
                               Info.noinfo)),
												   (Interface.make {loc = [X], glob = NameSet.empty},
														Interface.make {loc = [y], glob = NameSet.empty},
                            Info.noinfo))
									in
										(v :: vs, X :: Xs, y :: ys,
                     NameSet.union' X innernames,
                     NameSet.union' y outernames)
									end handle e => raise e
								val (vs, Xs, ys, _, _) 
									= foldr tensor ([], [], [], NameSet.empty, NameSet.empty) ws
							in
								VTen (vs, 
											(Interface.make {loc = Xs, glob = NameSet.empty},
											 Interface.make {loc = ys, glob = NameSet.empty}, Info.noinfo))
							end

              val locwir = WLS locwirs
            in
              VPar ([VWir (globwir, Info.noinfo), locwir],
                    (Interface.make {loc  = map Wiring.innernames locwirs,
                                     glob = Wiring.innernames globwir},
                     Interface.make {loc  = map Wiring.outernames locwirs,
                                     glob = Wiring.outernames globwir},
                     Info.noinfo))
            end
      (* Note: Could really also throw out G' if G' isId.*)
      in
        if Wiring.is_id s then G'
        else 
          let val W = localize(s, innf)
          in
            (VCom (G', W, (innerface W, outf, Info.noinfo)))
          end handle e => raise e
      end
	
	val t2p = unmk o t2p'
  

  (* merge2parallelproduct : bgval -> bgval
   * 
   * m2pp - taking (merge * id)(G || ... || H) to (G | ... | H)
   * 
   * TODO: this is a simple heuristic -- Troels is working on something more clever
   *
   * Simply recognize terms on the exact form
   *
   *   (idw || merge_n) o (v_1 || ... || v_n)  and
   *   merge_n o (v_1 || ... || v_n)
   *
   * and rewrite them into
   *
   *   v_1 | ... | v_n
   * 
   * where the v_i must have width 1.
   *)
  fun m2pp (VCom (v1, v2, meta)) =
      let
        val v1' = m2pp v1
        val v2' = m2pp v2
        fun has_width_1 v = 1 = Interface.width (outerface v)
      in
        case (v1', v2') of
          (VPar ([VWir (w, _), VMer (n, _)], _), VPar (vs, meta')) =>
          if Wiring.is_id w andalso length vs = n andalso List.all has_width_1 vs then
            VPri (vs, meta)
          else
            VCom (v1', v2', meta)
        | (VPar ([VMer (n, _)], _), VPar (vs, meta')) =>
          if length vs = n andalso List.all has_width_1 vs then
            VPri (vs, meta)
          else
            VCom (v1', v2', meta)
        | (VMer (n, _), VPar (vs, meta')) =>
          if length vs = n andalso List.all has_width_1 vs then
            VPri (vs, meta)
          else
            VCom (v1', v2', meta)
        | _ => VCom (v1', v2', meta)
      end
    | m2pp (VTen (vs, meta)) = VTen (map m2pp vs, meta)
    | m2pp (VPar (vs, meta)) = VPar (map m2pp vs, meta)
    | m2pp (VPri (vs, meta)) = VPri (map m2pp vs, meta)
    | m2pp (VAbs (X, v, meta)) = VAbs (X, m2pp v, meta)
    | m2pp v = v
   
  val _ = Flags.makeBoolFlag
            {name = "/kernel/ast/bgval/pp-simplify",
             default = false,
             short = "", long = "--pp-simplified",
             arg = "",
             desc = "Simplify BgVal terms before prettyprinting."}
   
  val _ = Flags.makeBoolFlag
            {name = "/kernel/ast/bgval/pp-tensor2parallel",
             default = false,
             short = "", long = "--pp-t2p",
             arg = "",
             desc = "Substitute || for * by removal of y//X's before prettyprinting."}
   
  val _ = Flags.makeBoolFlag
            {name = "/kernel/ast/bgval/pp-merge2prime",
             default = false,
             short = "", long = "--pp-m2p",
             arg = "",
             desc = "Substitute | for || by removal of merges before prettyprinting."}

  fun pp' BgTerm_pp withIface (indent:int) pps v =
    let
      val sppflag = Flags.getBoolFlag "/kernel/ast/bgval/pp-simplify"
      val t2pflag = Flags.getBoolFlag "/kernel/ast/bgval/pp-tensor2parallel"
      val m2pflag = Flags.getBoolFlag "/kernel/ast/bgval/pp-merge2prime"

      val simplify = if sppflag then simplify else (fn x => x) handle e => raise e
      val t2p      = if t2pflag then t2p'     else (fn x => x) handle e => raise e
      val m2pp     = if m2pflag then m2pp     else (fn x => x) handle e => raise e

      val v = (m2pp o simplify o t2p o simplify) v
      val (t, iface, oface) = unmk v handle e => raise e
      (* try to print the interfaces using the names given in the input *)
      val () = (Name.pp_unchanged (NameSet.union' (Interface.names iface)
                                                  (Interface.names oface)))
               handle Name.PPUnchangedNameClash _ => ()
    in
      if withIface then PrettyPrint.begin_block pps PrettyPrint.CONSISTENT 0 else ();
      BgTerm_pp indent pps t;
      if withIface then
        (  PrettyPrint.add_break pps (1, 0);
           PrettyPrint.add_string pps ": ";
           PrettyPrint.begin_block pps PrettyPrint.CONSISTENT 0;
           Interface.pp indent pps iface;
           PrettyPrint.add_break pps (1, 0);
           PrettyPrint.add_string pps "-> ";
           Interface.pp indent pps oface;
           PrettyPrint.end_block pps;
           PrettyPrint.end_block pps
          )
      else ()
    end

(* OLD pp' - now refactored into ppWithIface.
  fun pp' BgTerm_pp indent pps v =
      let
	val t2pflag = Flags.getBoolFlag "/kernel/ast/bgval/tensor2parallel"
	val bgval2bgterm = if t2pflag then t2p else unmk
        val (t, iface, oface) = bgval2bgterm v handle e => raise e
        (* try to print the interfaces using the names given in the input *)
        val ()
          = (Name.pp_unchanged (Interface.names iface) (Interface.names oface))
            handle Name.PPUnchangedNameClash _ =>
              (Name.pp_unchanged NameSet.empty NameSet.empty handle e => raise e)
      in
        BgTerm_pp indent pps t handle e => raise e
      end handle e => raise e
*)
  val ppWithIface = pp' BgTerm.pp true

  val oldpp = pp' BgTerm.oldpp false
  
  val pp = pp' BgTerm.pp false

  fun pp_unchanged indent pps v =
    (Name.pp_unchanged NameSet.empty;
     BgTerm.pp indent pps (#1 (unmk v)))


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

  val toString_unchanged
    = PrettyPrint.pp_to_string
        (Flags.getIntFlag "/misc/linewidth") 
        (pp_unchanged (Flags.getIntFlag "/misc/indent"))

  val size = BgTerm.size o #1 o unmk

  exception DuplicateNames of info * name list list * string
  fun explain_DuplicateNames (DuplicateNames (i, nss, errtxt)) =
      Exp (LVL_USER, Info.origin i, pp_nothing,
           map (fn ns => Exp (LVL_USER, Origin.unknown_origin,
                              mk_list_pp "{" "}" "," Name.pp ns, []))
               nss)
      :: [Exp (LVL_LOW, file_origin, mk_string_pp errtxt, [])]
    | explain_DuplicateNames _ = raise Match
  val _ = add_explainer
            (mk_explainer "duplicate names" explain_DuplicateNames)

  exception NameMissing of bgval * string
  fun explain_NameMissing (NameMissing (v, errtxt)) =
      [Exp (LVL_USER, Info.origin (info v), pack_pp_with_data pp v, []),
       Exp (LVL_LOW, file_origin, mk_string_pp errtxt, [])]
    | explain_NameMissing _ = raise Match
  val _ = add_explainer
            (mk_explainer "bgval abstraction failed because a \
                          \name is missing in the outer face"
                          explain_NameMissing)

  exception NotPrime of bgval * string
  fun explain_NotPrime (NotPrime (v, errtxt)) =
      [Exp (LVL_USER, Info.origin (info v), pack_pp_with_data pp v, []),
       Exp (LVL_LOW, file_origin, mk_string_pp errtxt, [])]
    | explain_NotPrime _ = raise Match
  val _ = add_explainer
            (mk_explainer "bgval to be abstracted must be prime"
                          explain_NotPrime)

  exception NotComposable of bgval * bgval * string
  fun explain_NotComposable (NotComposable (v1, v2, errtxt)) =
      [Exp (LVL_USER, Info.origin (info v1), pack_pp_with_data ppWithIface v1, []),
       Exp (LVL_USER, Info.origin (info v2), pack_pp_with_data ppWithIface v2, []),
       Exp (LVL_LOW, file_origin, mk_string_pp errtxt, [])]
    | explain_NotComposable _ = raise Match
  val _ = add_explainer
            (mk_explainer "bgvals are not composable"
                          explain_NotComposable)

  exception NotTensorable of bgval list * string
  fun explain_NotTensorable (NotTensorable (vs, errtxt)) =
      map (fn v => Exp (LVL_USER, Info.origin (info v),
                        pack_pp_with_data pp v, [])) vs
      @ [Exp (LVL_LOW, file_origin, mk_string_pp errtxt, [])]
    | explain_NotTensorable _ = raise Match
  val _ = add_explainer
            (mk_explainer "bgvals are not tensorable, due \
                          \to inner or outer name clash"
                          explain_NotTensorable)

  fun Mer i n   = VMer (n, i)
  fun Con i X   = VCon (X, i)
  fun Wir i w   = VWir (w, i)
  fun Ion i KyX =
      let
	val y = Ion.outernames KyX
	    handle DuplicatesRemoved =>
		   raise DuplicateNames 
			   (i, [#free (Ion.unmk KyX)],
			    "Outer ion names must be distinct")
	val Xs = Ion.innernames KyX 
	    handle DuplicatesRemoved =>
		   raise DuplicateNames 
			   (i, (map NameSet.list o #bound o Ion.unmk) KyX,
			    "Inner ion names must be distinct")
      in
	VIon (KyX, i)
      end
  fun Per i pi  = 
      let
	val I 
	  = foldr 
	      (fn ((_, X), XX) => NameSet.union X XX)
	      NameSet.empty
	      (Permutation.unmk pi)
	      handle 
	      DuplicatesRemoved =>
	      raise 
		DuplicateNames 
		  (i, (map (NameSet.list o #2) o Permutation.unmk) pi,
		   "Permutation local name sets must be disjoint")
      in
	VPer (Permutation.unchanged pi, i)
      end
  fun Abs i (X, v) = 
      let
	val innfglob = Interface.glob (innerface v)
	val {width = outfwidth, loc = outfloc, glob = outfglob}
	    = Interface.unmk (outerface v) 
	val XsubsetofOutfglob 
	  = NameSet.foldUntil
	    (fn x => fn _ => if NameSet.member x outfglob then
			       (false, NONE)
			     else
			       (true, SOME x))
	    NONE
	    X
      in
	if NameSet.isEmpty innfglob andalso outfwidth = 1
	then
	  case XsubsetofOutfglob of
	    NONE =>
	    let
	      val outerface 
		= Interface.make 
		    {loc = [NameSet.union (List.hd outfloc) X],
		     glob = NameSet.difference outfglob X}
	    in
	      VAbs (X, v, (innerface v, outerface, i))
	    end
	  | SOME x => 
	    raise NameMissing
		    (v, "Cannot abstract name `" ^ Name.unmk x
		        ^ "': it is absent from the set of global \
		          \names of the prime outer face. (in Abs)")
	else 
	  raise NotPrime (v, "Cannot abstract a non-prime (in Abs)")
      end
  fun Ten i vs =
      let
	fun checkface face
	  = foldr 
	    (fn (v, X) => 
		foldr (fn (X, Y) => NameSet.union X Y) 
		      X 
		      (Interface.glob (face v)
		       :: (Interface.loc (face v))))
	    NameSet.empty
	    vs
	val _ = checkface innerface
	  handle 
	  DuplicatesRemoved
	  => raise 
	    NotTensorable
	      (vs, "Inner name clash for tensor product in Ten")
	val _ = checkface outerface
	  handle 
	  DuplicatesRemoved
	  => raise 
	    NotTensorable
	      (vs, "Outer name clash for tensor product in Ten")
      val x = Interface.*  infix 6 x
      fun addinterf (v1, (innf, outf)) 
	= (innf x innerface v1, outf x outerface v1)
      val (innf, outf)
	= foldl addinterf (Interface.zero, Interface.zero) vs
	  handle 
	  DuplicatesRemoved
	  => raise 
	    NotTensorable
	      (vs, "Inner or outer name clash for tensor product in Ten")
    in
      VTen (vs, (innf, outf, i))
    end
  fun Com i (v1, v2) 
    = if arecomposable v1 v2 then 
	VCom (v1, v2, (innerface v2, outerface v1, i))
      else
	raise NotComposable 
		(v1, v2, "Interface mismatch for composition in Com")

  datatype bgpat =
	   PVar
	 | PCns
	 | PMer
	 | PCon
	 | PWir
	 | PIon
	 | PPer
	 | PAbs of bgpat
	 | PTen of bgpat list
	 | PTns
	 | PCom of bgpat * bgpat
  datatype bgmatch =
	   MVal of bgval
	 | MMer of int	      
	 | MCon of nameset    
	 | MWir of wiring     
	 | MIon of ion	      
	 | MPer of Immutable permutation
	 | MAbs of nameset * bgmatch
	 | MTen of bgmatch list
	 | MTns of bgval list
	 | MCom of bgmatch * bgmatch

  fun match PVar v = MVal v
    | match PCns (VMer (n, _))         = MMer n
    | match PCns (VCon (X, _))         = MCon X	 
    | match PCns (VWir (w, _))         = MWir w	 
    | match PCns (VIon (KyX, _))       = MIon KyX
    | match PCns (VPer (pi, _))        = MPer pi 
    | match PCns (VAbs (X, v, _))      = MAbs (X, MVal v)
    | match PCns (VTen (vs, _))        = MTns vs
    | match PCns (VCom (v1, v2, _))    = MCom (MVal v1, MVal v2)
    | match PMer (VMer (n, _))         = MMer n
    | match PCon (VCon (X, _))         = MCon X
    | match PWir (VWir (w, _))         = MWir w
    | match PIon (VIon (KyX, _))       = MIon KyX
    | match PPer (VPer (pi, _))        = MPer pi 
    | match (PAbs p) (VAbs (X, v, _))  = MAbs (X, (match p v))
    | match (PTen ps) (v as (VTen (vs, _))) =
      let
	exception ListPairUnequalLengths
	fun ListPairmapEq f ([], []) = []
	  | ListPairmapEq f (x :: xs, y :: ys)
	    = f (x, y) :: ListPairmapEq f (xs, ys)
	  | ListPairmapEq f _ = raise ListPairUnequalLengths
      in
	MTen (ListPairmapEq (fn (p, v) => match p v) (ps, vs))
	handle ListPairUnequalLengths => MVal v
      end
    | match PTns (VTen (vs, _))             = MTns vs
    | match (PCom (p1, p2)) (VCom (v1, v2, _)) 
      = MCom (match p1 v1, match p2 v2)
    | match _ v = MVal v 

  fun match2bgval i (MVal v) = v
    | match2bgval i (MMer n) = Mer i n
    | match2bgval i (MCon X) = Con i X
    | match2bgval i (MWir w) = Wir i w
    | match2bgval i (MIon KyX) = Ion i KyX
    | match2bgval i (MPer pi) = Per i pi
    | match2bgval i (MAbs (X, m)) = Abs i (X, match2bgval i m)
    | match2bgval i (MTen ms) = Ten i (map (match2bgval i) ms)
    | match2bgval i (MTns vs) = Ten i vs
    | match2bgval i (MCom (m1, m2))
      = Com i (match2bgval i m1, match2bgval i m2)

  fun pp_match indent pps =
      let
	val show = PrettyPrint.add_string pps
	fun <<() = PrettyPrint.begin_block 
		     pps PrettyPrint.INCONSISTENT indent
	fun >>() = PrettyPrint.end_block pps
	fun brk() = PrettyPrint.add_break pps (1, 0)
	fun ppv (VMer (n, _)) = show ("Mer " ^ Int.toString n)
	  | ppv (VCon _) = show "Con X"
	  | ppv (VWir _) = show "Wir w"
	  | ppv (VIon _) = show "Ion KyX"
	  | ppv (VPer _) = show "Per pi"
	  | ppv (VAbs _) = show "Abs (X, v)"
	  | ppv (VTen _) = show "Ten vs"
	  | ppv (VPar _) = show "Par vs"
	  | ppv (VPri _) = show "Pri vs"
	  | ppv (VCom _) = show "Com (v1, v2)" 
	fun pp (MVal v) = ppv v
	  | pp (MMer n) = show ("Mer " ^ Int.toString n)
	  | pp (MCon X) = show "MCon X"
	  | pp (MWir w) = show "MWir w"
	  | pp (MIon KyX) = show "MIon KyX"
	  | pp (MPer pi) = show "MPer pi"
	  | pp (MAbs (X, m)) 
	    = (<<();
	       show "MAbs (X, ";
	       pp m;
	       >>();
	       show ")")
	  | pp (MTen ms) 
	    = (<<(); 
	       show "MTen [";
	       foldl (fn (m, notfirst) 
			 => ((if notfirst then 
				(show ",";
				 brk())
			      else
				());
  			     pp m;
			     true))
		     false
		     ms;
		     >>();
		     show "]")
	  | pp (MTns vs)
	    = show "MTen vs"
	  | pp (MCom (m1, m2))
	    = (<<();
	       show "MCom (";
	       pp m1;
	       show ", ";
	       brk();
	       pp m2;
	       >>();
	       show ")")
      in
	pp
      end

  fun LS i w =
      let
        val X = Wiring.innernames w
        val y = Wiring.outernames w
      in
        Abs i (y, (Com i 
                     (Ten i 
                        [Wir i w, 
                         Per i (Permutation.id_n 1)], 
                      Con i X)))
      end

  exception NameClash of info * nameset * nameset * string
  fun explain_NameClash (NameClash (i, ns1, ns2, errtxt)) =
      Exp (LVL_USER, Info.origin i, pp_nothing,
           map (fn ns => Exp (LVL_USER, Origin.unknown_origin,
                              pack_pp_with_data NameSetPP.pp ns, []))
               [ns1, ns2])
      :: [Exp (LVL_LOW, file_origin, mk_string_pp errtxt, [])]
    | explain_NameClash _ = raise Match
  val _ = add_explainer
            (mk_explainer "name clash" explain_NameClash)

  fun WLS i ws =
      let
	fun tensor (w, (vs, Xs, ys, innernames, outernames)) = 
	    let
	      val X = Wiring.innernames w
	      val y = Wiring.outernames w
	      val v = Abs i 
		      (y,
		       (Com i 
			    (Ten i 
				 [Wir i w, 
				  Per i (Permutation.id_n 1)], 
				 Con i X)))
	    in
	      (v :: vs, X :: Xs, y :: ys,
	       NameSet.union X innernames
	       handle 
	       DuplicatesRemoved
	       => raise NameClash 
			  (i, X, innernames, 
			   "inner names in a wide local substitution"),
	       NameSet.union y outernames
	       handle 
	       DuplicatesRemoved
	       => raise NameClash 
			  (i, y, outernames, 
			   "outer names in a wide local substitution"))
	    end
	val (vs, Xs, ys, _, _) 
	  = foldr tensor ([], [], [], NameSet.empty, NameSet.empty) ws
      in
	VTen (vs, 
	      (Interface.make {loc = Xs, glob = NameSet.empty},
	       Interface.make {loc = ys, glob = NameSet.empty}, i))
      end

  exception NotParallelisable of bgval list * string
  fun explain_NotParallelisable (NotParallelisable (vs, errtxt)) =
      map (fn v => Exp (LVL_USER, Info.origin (info v),
                        pack_pp_with_data pp v, [])) vs
      @ [Exp (LVL_LOW, file_origin, mk_string_pp errtxt, [])]
    | explain_NotParallelisable _ = raise Match
  val _ = add_explainer
            (mk_explainer "cannot make the parallel product of bgvals, \
                          \due to outer local or inner name clash"
                          explain_NotParallelisable)

  exception NotPrimeable of bgval list * string
  fun explain_NotPrimeable (NotPrimeable (vs, errtxt)) =
      map (fn v => Exp (LVL_USER, Info.origin (info v),
                        pack_pp_with_data pp v, [])) vs
      @ [Exp (LVL_LOW, file_origin, mk_string_pp errtxt, [])]
    | explain_NotPrimeable _ = raise Match
  val _ = add_explainer
            (mk_explainer "cannot make the prime product of bgvals, \
                          \due to outer local or inner name clash, or \
                          \nonlocal inner faces"
                          explain_NotPrimeable)

  (* locglobnames returns a tuple (Y, Yloc, Yglob), where Y is a set of
   * all local and global names, Yloc is a set of all local
   * names, and Yglob is a set of all global names.
   *) 
  fun locglobnames iface =
      let 
	val {loc, glob, ...} = Interface.unmk iface
	val locs = foldl (fn (X, Xs) => NameSet.union X Xs)
			 NameSet.empty
			 loc
      in
	(NameSet.union locs glob, locs, glob)
      end

  fun addlink clashnames x (usednames, ls, lsinv) =
      let
	val (usednames, xnew)
	  = if NameSet.member x clashnames then
	      let
		val xnew = Name.fresh (SOME x)
	      in
		(NameSet.insert xnew usednames,
		 xnew)
	      end
	    else
	      (usednames, x)
      in
	(usednames, 
	 LinkSet.insert
	   (Link.make {outer = SOME xnew, inner = NameSet.singleton x})
	   ls,
 	 Link.make {outer = SOME x, inner = NameSet.singleton xnew}
	 :: lsinv)
      end
	
  fun Par' i vs =
      let

	(* Ys is a list of pairs of (outer, outer local) name sets. *)
	val Ys = map (locglobnames o outerface) vs
	(* clashnames are names in 2 or more interfaces, allnames
	 * contains the names of all interfaces, alllocnames contains
	 * the local names of all interfaces, allglobnames contains
	 * the global names of all interfaces.
	 *)
	val (clashnames, allnames, alllocnames, allglobnames)
	  = foldl 
              (fn ((Y, Yloc, Yglob), (clns, alns, allocns, alglobns))
                  => (NameSet.union' clns (NameSet.intersect Y alns),
                      NameSet.union' Y alns,
                      NameSet.union Yloc allocns,
                      NameSet.union' Yglob alglobns))
              (NameSet.empty, NameSet.empty, NameSet.empty, NameSet.empty)
              Ys
              handle DuplicatesRemoved
              => raise 
                   NotParallelisable
                     (vs, "while computing parallel product in Par")
        val _ = if not (NameSet.isEmpty
                  (NameSet.intersect alllocnames allglobnames)) then
                    raise 
                      NotParallelisable
                        (vs, "while computing parallel product in Par")
                else ()
      in
        if NameSet.isEmpty clashnames then
	  Ten i vs
	else
	  let
	    (* Given a bgval, used names set, a list of local link sets,
	     * a global link list, and a list of tuples of
	     * bgvals, (w_i, B_i, v_i), where w_i is a renaming, B_i an 
	     * identity permutation and v_i a bgval,
	     * mksubs returns an updated set of used names, an updated
	     * local link set list, an updated global link set, and
	     * conses a tuple onto the list of tuples.  The renamings
	     * rename clashing names apart, while the link set links them
	     * up again.
	     *)
	    fun mksubs (v, (usednames, loclss, globls, wBvs)) =
		let
		  val {loc, glob, ...} = Interface.unmk (outerface v)
		  val (usednames, ls, globls) 
		    = NameSet.fold (addlink clashnames) 
				   (usednames, LinkSet.empty, globls)
				   glob
		  val w = Wir i (Wiring.make ls)
		  val B = Permutation.id loc
		in
		  (usednames, loclss, globls, (w, B, v) :: wBvs)
		end
	    val (_, _, globls, wBvs)
	      = foldr mksubs (allnames, LinkSet.empty, [], []) vs
	    val w_inv = Wir i (Wiring.make' globls)
	    val A = if is_id0' w_inv then
		      Per i (Permutation.** (map #2 wBvs))
		    else 
		      Ten i [w_inv, Per i (Permutation.** (map #2 wBvs))]
	    fun mkwxB (w, B, v) 
	      = if Permutation.is_id B andalso is_id' w then
		  v
		else
		  if is_id0' w then
		    Com i (Per i B, v)
		  else if Permutation.is_id0 B then
		    Com i (w, v)
		  else
		    Com i (Ten i [w, Per i B], v)
	  in
	    Com i (A, Ten i (map mkwxB wBvs))
	  end
      end

  fun Par i vs =
    let
      (* Parallel product is only defined if the inner names are
         disjoint. *)
      val _ = foldl
                  (fn (X, all) => NameSet.union X all)
                  NameSet.empty 
                  (map (Interface.names o innerface) vs)
	        handle DuplicatesRemoved
	        => raise NotParallelisable
                     (vs, "while computing parallel product in Par")
    in
      Par' i vs
    end 

  fun Abs' i (X, v) =
    let
      val Y     = NameSet.difference X (Interface.names (outerface v))
      val width = Interface.width (outerface v)
    in
      case (NameSet.isEmpty Y, width = 0) of
        (true, false)  => Abs i (X, v)
      | (true, true)   => Abs i (X, Ten i [v, Per i (Permutation.id_n 1)])
      | (false, false) => Abs i (X, Ten i [v, Wir i (Wiring.introduce Y)])
      | (false, true)  => Abs i (X, Ten i [v, Wir i (Wiring.introduce Y),
                                           Per i (Permutation.id_n 1)])
    end

  fun Com' i (v1, v2) =
      let
        val i1 = innerface v1
        val i2 = innerface v2
        val o1 = outerface v1
        val o2 = outerface v2
        val i1_loc_ns
          = foldr
              (fn (X, Y) => NameSet.union X Y)
              NameSet.empty (Interface.loc i1)
        val o2_loc_ns
          = foldr
              (fn (X, Y) => NameSet.union X Y)
              NameSet.empty (Interface.loc o2)
        val X
          = NameSet.difference
              (NameSet.difference (Interface.glob o2) (Interface.glob i1))
              i1_loc_ns
        fun disjoint X Y =
          (NameSet.union X Y; true)
          handle NameSet.DuplicatesRemoved => false
        val id_o2_loc = (* if v1 wiring and v2 not, tensor id onto v1 *) 
          if Interface.width i1 = 0 andalso Interface.width o1 = 0 
          andalso Interface.width o2 > 0 then
            [Per i (Permutation.id (Interface.loc o2))]
          else
            []
        val (v1', o1') =
          if NameSet.isEmpty X then
            case id_o2_loc of
              [] => (v1, o1)
            | vs =>
              let val v1' = Ten i (v1 :: vs) handle e => raise e
              in (v1', outerface v1') end
          else
            let
              val v1' =
                if not (disjoint (Interface.names o1) X) then
                  Par' i (v1 :: Wir i (Wiring.id_X X) :: id_o2_loc)
                else
                  Ten i (v1 :: Wir i (Wiring.id_X X) :: id_o2_loc)  handle e => raise e
            in
              (v1', outerface v1')
            end

         val v2' =
           if Interface.width o2 = 1 then
             let
               val Y = NameSet.difference i1_loc_ns o2_loc_ns
             in
               if NameSet.isEmpty Y then
                 v2
               else
                 Abs' i (Y, v2)
                 handle _ =>
                   raise NotComposable 
                     (v1, v2, "Interface mismatch for composition in Com'")
             end
           else
             v2
      in
        if arecomposable v1' v2' then 
          VCom (v1', v2', (i2, o1', i))
        else
          raise NotComposable 
            (v1, v2, "Interface mismatch for composition in Com'")
      end

  fun Pri i vs =
      let
        (* Prime product is only defined when there are no
         * global inner names and the local inner names are
         * disjoint. *)
	      val _ = foldl
                  (fn (inner, allloc) =>
                      if NameSet.isEmpty (Interface.glob inner) then
                        foldl
                          (fn (loc, allloc) => NameSet.union loc allloc)
                          allloc
                          (Interface.loc inner)
                      else
                        raise 
                          NotPrimeable
                            (vs, "while computing prime product in Pri"))
                  NameSet.empty 
                  (map innerface vs)
	          handle DuplicatesRemoved
	                 => raise 
                     NotPrimeable
                       (vs, "while computing prime product in Pri")
	      (* Ys is a list of pairs of (outer, outer local) name sets. *)
	      val Ys = map (locglobnames o outerface) vs
	      (* clashnames are names in 2 or more interfaces, allnames
	       * contains the names of all interfaces, alllocnames contains
	       * the local names of all interfaces, allglobnames contains
	       * the global names of all interfaces.
	       *)
	      val (clashnames, allnames, alllocnames, allglobnames)
	        = foldl 
              (fn ((Y, Yloc, Yglob), (clns, alns, allocns, alglobns))
                  => (NameSet.union' clns (NameSet.intersect Y alns),
                      NameSet.union' Y alns,
                      NameSet.union Yloc allocns,
                      NameSet.union' Yglob alglobns))
              (NameSet.empty, NameSet.empty, NameSet.empty, NameSet.empty)
              Ys
              handle DuplicatesRemoved
                     => raise 
	                     NotPrimeable
                         (vs, "while computing prime product in Pri")
        val _ = if not (NameSet.isEmpty
                          (NameSet.intersect alllocnames allglobnames))
                then
                  raise NotPrimeable
                          (vs, "while computing prime product in Pri")
                else ()
               
	      (* Given a bgval, used names set, a list of local link sets,
	       * a global link list, a list of bgvals, Bv_i,
         * where  Bv_i = v_i
         *    or  Bv_i = B_i o v_i
         *    where  B_i = (w_i * id) o (id_Z * 'X^i_0' * ... * 'X^i_{k_i-1}') 
         *       or  B_i = (w_i * id) o ('X^i_0' * ... * 'X^i_{k_i-1}') 
         *       or  B_i = id_Z * 'X^i_0' * ... * 'X^i_{k_i-1}'
         *       or  B_i = 'X^i_0' * ... * 'X^i_{k_i-1}'
	       * and v_i a bgval and an integer K, mksubs returns an updated
	       * set of used names, an updated local link set list, an
	       * updated global link set, conses a bgval onto the list of
	       * bgvals, and adds the width of v to K.  The renamings rename
	       * clashing names apart, while the link set links them up
	       * again.
	       *)
	      fun mksubs (v, (usednames, loclss, globls, Bvs, K)) =
	        let
	          val {loc, glob, width} = Interface.unmk (outerface v)
	          val (usednames, ls, globls) 
		          = NameSet.fold (addlink clashnames) 
			                       (usednames, LinkSet.empty, globls)
			                       (foldl (fn (X, Y) => NameSet.union X Y) 
				                            glob
				                            loc)
	          val w = Wir i (Wiring.make ls)
	          val idw = Wir i (Wiring.id_X glob)
	          val idwxXs = if is_id0' idw then
			                     Ten i (map (Con i) loc)
			                   else 
			                     Ten i (idw :: map (Con i) loc)
	          val wxidp = Ten i [w, Per i (Permutation.id_n width)]
	        in
            case (is_id' w, List.all (fn X => NameSet.isEmpty X) loc) of
              (false, false) => 
	            (usednames, loclss, globls, Com i (Com i (wxidp, idwxXs), v) :: Bvs, K + width)
            | (true, false)  =>
	            (usednames, loclss, globls, Com i (idwxXs, v) :: Bvs, K + width)
            | (false, true)  => 
	            (usednames, loclss, globls, Com i (wxidp, v) :: Bvs, K + width)
            | (true, true)   => 
	            (usednames, loclss, globls, v :: Bvs, K + width)
	        end
	      val (_, _, globls, Bvs, K)
	        = foldr mksubs (allnames, LinkSet.empty, [], [], 0) vs
	      val w_inv = Wir i (Wiring.make' globls)
	      val A = if is_id0' w_inv then
		              Mer i K
		            else 
		              Ten i [w_inv, Mer i K]
	      val ABvs = if null Bvs then A else Com i (A, Ten i Bvs)
      in
	      if NameSet.isEmpty alllocnames then
	        ABvs
	      else 
	        Abs i (alllocnames, ABvs)
      end

  fun Ion' i KyX =
      let
	      val Xs = Ion.innernames KyX 
	          handle DuplicatesRemoved =>
		        raise DuplicateNames 
			            (i, (map NameSet.list o #bound o Ion.unmk) KyX,
			             "Inner ion names must be distinct")
        val {ctrl, free, bound} = Ion.unmk KyX

        fun find_duplicates (y, (has_duplicates, ys, y's, ls)) =
            (has_duplicates, NameSet.insert y ys, y::y's,
             Link.make {outer = SOME y, inner = NameSet.singleton y} :: ls)
            handle DuplicatesRemoved =>
            let
              val y' = Name.fresh (SOME y)
            in
              (true, ys, y'::y's,
               Link.make {outer = SOME y, inner = NameSet.singleton y'} :: ls)
            end
        val (has_duplicates, ys, y's, ls)
          = foldr find_duplicates (false, NameSet.empty, [], []) free
      in
        if has_duplicates then
          Com' i (Wir i (Wiring.make' ls),
                  VIon (Ion.make {ctrl = ctrl,
                                  free = y's,
                                  bound = bound}, i))
        else
          VIon (KyX, i)
      end

  fun make t2i =
      let
        fun make' (t as (BgTerm.Mer (n, _)))      = VMer (n, (t2i t))
          | make' (t as (BgTerm.Con (X, _)))      = VCon (X, (t2i t))
          | make' (t as (BgTerm.Wir (w, _)))      = VWir (w, (t2i t))
          | make' (t as (BgTerm.Ion (KyX, _)))    = Ion' (t2i t) KyX
          | make' (t as (BgTerm.Hop (t', _)))
            = raise Fail "FIXME BgTerm.Hop not supported in BgVal.make"
          | make' (t as (BgTerm.Per (pi, _)))     = Per (t2i t) pi
          | make' (t as (BgTerm.Abs (X, t', _)))
            = Abs' (t2i t) (X, make' t')
          | make' (t as (BgTerm.Ten (ts, _)))
            = Ten (t2i t) (List.map make' ts)
          | make' (t as (BgTerm.Pri (ts, _)))
            = Pri (t2i t) (List.map make' ts)
          | make' (t as (BgTerm.Par (ts, _)))
            = Par (t2i t) (List.map make' ts)
          | make' (t as (BgTerm.Com (t1, t2, _))) 
            = Com' (t2i t) (make' t1, make' t2)
      in
        make'
      end



  (* Rxxx rules.
   * NB: We don't keep track of used names, but rely on
   *     Name.fresh to provide globally fresh names. *)
  fun rename_internally' alpha (b as VMer _) =
      (* Rmer rule *)
      (b, alpha)
    | rename_internally' alpha (VCon (ns, i)) =
      (* Rcon rule *)
      (Con i (Wiring.app alpha ns), alpha)
    | rename_internally' alpha (VWir (w, i)) =
      (* Rwir rule *)
      let
        val ls = Wiring.unmk w
        (* Rename inner names with alpha and create a
           renaming for the outer name if present.    *)
        fun rename_link l (ls', beta_ls) =
            case Link.unmk l of
              {outer = SOME y, inner = X} =>
                let
                  val X' = Wiring.app alpha X
                  val z  = Name.fresh (SOME y)
                in
                  (LinkSet.insert
                     (Link.make {outer = SOME z, inner = X'}) ls',
                   LinkSet.insert
                     (Link.make {outer = SOME z, inner = NameSet.singleton y})
                     beta_ls)
                end
            | {outer = NONE, inner = X} =>
                (LinkSet.insert
                   (Link.make {outer = NONE, inner = Wiring.app alpha X}) ls',
                 beta_ls)

        val (ls', beta_ls)
            = LinkSet.fold rename_link (LinkSet.empty, LinkSet.empty) ls
      in
        (Wir i (Wiring.make ls'), Wiring.make beta_ls)
      end
    | rename_internally' alpha (VIon (KyX, i)) =
      (* Rion rule *)
      let
        val {ctrl, free = ys, bound = Xs} = Ion.unmk KyX
        val X's = map (Wiring.app alpha) Xs

        fun rename (y, (zs, beta_ls)) =
            let
              val z = Name.fresh (SOME y)
            in
              (z::zs,
               LinkSet.insert
                 (Link.make {outer = SOME z, inner = NameSet.singleton y})
                 beta_ls)
            end

        val (zs, beta_ls)
            = foldr rename ([], LinkSet.empty) ys
      in
        (Ion i (Ion.make {ctrl = ctrl, free = zs, bound = X's}),
         Wiring.make beta_ls)
      end
    | rename_internally' alpha (VPer (p, i)) =
      (* Rper rule *)
      let
        fun rename (j, ns) = (j, Wiring.app alpha ns)
        val p' = Permutation.make (map rename (Permutation.unmk p))
      in
        (Per i p', alpha)
      end
    | rename_internally' alpha (VAbs (ns, v, (_, _, i))) =
      (* Rabs rule *)
      let
        val (v', beta) = rename_internally' alpha v
      in
        (Abs i ((Wiring.app beta ns), v'), beta)
      end
    | rename_internally' alpha (VTen (vs, (_, _, i))) =
      (* Rten rule *)
      let
        fun rename (v, (vs', betas)) =
            let
              val X = Interface.names (innerface v)
              val (v', beta) = rename_internally' (Wiring.restrict alpha X) v
            in
              (v'::vs', beta::betas)
            end
        val (vs', betas) = foldr rename ([], []) vs
      in
        (Ten i vs', Wiring.** betas)
      end
    | rename_internally' alpha (VCom (v1, v2, (_, _, i))) =
      (* Rcom rule *)
      let
        val (v2', beta1) = rename_internally' alpha v2
        val (v1', beta2) = rename_internally' beta1 v1
      in
        (Com i (v1', v2'), beta2)
      end
    | rename_internally' _ _ 
      = raise NotImplemented
              "rename_internally' for parallel and prime product"

  fun rename_internally b =
      let
        val i = info b
        val X  = Interface.names (innerface b)
        val (b'', beta) = rename_internally' (Wiring.id_X X) b
        val beta_inv = Wiring.invert_renaming beta
        val beta_glob_inv
            = Wiring.restrict_outer beta_inv (Interface.glob (outerface b))
        val wls = WLS i (map
                           (Wiring.restrict_outer beta_inv)
                           (Interface.loc (outerface b)))
      in
        Com i (Ten i [Wir i beta_glob_inv, wls], b'')
      end


  structure Constraints = NameBijectionConstraints
  (* fun eq': test two bgvals for equality up to a bijection between the inner
   * names satisfying the constraints given by C.
   * 
   * If the bgvals are equal, a set of constraints on a bijection between the
   * outer names is returned.
   *)
  fun eq' C (VMer (i1, _))  (VMer (i2, _))  =
      if i1 = i2 then
        SOME Constraints.empty
      else
        NONE
    | eq' C (VCon (ns1, _)) (VCon (ns2, _)) = SOME C
    | eq' C (VWir (w1, _))  (VWir (w2, _))  = Wiring.eq' C w1 w2
    | eq' C (VIon (i1, _))  (VIon (i2, _))  = Ion.eq' C i1 i2
    | eq' C (VPer (p1, _))  (VPer (p2, _))  = Permutation.eq' C p1 p2
    | eq' C (VAbs (ns1, b1, _)) (VAbs (ns2, b2, _)) =
      let
        val allns1 = Interface.names (outerface b1)
        val notns1 = NameSet.difference allns1 ns1
        val allns2 = Interface.names (outerface b2)
        val notns2 = NameSet.difference allns2 ns2
        val C''    = Constraints.from_list [(ns1, ns2), (notns1, notns2)]
      in
        case eq' C b1 b2 of
          SOME C' => 
          if NameSet.size ns1 = NameSet.size ns2 then
            Constraints.combine (C', C'')
          else
            NONE
        | NONE => NONE
      end
    | eq' C (VTen (bs1, (_, of1, _))) (VTen (bs2, _)) =
      let
        fun eq'' (b1::bs1) (b2::bs2) C' =
            let
              (* restrict constraints to the current subterms *)
              val c_dom = Interface.names (innerface b1)
              val c_rng = Interface.names (innerface b2)
            in
              case Constraints.restrict (C, (c_dom, c_rng)) of
                SOME C''' =>
                  (case eq' C''' b1 b2 of
                     SOME C'' => eq'' bs1 bs2 (Constraints.plus (C', C''))
                   | NONE     => NONE)
              | NONE => NONE
            end
          | eq'' [] [] C' = SOME C'
          | eq'' _ _ _    = NONE
      in
        eq'' bs1 bs2 Constraints.empty
      end
    | eq' C (VCom (b11, b12, _)) (VCom (b21, b22, _)) =
      (case eq' C b12 b22 of
         NONE    => NONE
       | SOME C' => eq' C' b11 b21)
    | eq' cm _ _ = NONE

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

  exception UnknownControl of bgval
  fun explain_UnknownControl (UnknownControl v) =
      [Exp (LVL_USER, Info.origin (info v), pack_pp_with_data pp v, []),
       Exp (LVL_LOW, file_origin, mk_string_pp "in BgVal.replacectrls", [])]
    | explain_UnknownControl _ = raise Match
  val _ = add_explainer
            (mk_explainer "The ion control name has not been declared"
                          explain_UnknownControl)
  exception WrongArity of bgval
  fun explain_WrongArity (WrongArity v) =
      [Exp (LVL_USER, Info.origin (info v), pack_pp_with_data pp v, []),
       Exp (LVL_LOW, file_origin, mk_string_pp "in BgVal.replacectrls", [])]
    | explain_WrongArity _ = raise Match
  val _ = add_explainer
            (mk_explainer
              "Ion control arity does not match the number \
              \of free names or bound name sets"
                          explain_WrongArity)

  fun replacectrls ctrls =
    let
      fun rplc (v as VIon (ion, i))
        = (VIon (Ion.replacectrl ctrls ion, i)
           handle Ion.WrongArity _ => raise WrongArity v
                | Ion.UnknownControl _ => raise UnknownControl v)
        | rplc (VAbs (X, v, i))   = VAbs (X, rplc v, i) 
        | rplc (VTen (vs, i))     = VTen (map rplc vs, i)
        | rplc (VCom (b1, b2, i)) = VCom (rplc b1, rplc b2, i)
        | rplc v = v
    in
      rplc
    end

  val revision
    = hd (String.tokens (not o Char.isDigit) "$LastChangedRevision$")
end


functor BgVal (structure Info : INFO
	       structure Name : NAME
	       structure NameSet : MONO_SET
               structure NameBijectionConstraints : BIJECTION_CONSTRAINTS
	       structure Link : LINK
	       structure LinkSet : MONO_SET
	       structure Control : CONTROL
	       structure Ion : ION
	       structure Permutation : PERMUTATION
	       structure Wiring : WIRING
	       structure Interface : INTERFACE
	       structure BgTerm : BGTERM
	       structure ErrorHandler : ERRORHANDLER
                 where type ppstream    = PrettyPrint.ppstream
                   and type break_style = PrettyPrint.break_style
                   and type origin      = Origin.origin
	       structure NameSetPP : COLLECTIONPRETTYPRINT
                 where type ppstream    = PrettyPrint.ppstream
	       sharing type Name.name = 
			    NameSet.elt = 
			    Link.name =
			    Ion.name =
			    Wiring.name
	       sharing type NameSet.Set =
			    Name.NameSet.Set =
                            NameBijectionConstraints.set =
			    Link.nameset =
			    Ion.nameset =
			    Permutation.nameset =
			    Wiring.nameset =
			    Interface.nameset =
			    BgTerm.nameset =
                            NameSetPP.collection
               sharing type Control.control =
                            Ion.control
               sharing type Link.link = 
			    LinkSet.elt =
			    Wiring.link
               sharing type LinkSet.Set = Wiring.linkset
	       sharing type Ion.ion = BgTerm.ion
	       sharing type Permutation.permutation =
			    BgTerm.permutation
	       sharing type Permutation.Immutable =
			    BgTerm.Immutable
	       sharing type Wiring.wiring = BgTerm.wiring
	       sharing type Interface.interface =
			    Permutation.interface
               sharing type NameBijectionConstraints.constraints =
                            Ion.nameconstraints =
                            Permutation.nameconstraints =
                            Wiring.nameconstraints
               sharing type Info.info =
                            BgTerm.info
			    ) :> BGVAL 
  where type nameset = NameSet.Set
    and type name = Name.name
    and type wiring = Wiring.wiring 
    and type 'kind permutation = 'kind Permutation.permutation
    and type Immutable = Permutation.Immutable
    and type ion = Ion.ion
    and type interface = Interface.interface 
    and type bgterm = BgTerm.bgterm
    and type info = Info.info =
struct
  structure BgVal = BgVal'(structure Info = Info
			   structure Name = Name
			   structure NameSet = NameSet
                           structure NameBijectionConstraints = NameBijectionConstraints
			   structure Link = Link
			   structure LinkSet = LinkSet
                           structure Control = Control
			   structure Ion = Ion
			   structure Permutation = Permutation
			   structure Wiring = Wiring
			   structure Interface = Interface
			   structure BgTerm = BgTerm
			   structure ErrorHandler = ErrorHandler
			   structure NameSetPP = NameSetPP)
  open BgVal
end
