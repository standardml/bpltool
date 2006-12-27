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
	       structure Link : LINK
	       structure LinkSet : MONO_SET
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
			    Link.nameset =
			    Ion.nameset =
			    Permutation.nameset =
			    Wiring.nameset =
			    Interface.nameset =
			    BgTerm.nameset =
                            NameSetPP.collection
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

  fun info (VMer (_, i))            = i
    | info (VCon (_, i))            = i
    | info (VWir (_, i))            = i
    | info (VIon (_, i))            = i
    | info (VPer (_, i))            = i
    | info (VAbs (_, _, (_, _, i))) = i 
    | info (VTen (_, (_, _, i)))    = i
    | info (VCom (_, _, (_, _, i))) = i

  val unmk =
      let
	fun unmk' (v as (VMer (n, i)))
	    = (BgTerm.Mer (n, (info v)), Interface.m n, Interface.one)
	  | unmk' (v as (VCon (X, i)))
	    = (BgTerm.Con (X, (info v)), 
	       Interface.make {loc = [X], 
			       glob = NameSet.empty},
	       Interface.make {loc = [NameSet.empty], glob = X})
	  | unmk' (v as (VWir (w, i)))
	    = (BgTerm.Wir (w, (info v)),
	       Interface.X (Wiring.innernames w),
	       Interface.X (Wiring.outernames w))
	  | unmk' (v as (VIon (KyX, i)))
	    = (BgTerm.Ion (KyX, (info v)),
	       Interface.make {loc = [Ion.innernames KyX],
			       glob = NameSet.empty},
	       Interface.make {loc = [NameSet.empty], glob = Ion.outernames KyX})
	  | unmk' (v as (VPer (pi, i)))
	    = (BgTerm.Per (pi, (info v)), 
	       Permutation.innerface pi,
	       Permutation.outerface pi)
	  | unmk' (v as (VAbs (X, v', (innf, outf, i)))) 
	    = (BgTerm.Abs (X, #1 (unmk' v'), (info v)), innf, outf)
	  | unmk' (v as (VTen (vs, (innf, outf, i)))) 
	    = (BgTerm.Ten (List.map (#1 o unmk') vs, (info v)), innf, outf)
	  | unmk' (v as (VCom (v1, v2, (innf, outf, i)))) 
	    = (BgTerm.Com (#1 (unmk' v1), #1 (unmk' v2), (info v)),
	       innf,
	       outf)
      in
	unmk'
      end

  fun pp indent pps
    = BgTerm.pp indent pps o #1 o unmk

  fun ppWithIface (indent:int) pps v =
    let
      val (t, iface, oface) = unmk v
    in
      PrettyPrint.begin_block pps PrettyPrint.CONSISTENT indent;
    	BgTerm.pp indent pps t;
    	PrettyPrint.add_break pps (1, 0);
    	PrettyPrint.add_string pps ": ";
    	Interface.pp indent pps iface;
    	PrettyPrint.add_break pps (1, 0);
    	PrettyPrint.add_string pps "-> ";
    	Interface.pp indent pps oface;
    	PrettyPrint.end_block pps
    end

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

  fun innerface (VMer (n, i))   = Interface.m n
    | innerface (VCon (X, i)) 
      = Interface.make {loc = [X], glob = NameSet.empty}
    | innerface (VWir (w, i))   = Interface.X (Wiring.innernames w)
    | innerface (VIon (KyX, i)) 
      = Interface.make {loc = [Ion.innernames KyX], glob = NameSet.empty}
    | innerface (VPer (pi, _))  = Permutation.innerface pi
    | innerface (VAbs (_, _, (innf, _, _))) = innf
    | innerface (VTen (_, (innf, _, _)))    = innf
    | innerface (VCom (_, _, (innf, _, _))) = innf

  fun outerface (VMer (_, i))   = Interface.one
    | outerface (VCon (X, i)) 
      = Interface.make {loc = [NameSet.empty], glob = X}
    | outerface (VWir (w, i))   = Interface.X (Wiring.outernames w)
    | outerface (VIon (KyX, i)) =
      let
	val {free, ...} = Ion.unmk KyX
	val y = NameSet.fromList free
      in
	Interface.make {loc = [NameSet.empty], glob = y}
      end
    | outerface (VPer (pi, _))  = Permutation.outerface pi
    | outerface (VAbs (_, _, (_, outf, _))) = outf
    | outerface (VTen (_, (_, outf, _)))    = outf
    | outerface (VCom (_, _, (_, outf, _))) = outf

  fun arecomposable v1 v2 = Interface.eq (innerface v1, outerface v2)

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

exception NotImplemented of bgval * string
fun explain_NotImplemented (NotImplemented (v, errtxt)) =
    [Exp (LVL_USER, Info.origin (info v), pack_pp_with_data pp v, []),
     Exp (LVL_LOW, file_origin, mk_string_pp errtxt, [])]
  | explain_NotImplemented _ = raise Match
val _ = add_explainer
          (mk_explainer "feature not implemented"
                        explain_NotImplemented)

(* Determine whether a (X)v is the identity. *)
fun is_concretion_of X (VMer (1, _))     = NameSet.isEmpty X
  | is_concretion_of X (VMer _)          = false
  | is_concretion_of X (VCon (Y, _))     = NameSet.eq X Y
  | is_concretion_of X (VWir _)          = false
  | is_concretion_of X (VIon _)          = false
  | is_concretion_of X (VPer (pi, _))    = NameSet.isEmpty X andalso
					   Permutation.is_id pi
  | is_concretion_of X (VAbs (Y, v, _))  
    = is_concretion_of (NameSet.union X Y) v
  | is_concretion_of X (VTen ([v], _))   = is_concretion_of X v
  | is_concretion_of _ (VTen _)          = false
  | is_concretion_of X (v as (VCom _))          
    = raise NotImplemented (v, "is_concretion_of for composition")

fun is_id (VMer (1, _))    = true
  | is_id (VMer _)         = false
  | is_id (VCon (X, _))    = NameSet.isEmpty X
  | is_id (VWir (w, _))    = Wiring.is_id w
  | is_id (VIon _)         = false
  | is_id (VPer (pi, _))   = Permutation.is_id pi
  | is_id (VAbs (X, v, _)) = is_concretion_of X v
  | is_id (VTen (vs, _))   = List.all is_id vs
  | is_id (v as (VCom _))  = raise NotImplemented
				 (v, "is_id for composition")

fun is_id' v = is_id v handle NotImplemented _ => false

fun is_id0 (VMer (1, _))    = false
  | is_id0 (VMer _)         = false
  | is_id0 (VCon (X, _))    = false
  | is_id0 (VWir (w, _))    = Wiring.is_id0 w
  | is_id0 (VIon _)         = false
  | is_id0 (VPer (pi, _))   = Permutation.is_id0 pi
  | is_id0 (VAbs (X, v, _)) = false
  | is_id0 (VTen (vs, _))   = List.all is_id0 vs
  | is_id0 (v as (VCom _))  = raise NotImplemented
				 (v, "is_id0 for composition")

fun is_id0' v = is_id0 v handle NotImplemented _ => false

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
	
  fun Par i vs =
      let
        (* Parallel product is only defined if the inner names are
           disjoint. *)
	val _ = foldl
                  (fn (X, all) => NameSet.union X all)
                  NameSet.empty 
                  (map (Interface.names o innerface) vs)
	        handle DuplicatesRemoved
	        => raise 
                     NotParallelisable
                       (vs, "while computing parallel product in Par")

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
	    val A = if is_id0 w_inv then
		      Per i (Permutation.** (map #2 wBvs))
		    else 
		      Ten i [w_inv, Per i (Permutation.** (map #2 wBvs))]
	    fun mkwxB (w, B, v) 
	      = if Permutation.is_id B andalso is_id w then
		  v
		else
		  if is_id0 w then
		    Com i (Per i B, v)
		  else if Permutation.is_id0 B then
		    Com i (w, v)
		  else
		    Com i (Ten i [w, Per i B], v)
	  in
	    Com i (A, Ten i (map mkwxB wBvs))
	  end
      end

  fun Pri i vs =
      let
        (* Prime product is only defined when there are no
           global inner names and the local inner names are
           disjoint. *)
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
                            (vs, "while computing parallel product in Par"))
                  NameSet.empty 
                  (map innerface vs)
	        handle DuplicatesRemoved
	        => raise 
                     NotPrimeable
                       (vs, "while computing parallel product in Par")
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
	 * a global link list, a list of tuples of
	 * bgvals, (B_i, v_i), 
	 * B_i = (w_i * id)(id_Z * 'X^i_0' * ... * 'X^i_{k_i-1}') 
	 * and v_i a bgval, and an integer K,
	 * mksubs returns an updated set of used names, an updated
	 * local link set list, an updated global link set,
	 * conses a tuple onto the list of tuples, and adds the width
	 * of v to K.  The renamings
	 * rename clashing names apart, while the link set links them
	 * up again.
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
	      val idwxXs = if is_id0 idw then
			     Ten i (map (Con i) loc)
			   else 
			     Ten i (idw :: map (Con i) loc)
	      val wxidp = Ten i [w, Per i (Permutation.id_n width)]
	      val B = if is_id wxidp then
			idwxXs
		      else
			Com i (wxidp, idwxXs)
	    in
	      (usednames, loclss, globls, Com i (B, v) :: Bvs, K + width)
	    end
	val (_, _, globls, Bvs, K)
	  = foldr mksubs (allnames, LinkSet.empty, [], [], 0) vs
	val w_inv = Wir i (Wiring.make' globls)
	val A = if is_id0 w_inv then
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


  fun make t2i =
      let
	fun make' (t as (BgTerm.Mer (n, _)))      = VMer (n, (t2i t))
	  | make' (t as (BgTerm.Con (X, _)))      = VCon (X, (t2i t))
	  | make' (t as (BgTerm.Wir (w, _)))      = VWir (w, (t2i t))
	  | make' (t as (BgTerm.Ion (KyX, _)))    = Ion (t2i t) KyX
	  | make' (t as (BgTerm.Per (pi, _)))     = Per (t2i t) pi
	  | make' (t as (BgTerm.Abs (X, t', _)))
	    = Abs (t2i t) (X, make' t')
	  | make' (t as (BgTerm.Ten (ts, _)))
            = Ten (t2i t) (List.map make' ts)
	  | make' (t as (BgTerm.Pri (ts, _)))
            = Pri (t2i t) (List.map make' ts)
	  | make' (t as (BgTerm.Par (ts, _)))
            = Par (t2i t) (List.map make' ts)
	  | make' (t as (BgTerm.Com (t1, t2, _))) 
	    = Com (t2i t) (make' t1, make' t2)
      in
	make'
      end


  fun simplify (VAbs (X, v, ioi as (_, _, i))) = 
    (* ({})P -> P
     * (X)(X')P -> (X u X')P
     * (X)"X" -> id_(X)
     *)
    if NameSet.isEmpty X then
      simplify v
    else
	  	(case simplify v of
	  	   VAbs (X', v', ioi') => VAbs (NameSet.union X X', v', ioi)
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
	            | v => (w', i', v :: vs)
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
	  | simplify (VCom (v1, v2, ioi as (_, _, i))) =
	  (* "X"(X)P -> P
	   * w1 w2 -> [[w1 w2]]
	   * pi1 pi2 -> [[pi1 pi2]]
	   * id v -> v
	   * v id -> v
	   * (v1 * v2)(v1' * v2') -> v1 v1' * v2 v2', if vi : Ii ->, vi' : -> Ii
		 * (alpha * id_1) K_y(X) -> K_{alpha(y)}(X)
		 * (alpha * id_1) (beta * K_y(X)) -> alpha' beta * K_{alpha(y)}(X)
		 * A o (B o C) -> (A o B) o C    (for nicer prettyprinting)
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
		          fun distrib first (vs1, _, is1, os1) (vs2, _, is2, os2) [] []
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
		            | distrib _ (vs1, m1, is1, os1) (vs2, m2, is2, os2)
		                        (v1 :: vs1') (v2 :: vs2')
		            = let
		                val iface1' = innerface v1
		                val oface2' = outerface v2
		                val m1' = Interface.width (iface1')
		                val m2' = Interface.width (oface2')
		              in
		                if m1 + m1' < m2 then
		                  distrib false 
		                    (v1 :: vs1, m1 + m1', iface1' :: is1, outerface v1 :: os1)
		                    (vs2, m2, is2, os2) vs1' (v2 :: vs2')
		                else if m1 + m1' = m2 then
		                  add (v1 :: vs1, iface1' :: is1, outerface v1 :: os1)
		                      (vs2, is2, os2) 
		                      (distrib false ([], 0, [], []) ([], 0, [], []) vs1' (v2 :: vs2'))
		                else if m2 + m2' < m1 then
		                  distrib false 
		                    (vs1, m1, is1, os1)
		                    (v2 :: vs2, m2 + m2', innerface v2 :: is2, oface2' :: os2)
		                    (v1 :: vs1') vs2'
		                else if m2 + m2' = m1 then
		                  add (vs1, is1, os1)
		                      (v2 :: vs2, innerface v2 :: is2, oface2' :: os2)
		                      (distrib false ([], 0, [], []) ([], 0, [], []) (v1 :: vs1') vs2')
		                else if m2 + m2' = m1 + m1' then
		                  add (v1 :: vs1, iface1' :: is1, outerface v1 :: os1)
		                      (v2 :: vs2, innerface v2 :: is2, oface2' :: os2)
		                      (distrib false ([], 0, [], []) ([], 0, [], []) vs1' vs2')
		                else
		                  distrib false
		                    (v1 :: vs1, m1 + m1', iface1' :: is1, outerface v1 :: os1)
		                    (v2 :: vs2, m2 + m2', innerface v2 :: is2, oface2' :: os2)
		                    vs1' vs2'
	                end
		            | distrib _ (vs1, _, is1, os1) (vs2, _, is2, os2) [] vs2'
		            = let
		                val iface2' = mkinterface innerface vs2'    
		                val oface2' = mkinterface outerface vs2'
		              in
	                  add (vs1, is1, os1)
	                      (rev vs2' @ vs2, iface2' :: is2, oface2' :: os2)
	                      []
	                end
		            | distrib _ (vs1, _, is1, os1) (vs2, _, is2, os2) vs1' []
		            = let
		                val iface1' = mkinterface innerface vs1'
		                val oface1' = mkinterface outerface vs1'
		              in
	                  add (rev vs1' @ vs1, iface1' :: is1, oface1' :: os1)
	                      (vs2, is2, os2)
	                      []
	                end
	            val vs = distrib true ([], 0, [], []) ([], 0, [], []) vs1 vs2
		        in
		          simplify (* Does this loop??? *)
		            (VTen (vs, (mkinterface innerface vs,
		                        mkinterface outerface vs,
	                          i)))
		        end
	        else
	          VCom (v1', v2', ioi)
	      | (VTen ([VWir (w, _), v], _), VIon (KyX, _))
	      => if is_id v andalso Wiring.is_renaming w then
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
	      | (v1', VCom (v2a, v2b, _)) => VCom (VCom (v1', v2a, ioi), v2b, ioi)
	      | (v1', v2') => VCom (v1', v2', ioi)
	    end                 
    | simplify v = v

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
end


functor BgVal (structure Info : INFO
	       structure Name : NAME
	       structure NameSet : MONO_SET
	       structure Link : LINK
	       structure LinkSet : MONO_SET
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
			    Link.nameset =
			    Ion.nameset =
			    Permutation.nameset =
			    Wiring.nameset =
			    Interface.nameset =
			    BgTerm.nameset =
                            NameSetPP.collection
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
			   structure Link = Link
			   structure LinkSet = LinkSet
			   structure Ion = Ion
			   structure Permutation = Permutation
			   structure Wiring = Wiring
			   structure Interface = Interface
			   structure BgTerm = BgTerm
			   structure ErrorHandler = ErrorHandler
			   structure NameSetPP = NameSetPP)
  open BgVal
end
