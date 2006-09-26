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
 * @version $Revision: 1.20 $
 *)
functor BgVal (structure Name : NAME
	       structure NameSet : MONO_SET
	       structure Link : LINK
	       structure LinkSet : MONO_SET
	       structure Ion : ION
	       structure Permutation : PERMUTATION
	       structure Wiring : WIRING
	       structure Interface : INTERFACE
	       structure BgTerm : BGTERM
	       structure PrettyPrint : PRETTYPRINT
	       type info
	       val noinfo : info
	       sharing type Name.name = 
			    NameSet.elt = 
			    Link.name =
			    Ion.name
	       sharing type NameSet.Set =
			    Link.nameset =
			    Ion.nameset =
			    Permutation.nameset =
			    Wiring.nameset =
			    Interface.nameset =
			    BgTerm.nameset
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
	       sharing type PrettyPrint.ppstream =
			    BgTerm.ppstream) :> BGVAL 
  where type nameset = NameSet.Set
    and type wiring = Wiring.wiring 
    and type 'kind permutation = 'kind Permutation.permutation
    and type Immutable = Permutation.Immutable
    and type ion = Ion.ion
    and type interface = Interface.interface 
    and type bgterm = BgTerm.bgterm
    and type info = info
    and type ppstream = PrettyPrint.ppstream = 
struct
  type info = info
  type bgterminfo = BgTerm.info
  type name = Ion.name
  type nameset = NameSet.Set
  type bgterm = BgTerm.bgterm
  type interface = Interface.interface
  type Immutable = Permutation.Immutable
  type 'kind permutation = 'kind Permutation.permutation
  type ion = Ion.ion
  type wiring = Wiring.wiring

  type ppstream = PrettyPrint.ppstream

  val noinfo = noinfo

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

  exception DuplicateNames of info * name list list * string

  exception NameMissing of string * bgval * string

  exception NotPrime of string * bgval * string

  exception NotComposable of string * bgval * bgval * string

  exception NotTensorable of string * bgval list * string

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
	    handle NameSet.DuplicatesRemoved _ =>
		   raise DuplicateNames 
			   (i, [#free (Ion.unmk KyX)],
			    "Outer ion names must be distinct")
	val Xs = Ion.innernames KyX 
	    handle NameSet.DuplicatesRemoved _ =>
		   raise DuplicateNames 
			   (i, 
			    (map NameSet.list o #bound o Ion.unmk) KyX,
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
	      NameSet.DuplicatesRemoved _ =>
	      raise 
		DuplicateNames 
		  (i, 
		   (map (NameSet.list o #2) o Permutation.unmk) pi,
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
		    ("bgval.sml", v, 
		     "Cannot abstract name `" ^ Name.unmk x
		     ^ "': it is absent from the set of global \
		       \names of the prime outer face. (in Abs)")
	else 
	  raise NotPrime ("bgval.sml", v, 
			  "Cannot abstract a non-prime (in Abs)")
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
	  (DuplicatesRemoved _)
	  => raise 
	    NotTensorable
	      ("bgval.sml", vs,
	       "Inner name clash for tensor product in Ten")
	val _ = checkface outerface
	  handle 
	  (DuplicatesRemoved _)
	  => raise 
	    NotTensorable
	      ("bgval.sml", vs,
	       "Outer name clash for tensor product in Ten")
      val x = Interface.*  infix 6 x
      fun addinterf (v1, (innf, outf)) 
	= (innf x innerface v1, outf x outerface v1)
      val (innf, outf)
	= foldl addinterf (Interface.zero, Interface.zero) vs
	  handle 
	  (DuplicatesRemoved (X, xs))
	  => raise 
	    NotTensorable
	      ("bgval.sml", vs,
	       "Inner or outer name clash for tensor product in Ten")
    in
      VTen (vs, (innf, outf, i))
    end
  fun Com i (v1, v2) 
    = if arecomposable v1 v2 then 
	VCom (v1, v2, (innerface v2, outerface v1, i))
      else
	raise NotComposable 
		("bgval.sml", v1, v2, 
		 "Interface mismatch for composition in Com")

  exception MalformedBgTerm of info * string
			  
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
  fun info (VMer (_, i))            = i
    | info (VCon (_, i))            = i
    | info (VWir (_, i))            = i
    | info (VIon (_, i))            = i
    | info (VPer (_, i))            = i
    | info (VAbs (_, _, (_, _, i))) = i 
    | info (VTen (_, (_, _, i)))    = i
    | info (VCom (_, _, (_, _, i))) = i

exception NotImplemented of string * bgval * string

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
    = raise NotImplemented ("bgval.sml", v, 
			    "is_concretion_of for composition")

fun is_id (VMer (1, _))    = true
  | is_id (VMer _)         = false
  | is_id (VCon (X, _))    = NameSet.isEmpty X
  | is_id (VWir (w, _))    = Wiring.is_id w
  | is_id (VIon _)         = false
  | is_id (VPer (pi, _))   = Permutation.is_id pi
  | is_id (VAbs (X, v, _)) = is_concretion_of X v
  | is_id (VTen (vs, _))   = List.all is_id vs
  | is_id (v as (VCom _))  = raise NotImplemented
				 ("bgval.sml", v,
				  "is_id for composition")

fun is_id0 (VMer (1, _))    = false
  | is_id0 (VMer _)         = false
  | is_id0 (VCon (X, _))    = false
  | is_id0 (VWir (w, _))    = Wiring.is_id0 w
  | is_id0 (VIon _)         = false
  | is_id0 (VPer (pi, _))   = Permutation.is_id0 pi
  | is_id0 (VAbs (X, v, _)) = false
  | is_id0 (VTen (vs, _))   = List.all is_id0 vs
  | is_id0 (v as (VCom _))  = raise NotImplemented
				 ("bgval.sml", v,
				  "is_id for composition")

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
	       DuplicatesRemoved (_, _)
	       => raise NameClash 
			  (i, X, innernames, 
			   "inner names in a wide local substitution"),
	       NameSet.union y outernames
	       handle 
	       DuplicatesRemoved (_, _)
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

  exception NotParallelisable of string * bgval list * string

  exception NotPrimeable of string * bgval list * string

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
  fun fresh basename = fresh' 0 (Name.unmk basename ^ "_")
  end

  fun addlink clashnames x (usednames, ls, lsinv) =
      let
	val (usednames, xnew)
	  = if NameSet.member x clashnames then
	      let
		val xnew = fresh x usednames
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
	        handle DuplicatesRemoved (X, xs) 
	        => raise 
                     NotParallelisable
                       ("bgval.sml", vs,
                        "while computing parallel product in Par")

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
              handle DuplicatesRemoved (X, xs) 
              => raise 
                   NotParallelisable
                     ("bgval.sml", vs,
                      "while computing parallel product in Par")
        val _ = if not (NameSet.isEmpty
                  (NameSet.intersect alllocnames allglobnames)) then
                    raise 
                      NotParallelisable
                        ("bgval.sml", vs,
                         "while computing parallel product in Par")
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
                            ("bgval.sml", vs,
                             "while computing parallel product in Par"))
                  NameSet.empty 
                  (map innerface vs)
	        handle DuplicatesRemoved (X, xs) 
	        => raise 
                     NotPrimeable
                       ("bgval.sml", vs,
                        "while computing parallel product in Par")
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
              handle DuplicatesRemoved (X, xs) 
              => raise 
	           NotPrimeable
                     ("bgval.sml", vs,
                      "while computing prime product in Pri")
        val _ = if not (NameSet.isEmpty
                          (NameSet.intersect alllocnames allglobnames))
                then
                    raise NotPrimeable
                            ("bgval.sml", vs,
                             "while computing prime product in Pri")
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
  fun unmk v2i =
      let
	fun unmk' (v as (VMer (n, i)))
	    = (BgTerm.Mer (n, (v2i v)), Interface.m n, Interface.one)
	  | unmk' (v as (VCon (X, i)))
	    = (BgTerm.Con (X, (v2i v)), 
	       Interface.make {loc = [X], 
			       glob = NameSet.empty},
	       Interface.make {loc = [NameSet.empty], glob = X})
	  | unmk' (v as (VWir (w, i)))
	    = (BgTerm.Wir (w, (v2i v)),
	       Interface.X (Wiring.innernames w),
	       Interface.X (Wiring.outernames w))
	  | unmk' (v as (VIon (KyX, i)))
	    = (BgTerm.Ion (KyX, (v2i v)),
	       Interface.make {loc = [Ion.innernames KyX],
			       glob = NameSet.empty},
	       Interface.make {loc = [], glob = Ion.outernames KyX})
	  | unmk' (v as (VPer (pi, i)))
	    = (BgTerm.Per (pi, (v2i v)), 
	       Permutation.innerface pi,
	       Permutation.outerface pi)
	  | unmk' (v as (VAbs (X, v', (innf, outf, i)))) 
	    = (BgTerm.Abs (X, #1 (unmk' v'), (v2i v)), innf, outf)
	  | unmk' (v as (VTen (vs, (innf, outf, i)))) 
	    = (BgTerm.Ten (List.map (#1 o unmk') vs, (v2i v)), innf, outf)
	  | unmk' (v as (VCom (v1, v2, (innf, outf, i)))) 
	    = (BgTerm.Com (#1 (unmk' v1), #1 (unmk' v2), (v2i v)),
	       innf,
	       outf)
      in
	unmk'
      end

  fun pp indent pps
    = BgTerm.pp indent pps o #1 o unmk (fn _ => BgTerm.noinfo)

end
