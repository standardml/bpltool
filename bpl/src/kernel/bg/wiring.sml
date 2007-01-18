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

(** Abstract data type for modelling wirings.
 * @version $LastChangedRevision$
 *)
functor Wiring'(structure Link : LINK
		structure LinkSet : MONO_SET
		structure Name : NAME
		structure NameSet : MONO_SET
		structure NameMap : MONO_FINMAP
		structure IntSet : MONO_SET where type elt = int
		structure ErrorHandler : ERRORHANDLER
                  where type ppstream    = PrettyPrint.ppstream
                    and type break_style = PrettyPrint.break_style
                    and type origin      = Origin.origin
		structure NameSetPP : COLLECTIONPRETTYPRINT
                  where type ppstream    = PrettyPrint.ppstream
		sharing type Link.link = LinkSet.elt
		sharing type Name.name = Link.name = NameSet.elt = NameMap.dom
		sharing type NameSet.Set =
                             Link.nameset =
		             NameSetPP.collection) : WIRING 
                where type link       = Link.link
		  and type linkset    = LinkSet.Set 
                  and type name       = Name.name
		  and type nameset    = NameSet.Set
                  and type 'a namemap = 'a NameMap.map =
struct
  type link = Link.link
  type linkset = LinkSet.Set
  type nameset = NameSet.Set
  type 'a namemap = 'a NameMap.map
  type name = NameSet.elt
  open Debug
  open ErrorHandler

  val file_origin = Origin.mk_file_origin
                      "$BPL/src/kernel/bg/wiring.sml"
                      Origin.NOPOS
  fun mk_explainer errtitle (explainer : exn -> explanation list) e =
      Exp (LVL_USER, Origin.unknown_origin, mk_string_pp errtitle,
           explainer e)

  (* A nameedge is either an outer name or an internal edge. *)
  datatype nameedge = Name of name | Closure of int
  (* Internal link representation, identifying internal bigraph edges
   * by integers.
   *)
  type link' = {outer : nameedge, inner : nameset}

  (* Compare operator on nameedges. *)
  fun nameedgecompare (Name n1)    (Name n2)    = Name.compare (n1, n2)
    | nameedgecompare (Name _)     (Closure _)  = LESS
    | nameedgecompare (Closure _)  (Name _)     = GREATER
    | nameedgecompare (Closure i1) (Closure i2) = Int.compare (i1, i2)

  (* Less-than operator on link's. *)
  structure Link'Order =
  struct
  type T = link'
  fun lt ({outer = ne1, inner = ns1} : T) ({outer = ne2, inner = ns2} : T) 
      = (case nameedgecompare ne1 ne2 of
           LESS    => true
         | GREATER => false
         | EQUAL   =>
             (case NameSet.compare (ns1, ns2) of
                LESS => true
              | _    => false))
  end

  structure Link'Set = OrderSet (Link'Order)
  type link'set = Link'Set.Set

  fun nameedgehash (Name x) = Name.hash x
    | nameedgehash (Closure i) = Word.fromInt i

  exception NOT_FOUND
  structure NameHashMap 
    = HashTableFn (type hash_key = name
                   val hashVal = Name.hash
		   val sameKey = Name.==);
  fun createNameHashMap size = NameHashMap.mkTable (size, NOT_FOUND)
  fun createNameHashMap' () = createNameHashMap 37

  structure NameEdgeMap
    = HashTableFn (type hash_key = nameedge
                   val hashVal = nameedgehash
		   val sameKey = op =);
  fun createNameEdgeMap size = NameEdgeMap.mkTable (size, NOT_FOUND)
  fun createNameEdgeMap' ()  = createNameEdgeMap 37

  (* The wiring representation used by this Wiring module is a double
   * representation, allowing faster composition.
   *)
  type wiring = link'set * nameedge NameHashMap.hash_table

  fun eq (ls1, _) (ls2, _) = Link'Set.eq ls1 ls2

  (* Convert an inverted map, mapping nameedges to name sets,
   * into a link'set. 
   *)
  fun invmap2link'set invmap =
      NameEdgeMap.foldi
      (fn (ne, X, ls) => Link'Set.insert {outer = ne, inner = X} ls)
      Link'Set.empty invmap 

  (* Convert a map, mapping names to nameedges, into a link'set. *)
  fun map2link'set amap =
      let
	val invmap = createNameEdgeMap'()
      in
	NameHashMap.appi
	    (fn (x,ne) => 
		case NameEdgeMap.find invmap ne of
		    SOME xs
		    => NameEdgeMap.insert invmap (ne, NameSet.insert x xs)
		  | NONE 
		    => NameEdgeMap.insert invmap (ne, NameSet.singleton x))
            amap ;
	invmap2link'set invmap
      end
  (* Invert a map mapping nameedges to name sets into one
   * mapping individual names to nameedges.
   *)
  fun invert mapsize invmap =
      let
	val newmap = createNameHashMap mapsize
	fun entername ne x _ = NameHashMap.insert newmap (x, ne)
      in
	NameEdgeMap.appi 
	    (fn (ne, xs) => NameSet.fold (entername ne) () xs)
            invmap ;
	newmap
      end

  fun make' ls =
      let
	(* First we create an inverted map, mapping nameedges to sets
	 * of inner names.
	 *)
	val invmap 
	  = createNameEdgeMap' ()
	(* addlink adds one link to the inverted map, numbering the
	 * internal edges (i.e., closed links) sequentially using i.
	 *)
	fun addlink (l, (i, insize)) = 
	    case Link.unmk l of
	      {outer = SOME outername, inner} =>
		(case NameEdgeMap.find invmap 
				       (Name outername) of
		  SOME innernameset 
		  => (NameEdgeMap.insert invmap
					 (Name outername,
					  NameSet.union inner innernameset);
		      (i, insize + NameSet.size inner))
		| NONE
		  => (NameEdgeMap.insert invmap
					 (Name outername,
					  inner);
		      (i, insize + NameSet.size inner)))
	    | {outer = NONE, inner} 
	      => (NameEdgeMap.insert invmap (Closure (i + 1), inner);
		  (i + 1, insize + NameSet.size inner))
	(* We add all the links to the inverted map. *)
	val (_, innernamesize) = foldl addlink (0, 0) ls;
      in
	(* And then compute a link'set from it, as well as a
	 * non-inverted map.
	 *)
	(invmap2link'set invmap, invert innernamesize invmap)
      end

  fun make ls = make' (LinkSet.list ls)

  (*FIXME not necessarily the most efficient way to do it... *)
  fun make_ren nm =
      make' (NameMap.Fold
               (fn ((x, y), ls) =>
                   (Link.make {outer = SOME y, inner = NameSet.singleton x})
                   :: ls)
               [] nm)
	      
  fun unmk (link'set, _) = 
      let
	fun insertlink {outer = Name y, inner}
	    = LinkSet.insert (Link.make {outer = SOME y, inner = inner})
	  | insertlink {outer = Closure _, inner}
	    = LinkSet.insert (Link.make {outer = NONE, inner = inner})
      in
	Link'Set.fold insertlink LinkSet.empty link'set
      end

  fun innernames (ls, _)
    = Link'Set.fold 
	(fn {inner, ...} => NameSet.union inner)
	NameSet.empty
	ls
  fun outernames (ls, _)
    = Link'Set.fold
	(fn {outer = Name y, ...} => NameSet.insert y
	  | _ => fn Y => Y) 
	NameSet.empty
	ls

  fun introductions (ls, _)
    = Link'Set.fold
        (fn {outer = Name y, inner}
          => if NameSet.isEmpty inner then 
               NameSet.insert y
             else
               (fn Y => Y)
          | _ => (fn Y => Y))
        NameSet.empty
        ls

  exception InternalError 
	    of string * nameset NameEdgeMap.hash_table * nameedge * string

  exception CannotExtend of wiring * wiring * nameedge

  fun is_id (ls, ht) = 
      let
	fun is_not_id_link {outer = Name y, inner} _ =
	    if NameSet.size inner = 1 
	       andalso NameSet.member y inner
	    then
	      (false, true)
	    else
	      (true, false)
	  | is_not_id_link {outer = Closure _, inner} _ =
	    if NameSet.isEmpty inner then
	      (false, true)
	    else
	      (true, false)
      in
	Link'Set.foldUntil is_not_id_link true ls
      end

  fun is_id0 (ls, ht) =
      let
	fun is_not_id0_link {outer = Closure _, inner} _ =
	    if NameSet.isEmpty inner then
	      (false, true)
	    else
	      (true, false)
	  | is_not_id0_link _ _ = (true, false)
      in
	Link'Set.foldUntil is_not_id0_link true ls
      end

  fun pp indent pps (w as (ls, _)) =
      let
	open PrettyPrint
	val show = add_string pps
	fun << () = begin_block pps INCONSISTENT indent
	fun >> () = end_block pps
	fun brk () = add_break pps (1, 0)
	fun ppname {outer, inner} notfirst =
	    (if notfirst then (show ","; brk()) else ();
	     case outer of
	       Name y => Name.pp indent pps y
	     | _ => ();
	     true)
	fun ppwire {outer, inner} notfirst =
	    (if notfirst then (brk(); show "* ") else ();
	     case outer of
	       Name y => Name.pp indent pps y
	     | _ => ();
	     show "/";
	     if NameSet.size inner = 1 then
	       NameSet.apply (Name.pp indent pps) inner
	     else
	       NameSetPP.pp indent pps inner;
	     true)
      in
	case Link'Set.size ls of
	  0 => show "idw_0"
	| 1 => (Link'Set.fold ppwire false ls; ())
	| _ => if is_id w then
		 (show "idw_";
		  <<(); show "{";
		  Link'Set.fold ppname false ls; 
		  show "}"; >>())
	       else      
		 (<<();
		  show "("; Link'Set.fold ppwire false ls; show ")";
		  >>())
      end

  fun toString w
    = PrettyPrint.pp_to_string
        (Flags.getIntFlag "/misc/linewidth") 
        (pp (Flags.getIntFlag "/misc/indent")) w

  (* Algorithm for composing wirings:
   * 1 For each link V |-> y in w1, add y |-> {} to w_inv.
   * 2 For each link X |-> u in w2,
   *   2a If u = SOME Name v, then consider e = w1(v):
   *      2a.I   If e = SOME Name y, then add X to w_inv(y)
   *      2a.II  If e = SOME Closure i, then add X to w_inv(i)
   *      2a.III If e = NONE, then add a new closure i |-> X to w_inv
   *   2b If u = SOME Closure i, then add a new closure i' |-> X to
   *      w_inv.
   * 3 Use the inverted map to construct w = w1 o w2.
   *)
  fun compose ((l1s, ht1) : wiring, (l2s, ht2)) =
      let
	(* w_inv maps w outer names and closure edges to sets of w2
	 * inner names. 
	 *)
	val w_inv 
	  = createNameEdgeMap (2 * Link'Set.size l1s)
	fun mapinsert {outer, inner} (i, innsz) =
	    ((NameEdgeMap.insert w_inv (outer, NameSet.empty);
	     case outer of Closure i' => Int.max (i, i')
			 | _ => i), innsz + NameSet.size inner)
	(* Step 1 *)
	val (i_max, innsz) = Link'Set.fold mapinsert (0, 0) l1s

	fun addlink {outer = (Name v), inner = X} (i, innsz) =
	    (* Step 2a *)
	    ((case NameHashMap.find ht1 v of
		SOME ne
		=> ((case NameEdgeMap.find w_inv ne of
		       SOME X' (* Step 2a.I+II *)
		       => (NameEdgeMap.insert w_inv 
					      (ne, NameSet.union X X'))
		     | NONE =>
		       raise InternalError ("wiring.sml", w_inv, ne,
					    "addlink")); 
	            i)
	      | NONE => (* Step 2a.III *)
		(NameEdgeMap.insert w_inv (Closure (i + 1), X);
		 i + 1)), innsz + NameSet.size X)
	  | addlink {outer = (Closure i'), inner = X} (i, innsz) =
	    (* Step 2b *)
	    ((NameEdgeMap.insert w_inv (Closure (i + 1), X);
	      i + 1), innsz + NameSet.size X)
	(* Step 2 *)
	val (i_max, innsz) = Link'Set.fold addlink (i_max, innsz) l2s
      in
	(* Step 3 *)
	(invmap2link'set w_inv, invert innsz w_inv)
      end

  local
  fun parprod (l1s, ht1) (l2s, ht2) =
      (* Careful, now.  We must renumber the closure numbers of
       * (ls2, ht2) so they don't merge with those of (ls1, ht1).
       *)
      let
	val ht = createNameHashMap (Link'Set.size l1s + Link'Set.size l2s)
	val i_max = ref ~1
	fun insertlinkinht offset (innername, nameedge as (Name n)) =
	    NameHashMap.insert ht (innername, nameedge)
	  | insertlinkinht offset (innername, Closure i) =
	    ((if !i_max < i then i_max := i else ());
	     NameHashMap.insert ht (innername, Closure (offset + i)))
	val _ = NameHashMap.appi (insertlinkinht 0) ht1
	val i2_offset = !i_max + 1
	val _ = NameHashMap.appi (insertlinkinht i2_offset) ht2;
      in
	(i2_offset, ht)
      end
  in
  fun x (w1 as (l1s, ht1), w2 as (l2s, ht2)) =
      let
	val (i2_offset, ht) = parprod w1 w2
	fun insertlinkinls (link' as {outer = Name n, inner}) ls =
	    Link'Set.insert link' ls
	  | insertlinkinls {outer = Closure i, inner} ls =
	    Link'Set.insert {outer = Closure (i2_offset + i), 
			     inner = inner}
			    ls
	val ls = Link'Set.fold insertlinkinls l1s l2s
      in
	(ls, ht)
      end

  fun || (w1, w2) =
      let
	val (i2_offset, ht) = parprod w1 w2
      in
	(map2link'set ht, ht)
      end
  end

  val id_0 : wiring = (Link'Set.empty, createNameHashMap 1)

  fun ||| [] = id_0
    | ||| [w] = w
    | ||| (w :: ws) = || (w, ||| ws)

  fun plus (w1 as (l1s, ht1)) (w2 as (l2s, ht2)) =
      (* Careful, now.  We must renumber the closure numbers of
       * (ls2, ht2) so they don't merge with those of (ls1, ht1)
       * unless they are supposed to do so.
       *)
    let
			val ht = NameHashMap.copy ht2
			val imax (* Maximum closure index of w2 *)
			  = Link'Set.fold
			      (fn {outer = Closure i, inner} =>
			         (fn imax => if i > imax then i else imax)
			        | _ => (fn imax => imax)) ~1 l2s
			
			(* Insert link x |-> y1 into ht, return true if merge occurred. *)        
			fun insertnamelink y1 x merged =
			  case NameHashMap.find ht2 x of
			    SOME (Name y2) =>
		 	      if Name.== (y1, y2) then
		 	        true
		 	      else
		 	        raise CannotExtend (w1, w2, Name y2)
		 	  | SOME (Closure i) => raise CannotExtend (w1, w2, Closure i)
		 	  | NONE => (NameHashMap.insert ht (x, Name y1); merged)

      (* Merge links  inner |-> y  into lacc, removing it from ls.
       * Return new lacc and new ls.
       *)
      fun mergelinks (l as {outer = Name y, inner}) 
                     (lacc as {outer = Name yacc, inner = inneracc}, ls)
        = if Name.== (y, yacc) then
            ({outer = Name yacc, inner = NameSet.union' inner inneracc},
             Link'Set.remove l ls)
          else
            (lacc, ls)
        | mergelinks _ laccls = laccls
		
		  (* Merge the points of every internal edge with an index in is
		   * into an accumulated set, and remove them from ls.  Return
		   * the new accumulated set and new ls.
		   *)
		  fun mergeedges is i1 (l as {outer = Closure i2, inner})
		                       (inneracc, ls)
		    = if IntSet.member i2 is then
		        (NameSet.union' inner inneracc, Link'Set.remove l ls)
		      else
		        (inneracc, ls)
		    | mergeedges is i (l as {outer = Name _, inner = _})
		                      inneraccls
		    = inneraccls
		  
		  (* Insert link l into ls, returning an updated version of
		   * imax and ls.
		   *)  
			fun insertlinks (l as {outer = Name y1, inner}) (imax, ls)
			  = if NameSet.fold (insertnamelink y1) false inner then
			      let
			        val (l, ls) = Link'Set.fold mergelinks (l, ls) ls
			      in
			        (imax, Link'Set.insert l ls)
			      end
			    else
			      (imax, ls)
			  | insertlinks {outer = Closure i, inner} (imax, ls)
			  = let
			      fun addedgeof x (I, imin)
			        = case NameHashMap.find ht2 x of
			            SOME (Closure i')
			             => (IntSet.insert' i' I,
			                 if i' < imin then i' else imin)
			          | NONE => (I, imin)
			          | SOME (Name y2) =>
			              raise CannotExtend (w1, w2, Name y2)
			      val (is, imin)
			        = NameSet.fold addedgeof (IntSet.empty, imax + 1) inner
			    in
			      (if imin < imax then imax else imin,
			       if IntSet.isEmpty is then
			         Link'Set.insert {outer = Closure imin, inner = inner} ls
			       else
			         let
			           val (inner, ls)
			             = Link'Set.fold (mergeedges is imin) (inner, ls) ls
			         in
			           NameSet.apply (fn x => NameHashMap.insert ht (x, Closure imin)) inner;
			           Link'Set.insert {outer = Closure imin, inner = inner} ls
			         end)
			    end
			val (_, ls) = Link'Set.fold insertlinks (imax, l2s) l1s
	  in
	    (ls, ht)
	  end
          

  fun ++ [] = id_0
    | ++ [w] = w
    | ++ (w :: ws) = plus w (++ ws)

  exception NotInDomain of wiring * name * string
  fun explain_NotInDomain (NotInDomain (w, x, errtxt)) =
      [Exp (LVL_USER, Origin.unknown_origin, pack_pp_with_data pp w, []),
       Exp (LVL_USER, Origin.unknown_origin, pack_pp_with_data Name.pp x, []),
       Exp (LVL_LOW, file_origin, mk_string_pp errtxt, [])]
    | explain_NotInDomain _ = raise Match
  val _ = add_explainer
            (mk_explainer "the name is not in the domain of the wiring"
                          explain_NotInDomain)
  exception NotInCodomain of wiring * nameset * string
  fun explain_NotInCodomain (NotInCodomain (w, X, errtxt)) =
      [Exp (LVL_USER, Origin.unknown_origin, pack_pp_with_data pp w, []),
       Exp (LVL_USER, Origin.unknown_origin, pack_pp_with_data NameSetPP.pp X, []),
       Exp (LVL_LOW, file_origin, mk_string_pp errtxt, [])]
    | explain_NotInCodomain _ = raise Match
  val _ = add_explainer
            (mk_explainer "the names are not in the codomain of the wiring"
                          explain_NotInCodomain)

  fun in_domain x (_, ht)
    = case NameHashMap.find ht x of
        SOME _ => true
      | NONE => false

  fun app_x (w as (_, ht)) x =
    case NameHashMap.find ht x of
      SOME (Name n) => SOME n
    | NONE => raise NotInDomain (w, x, "in app_x")
    | _ => NONE

  fun app w X =
      NameSet.fold ((fn (SOME n) => (fn Y => NameSet.insert n Y) 
		     | NONE => fn Y => Y)
		    o app_x w)
		   NameSet.empty
		   X
      handle NotInDomain (w, x, _) => raise NotInDomain (w, x, "in app")

  fun app_inverse (w as (ls, _)) Y =
      let
        val (wY', Y')
          = Link'Set.fold
              (fn {outer = Name y, inner} =>
                  (fn (wY', Y') =>
                      if NameSet.member y Y then
                        (NameSet.union wY' inner, NameSet.insert' y Y')
                      else
                        (wY', Y'))
                | _ => fn X => X)
              (NameSet.empty, NameSet.empty) ls
      in
        if NameSet.eq Y Y' then
          wY'
        else
          raise NotInCodomain (w, NameSet.difference Y Y', "in app_inverse")
      end

  exception NotARenaming of wiring * string
  fun explain_NotARenaming (NotARenaming (w, errtxt)) =
      [Exp (LVL_USER, Origin.unknown_origin, pack_pp_with_data pp w, []),
       Exp (LVL_LOW, file_origin, mk_string_pp errtxt, [])]
    | explain_NotARenaming _ = raise Match
  val _ = add_explainer
            (mk_explainer "the wiring was expected to be a renaming"
                          explain_NotARenaming)

  fun unmk_ren (w as (link'set, _)) = 
      let
	fun insertlink {outer = Name y, inner} nm =
	      (case NameSet.list inner of
                 [x] => NameMap.add (x, y, nm)
               | _   => raise NotARenaming (w, "in unmk_ren(1)"))
	  | insertlink {outer = Closure _, ...} _ =
	      raise NotARenaming (w, "in unmk_ren(2)")
      in
	Link'Set.fold insertlink NameMap.empty link'set
      end

  fun app_renaming_x w x =
      case app_x w x of
        SOME y => y
      | NONE   => raise NotARenaming (w, "in app_renaming_x")

  fun app_renaming_inverse_x w y =
      (*FIXME innefficient...*)
      let
        val X = app_inverse w (NameSet.singleton y)
      in
        if NameSet.size X = 1 then
          hd (NameSet.list X)
        else
          raise NotARenaming (w, "in app_renaming_inverse_x")
      end
      handle NotInCodomain (w, Y, _)
             => raise NotInCodomain (w, Y, "in app_renaming_inverse_x")

  (* Algorithm for restricting a wiring:
   * Construct an inverse hash table (mapping nameedges to
   * name sets) by looking up what each inner name maps to.
   * Then create the restricted wiring from this inverse
   * hash table.
   *)
  fun restrict (ls, ht) X =
    let
			val ht_inv = createNameEdgeMap (2 * Link'Set.size ls)

			fun addlink x =
	    (case NameHashMap.find ht x of
	       SOME ne
	       => (case NameEdgeMap.find ht_inv ne of
		     			SOME X'
		     			=> NameEdgeMap.insert ht_inv 
					   			(ne, NameSet.insert x X')
		   			| NONE => 
		     			NameEdgeMap.insert ht_inv
								(ne,NameSet.singleton x))
	     | NONE => ())
      (* Add all the links mapping from X. *)
			val i_max = NameSet.apply addlink X
    in
			(invmap2link'set ht_inv, invert (NameSet.size X) ht_inv)
    end

  fun split (ls, ht) X =
    let
      val ht0 = createNameHashMap (2 * Link'Set.size ls)
      val ht1 = createNameHashMap (2 * Link'Set.size ls)
      val ht0_inv = createNameEdgeMap (2 * Link'Set.size ls)
      val ht1_inv = createNameEdgeMap (2 * Link'Set.size ls)
			fun addto htx htx_inv (pair as (x, ne)) =
			  (NameHashMap.insert htx pair;
			  case NameEdgeMap.find htx_inv ne of
			    SOME X'
			    => NameEdgeMap.insert htx_inv
			         (ne, NameSet.insert x X')
			  | NONE =>
			       NameEdgeMap.insert htx_inv
			         (ne, NameSet.singleton x))
      fun splitter (pair as (x, _))
        = if NameSet.member x X then
            addto ht1 ht1_inv pair
          else
            addto ht0 ht0_inv pair
    in
      NameHashMap.appi splitter ht;
      {inDom    = (invmap2link'set ht1_inv, ht1),
       notInDom = (invmap2link'set ht0_inv, ht0)}
    end

  fun restrict_outer (ls, ht) Y =
      let
	val (new_ls, new_ht_size)
            = Link'Set.fold (fn (l as {outer = Name n, inner}) =>
                               (fn (new_ls, new_ht_size) =>
                                  if NameSet.member n Y then
                                    (Link'Set.insert l new_ls,
                                     new_ht_size + NameSet.size inner)
                                  else
                                    (new_ls, new_ht_size))
                              | _ => fn new => new)
                            (Link'Set.empty, 0) ls
        val new_ht = createNameHashMap new_ht_size
        fun add_nameedge {outer, inner}
            = NameSet.apply
                (fn n => NameHashMap.insert new_ht (n, outer))
                inner
      in
        (Link'Set.apply add_nameedge new_ls;
         (new_ls, new_ht))
      end

  fun split_outer (ls, ht) Y =
      let
				val (ls_inCod, ht_inCod_size, ls_notInCod, ht_notInCod_size)
            = Link'Set.fold
                (fn l as {outer = Name n, inner} =>
                    (fn (ls_inCod, ht_inCod_size,
                         ls_notInCod, ht_notInCod_size) =>
	                      if NameSet.member n Y then
	                        (Link'Set.insert l ls_inCod,
	                         ht_inCod_size + NameSet.size inner,
	                         ls_notInCod,
	                         ht_notInCod_size)
	                      else
	                        (ls_inCod,
	                         ht_inCod_size,
	                         Link'Set.insert l ls_notInCod,
	                         ht_notInCod_size + NameSet.size inner))
	                | l as {outer, inner} => (* internal edges go into notInCod *)
	                  fn (ls_inCod, ht_inCod_size,
	                      ls_notInCod, ht_notInCod_size) =>
                        (ls_inCod,
                         ht_inCod_size,
                         Link'Set.insert l ls_notInCod,
                         ht_notInCod_size + NameSet.size inner))
	                (Link'Set.empty, 0, Link'Set.empty, 0) ls
        val ht_inCod = createNameHashMap ht_inCod_size
        val ht_notInCod = createNameHashMap ht_notInCod_size
        fun add_nameedge ht {outer, inner}
            = NameSet.apply
                (fn n => NameHashMap.insert ht (n, outer))
                inner
      in
        Link'Set.apply (add_nameedge ht_inCod) ls_inCod;
        Link'Set.apply (add_nameedge ht_notInCod) ls_notInCod;
        {inCod = (ls_inCod, ht_inCod),
         notInCod = (ls_notInCod, ht_notInCod)}
      end

  fun addto ht names y
    = NameSet.apply (fn x => NameHashMap.insert ht (x, y)) names

  fun splitopen (w as (ls, ht)) =
    let
      val ht_open = createNameHashMap' ()
      val ht_opened = createNameHashMap' ()
      fun addlink (l as {outer, inner})
                  (ls_open, ls_opened, newnames) =
        case outer of
          Name y => (addto ht_open inner outer;
                     (Link'Set.insert l ls_open,
                      ls_opened,
                      newnames))
        | Closure i =>
            let
              val y = Name.fresh NONE
              val newl = {outer = Name y, inner = inner}
            in
              addto ht_opened inner (Name y);
              (ls_open,
               Link'Set.insert newl ls_opened,
               NameSet.insert y newnames)
            end
      val (ls_open, ls_opened, newnames)
        = Link'Set.fold
            addlink (Link'Set.empty, Link'Set.empty,
            	       NameSet.empty) ls
    in
      {opened    = (ls_opened, ht_opened),
       rest      = (ls_open, ht_open),
       newnames  = newnames}
    end

  fun openup (w as (ls, ht)) =
    let
      val new_ht = NameHashMap.copy ht
      fun addlink (l as {outer, inner})
                  (new_ls, newnames) =
        case outer of
          Name x => (new_ls, newnames)
        | Closure i =>
            let
              val y = Name.fresh NONE
              val newl = {outer = Name y, inner = inner}
              val new_ls = Link'Set.remove l new_ls 
            in
              addto new_ht inner (Name y);
              (Link'Set.insert newl new_ls,
              NameSet.insert y newnames)
            end
      val (new_ls, newnames)
        = Link'Set.fold
            addlink (Link'Set.empty, NameSet.empty) ls
    in
      {opened = (new_ls, new_ht),
       newnames = newnames}
    end

  fun closelinks Y (ls, ht) =
    let
      val new_ht = NameHashMap.copy ht
      fun count {outer = Closure i, inner} imax
        = if i > imax then i else imax
        | count _ imax = imax
      val imax = Link'Set.fold count 0 ls
      fun closelink (l as {outer = Name y, inner}) (new_ls, i)
        = if NameSet.member y Y then
            (addto new_ht inner (Closure (i + 1));
             (Link'Set.insert {outer = Closure (i + 1), inner = inner}
                              new_ls,
              i + 1))
          else
            (Link'Set.insert l new_ls, i)
        | closelink l (new_ls, i)
        = (Link'Set.insert l new_ls, i)
      val (new_ls, _) = Link'Set.fold closelink (Link'Set.empty, imax) ls
    in
      (new_ls, new_ht)
    end

  fun invert_renaming (w as (ls, ht)) =
      let
        val ht' = createNameHashMap (NameHashMap.numItems ht)

        fun invert_link' {outer = Name n, inner = ns} =
            let
              val n' = case NameSet.list ns of
                         [n] => n
                       | []  => raise NotARenaming (w,
                                "wiring contains a link with no inner names")
                       | _   => raise NotARenaming (w,
                                "wiring contains a link with multiple inner names")
              val ne = Name n'
            in
	      (NameHashMap.insert ht' (n, ne);
               {outer = ne, inner = NameSet.singleton n})
            end
          | invert_link' {outer = Closure _, ...} =
            raise NotARenaming (w, "wiring contains a closed link")
      in
        (Link'Set.map invert_link' ls, ht')
      end
      

  (* ROOM FOR EFFICIENCY IMPROVEMENT: Instead of testing whether all
   * elements of Y map to themself (is_id_y), remove y's from Y in the
   * first step (testlink), and then finally check whether Y is empty.
   * This requires NameSet.remove y Y to signal whether y actually
   * was present in Y before removing.
   *)
  fun is_id_x_sigma Y (ls, ht) =
    let
      fun testlink {outer = Name y, inner} _ =
          if NameSet.member y Y then
            let val OK = NameSet.size inner = 1
            in (not OK, OK) end
          else
            (false, true)
        | testlink {outer = Closure _, ...} _ = (true, false)
      fun is_id_y y _ =
        case NameHashMap.find ht y of
          SOME (Name y')
           => let val OK = y' = y
              in (not OK, OK) end
        | _ => (true, false)
    in
      Link'Set.foldUntil testlink true ls
      andalso
      NameSet.foldUntil is_id_y true Y
    end            

  fun is_renaming (ls, ht) =
    let
      fun isSingleton X
        = NameSet.foldUntil (fn _ => fn i => (i >= 1, i + 1)) 0 X
          = 1
      fun testlink {outer = Name y, inner} _
        = let val OK = isSingleton inner
          in (not OK, OK) end
        | testlink {outer = Closure _, inner} _ = (true, false)
    in
      Link'Set.foldUntil testlink true ls
    end

  fun id_X X =
      let
	val ht = createNameHashMap' ()
	fun addlink x ls =
	    (NameHashMap.insert ht (x, Name x);
	     Link'Set.insert
	       {outer = Name x, inner = NameSet.singleton x}
	       ls)
      in
        (NameSet.fold addlink Link'Set.empty X, ht)
      end

  fun introduce X =
      let
	val ht = createNameHashMap 1
	fun addlink x ls =
	    Link'Set.insert {outer = Name x, inner = NameSet.empty} ls
      in
	(NameSet.fold addlink Link'Set.empty X, ht)
      end

  fun close X =
      let
	val ht = createNameHashMap (2 * NameSet.size X)
	fun addlink x (ls, i) =
	    (NameHashMap.insert ht (x, Closure i);
	     (Link'Set.insert
	       {outer = Closure i, inner = NameSet.singleton x}
	       ls,
	       i + 1))
      in
	(#1 (NameSet.fold addlink (Link'Set.empty, 0) X), ht)
      end
  
  val op * = x

  fun op + (w1, w2) = plus w1 w2

  val ** = foldr x id_0

  val (op o) = compose
end

functor Wiring (structure Link : LINK
		structure LinkSet : MONO_SET
		structure Name : NAME
		structure NameSet : MONO_SET
		structure NameMap : MONO_FINMAP
		structure IntSet : MONO_SET where type elt = int
		structure ErrorHandler : ERRORHANDLER
                  where type ppstream    = PrettyPrint.ppstream
                    and type break_style = PrettyPrint.break_style
                    and type origin      = Origin.origin
		structure NameSetPP : COLLECTIONPRETTYPRINT
                  where type ppstream    = PrettyPrint.ppstream
		sharing type Link.link = LinkSet.elt
		sharing type Name.name = Link.name = NameSet.elt = NameMap.dom
		sharing type NameSet.Set =
                             Link.nameset =
		             NameSetPP.collection) :> WIRING 
                where type link       = Link.link
		  and type linkset    = LinkSet.Set 
                  and type name       = Name.name
		  and type nameset    = NameSet.Set
                  and type 'a namemap = 'a NameMap.map =
struct
  structure Wiring = Wiring'(structure Link = Link
			     structure LinkSet = LinkSet
			     structure Name = Name
			     structure NameSet = NameSet
			     structure NameMap = NameMap
			     structure IntSet = IntSet
			     structure ErrorHandler = ErrorHandler
			     structure NameSetPP = NameSetPP)
  open Wiring
end