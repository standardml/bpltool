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
functor Wiring (structure Link : LINK
		structure LinkSet : MONO_SET
		structure Name : NAME
		structure NameSet : MONO_SET
		structure PrettyPrint : PRETTYPRINT
		structure NameSetPP : COLLECTIONPRETTYPRINT
		sharing type Link.link = LinkSet.elt
		sharing type Name.name = Link.name = NameSet.elt
		sharing type NameSet.Set = Link.nameset
                sharing type PrettyPrint.ppstream =
			     NameSetPP.ppstream
		sharing type NameSet.Set = NameSetPP.collection) :> WIRING 
                where type link    = Link.link
		  and type linkset = LinkSet.Set 
		  and type nameset = NameSet.Set
                  and type ppstream =
			   PrettyPrint.ppstream =
struct
  type link = Link.link
  type linkset = LinkSet.Set
  type nameset = NameSet.Set
  type name = NameSet.elt
  type ppstream = PrettyPrint.ppstream

  (* A nameedge is either an outer name or an internal edge. *)
  datatype nameedge = Name of name | Closure of int
  (* Internal link representation, identifying internal bigraph edges
   * by integers.
   *)
  type link' = {outer : nameedge, inner : nameset}

  (* Less-than operator on nameedges. *)
  fun nameedgelt (Name n1)    (Name n2)    = Name.unmk n1 < Name.unmk n2
    | nameedgelt (Name _)     (Closure _)  = true
    | nameedgelt (Closure _)  (Name _)     = false
    | nameedgelt (Closure i1) (Closure i2) = i1 < i2

  (* Less-than operator on link's. *)
  structure Link'Order =
  struct
  type T = link'
  fun lt ({outer = ne1, ...} : T) ({outer = ne2, ...} : T) 
      = nameedgelt ne1 ne2
  end

  structure Link'Set = OrderSet (Link'Order)
  type link'set = Link'Set.Set

(*
  (* Simple hash function for strings "c0 c1 ... cn-1" given by
   * s[0] * 31^(n-1) + s[1] * 31^(n-2) + ... + s[n-1], where
   * s[i] = ord(ci).  hash "" = 0.
   *)
  val stringhash =
      let
	fun hash' h [] = h
	  | hash' h (c :: cs) = hash' (Word.fromInt(ord (c)) + 0w31 * h) cs
      in
	hash' 0w0 o explode
      end
*)

  (* Apparently the following hash function is advocated by
   * Knuth - at the very least it actually works in Moscow ML.
   *)
  fun stringhash s = 
      let open Word
	  fun f (c,h) = 
	      xorb(xorb(<<(h,0w5),>>(h,0w27)), Word.fromInt (ord c));
      in  CharVector.foldr f 0w0 s
      end

  fun nameedgehash (Name x) = stringhash (Name.unmk x) 
    | nameedgehash (Closure i) = Word.fromInt i

  exception NOT_FOUND
  structure NameMap 
    = HashTableFn (type hash_key = name
                   val hashVal = stringhash o Name.unmk
		   val sameKey = Name.==);
  fun createNameMap size = NameMap.mkTable (size, NOT_FOUND)
  fun createNameMap' () = createNameMap 37

  structure NameEdgeMap
    = HashTableFn (type hash_key = nameedge
                   val hashVal = nameedgehash
		   val sameKey = op =);
  fun createNameEdgeMap size = NameEdgeMap.mkTable (size, NOT_FOUND)
  fun createNameEdgeMap' ()  = createNameEdgeMap 37

  (* The wiring representation used by this Wiring module is a double
   * representation, allowing faster composition.
   *)
  type wiring = link'set * nameedge NameMap.hash_table

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
	NameMap.appi
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
	val newmap = createNameMap mapsize
	fun entername ne x _ = NameMap.insert newmap (x, ne)
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

  exception InternalError 
	    of string * nameset NameEdgeMap.hash_table * nameedge * string

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
	       Name y => show (Name.unmk y)
	     | _ => ();
	     true)
	fun ppwire {outer, inner} notfirst =
	    (if notfirst then (brk(); show "* ") else ();
	     case outer of
	       Name y => show (Name.unmk y)
	     | _ => ();
	     show "/";
	     if NameSet.size inner = 1 then
	       NameSet.apply (show o Name.unmk) inner
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
	    ((case NameMap.find ht1 v of
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
	val ht = createNameMap (Link'Set.size l1s + Link'Set.size l2s)
	val i_max = ref ~1
	fun insertlinkinht offset (innername, nameedge as (Name n)) =
	    NameMap.insert ht (innername, nameedge)
	  | insertlinkinht offset (innername, Closure i) =
	    ((if !i_max < i then i_max := i else ());
	     NameMap.insert ht (innername, Closure (offset + i)))
	val _ = NameMap.appi (insertlinkinht 0) ht1
	val i2_offset = !i_max + 1
	val _ = NameMap.appi (insertlinkinht i2_offset) ht2;
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

  fun app_x (_, ht) x 
    = case NameMap.find ht x of
	SOME (Name n) => SOME n
      | _ => NONE 

  fun app w X =
      NameSet.fold ((fn (SOME n) => (fn Y => NameSet.insert n Y) 
		     | NONE => fn Y => Y)
		    o app_x w)
		   NameSet.empty
		   X

  fun app_inverse (ls, _) Y =
      Link'Set.fold (fn {outer = Name y, inner} =>
                          (fn X => if NameSet.member y Y then
                                     NameSet.union X inner
                                   else
                                     X)
                      | _ => fn X => X)
                    NameSet.empty ls

  (* Algorithm for restricting a wiring:
   * Construct an inverse hash table (mapping nameedges to
   * name sets) by looking up what each inner name maps to.
   * Then create the restricted wiring from this inverse
   * hash table.
   *)
  fun restrict (ls, ht) X =
      let
	val new_ht_inv = createNameEdgeMap (2 * Link'Set.size ls)

	fun mapmax_i (_, Closure i, i_max) = Int.max (i, i_max)
	  | mapmax_i (_, _, i_max) = i_max 

	fun addlink x i =
	    (case NameMap.find ht x of
	       SOME ne
	       => (case NameEdgeMap.find new_ht_inv ne of
		     SOME X'
		     => NameEdgeMap.insert new_ht_inv 
					   (ne, NameSet.insert x X')
		   | NONE => 
		     NameEdgeMap.insert new_ht_inv
					(ne,NameSet.singleton x);
				       i)
	     | NONE => i)
(* The following code is wrong: It causes the restriction of the
   empty wiring to become a closure of the name set X.
 
	     | NONE =>
	       (NameEdgeMap.insert new_ht_inv
				   (Closure (i + 1), NameSet.singleton x);
				  i + 1))
*)
	(* First we determine the maximum i value in closures in ht. *)
	val i_max = NameMap.foldi mapmax_i 0 ht
        (* Then we add all the links mapping from X. *)
	val i_max = NameSet.fold addlink i_max X
      in
	(invmap2link'set new_ht_inv, invert (NameSet.size X) new_ht_inv)
      end

  (** Restrict a wiring to only map to a given set of names.
   * The inner face is trimmed to include only names which maps to an
   * outer name.
   *)
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
        val new_ht = createNameMap new_ht_size
        fun add_nameedge {outer, inner}
            = NameSet.apply
                (fn n => NameMap.insert new_ht (n, outer))
                inner
      in
        (Link'Set.apply add_nameedge new_ls;
         (new_ls, new_ht))
      end

  fun id_X X =
      let
	val ht = createNameMap' ()
	fun addlink x ls =
	    (NameMap.insert ht (x, Name x);
	     Link'Set.insert
	       {outer = Name x, inner = NameSet.singleton x}
	       ls)
      in
        (NameSet.fold addlink Link'Set.empty X, ht)
      end

  val id_0 : wiring = (Link'Set.empty, createNameMap 1)

  fun introduce X =
      let
	val ht = createNameMap 1
	fun addlink x ls =
	    Link'Set.insert {outer = Name x, inner = NameSet.empty} ls
      in
	(NameSet.fold addlink Link'Set.empty X, ht)
      end

  fun close X =
      let
	val ht = createNameMap (2 * NameSet.size X)
	fun addlink x (ls, i) =
	    (NameMap.insert ht (x, Closure i);
	     (Link'Set.insert
	       {outer = Closure i, inner = NameSet.singleton x}
	       ls,
	       i + 1))
      in
	(#1 (NameSet.fold addlink (Link'Set.empty, 0) X), ht)
      end
  
  val op * = x

  val ** = foldr x id_0

  val (op o) = compose
end
