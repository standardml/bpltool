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

(** Abstract data type for modelling names.
 * 
 * @version $LastChangedRevision$
 *)
structure Name :> NAME =
struct
  (* Names are identified by unique words. *)
  type name = word * string

  fun == ((id1, _) : name, (id2, _) : name) =
      id1 = id2

  fun lt ((id1, _) : name, (id2, _) : name) =
      id1 < id2

  fun compare ((id1, _) : name, (id2, _) : name) =
      Word.compare(id1,id2)

  val op < = lt

  (* Keep track of used ids *)
  val next_id = ref 0w0

  fun fresh' s =
      let
        val id = !next_id
      in
        (next_id := !next_id + 0w1;
         (id, s))
      end

  fun fresh (SOME (_, s)) =
      fresh' s
    | fresh NONE =
      fresh' ""

  fun hash (w, _) = w

  (* make must always return the same name when given the same
   * string. So we store the returned names in a hash table. *)

  (* Apparently the following hash function is advocated by
   * Knuth - at the very least it actually works in Moscow ML. *)
  fun stringhash s = 
      let
        open Word
        fun f (c,h) = 
            xorb(xorb(<<(h,0w5),>>(h,0w27)), Word.fromInt (ord c))
      in
        CharVector.foldr f 0w0 s
      end

  exception NOT_FOUND
  structure StringHash =
      HashTableFn (type hash_key = string
                   val  hashVal = stringhash
                   val  sameKey = (op = : string * string -> bool))
  val name_map =
      StringHash.mkTable (37, NOT_FOUND) : name StringHash.hash_table

  fun make s =
      case StringHash.find name_map s of
        SOME n => n
      | NONE   =>
        let
          val n = fresh' s
        in
          (StringHash.insert name_map (s, n);
           n)
        end
  fun ekam ((_, s) : name) = s
  fun unmk ((id, s) : name) = s ^ "_" ^ (String.map Char.toLower (Word.toString id))
 

  structure Order : ORDERING =
  struct 
    type T = name 
    fun lt n1 n2 = n1 < n2
  end
(*
  structure NameSet : MONO_SET =
  struct
    type elt = name
    type Set = elt Rbset.set
    val empty : Set = Rbset.empty compare
    val singleton = Rbset.singleton compare
    val size = Rbset.numItems
    val isEmpty = Rbset.isEmpty
    fun member n ns = Rbset.member(ns,n)
    fun eq s1 s2 = Rbset.equal(s1,s2)
    exception DuplicatesRemoved = Rbset.AlreadyThere
    val list = Rbset.listItems
    fun addList ns s = Rbset.addList(s,ns)
    fun fromList ns = addList ns empty
    fun insert n ns = Rbset.add(ns,n)
    fun insert' n ns = Rbset.add'(ns,n)
    fun remove n ns = Rbset.delete(ns,n)
    fun difference s1 s2 = Rbset.difference(s1,s2)
    fun intersect s1 s2 = Rbset.intersection(s1,s2)
    fun union s1 s2 = Rbset.union(s1,s2)
    fun union' s1 s2 = Rbset.union'(s1,s2)
    fun partition f s = raise Fail("Not implemented: partition")
    fun subst (a,b) s = raise Fail("Not implemented: subst")
    fun fold f = Rbset.foldl (fn(e,b)=>f e b)
    fun foldUntil f = Rbset.foldlUntil (fn(e,b)=>f e b)
    fun map f = Rbset.map(f,compare)
    val apply = Rbset.app

    type StringTree = EdLibPrettyPrint.StringTree
    fun layoutSet {start, sep, finish} layoutItem s =
      EdLibPrettyPrint.NODE {start=start,
			     finish=finish,
			     indent=3,
			     childsep=EdLibPrettyPrint.RIGHT sep,
			     children=List.map layoutItem (list s)}

  end
*)
  structure NameSet = Rbset(structure Order = struct
			      type t = name
                              val compare = compare
                            end)
end
