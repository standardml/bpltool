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

(* Abstract data type for modelling a set of constraints on a partial
 * bijection b on a set U. The constraints are simply pairs (S,T) with
 * S,T \subseteq U  and  |S| = |T|  to be interpreted as "b restricted
 * to S must be a total bijection between S and T".
 *
 * @version $LastChangedRevision$
 *)
functor BijectionConstraints
  (structure Set : MONO_SET
   structure Map : MONO_FINMAP
   sharing type Set.elt = Map.dom
  ) :> BIJECTION_CONSTRAINTS where type set = Set.Set =
struct
  type set = Set.Set
    
  (* The constraints are kept in a list as well as a map mapping elements
   * of the domain to the constraint it is mentioned in.
   * The elements of the list are pairwise disjoint and the union of the
   * first element of all the pairs is the domain and similarly for the
   * codomain.
   *)
  type constraints = {  list : (set * set) list
                      , map : (set * set) Map.map
                      , dom : set, rng : set}

  val empty = {  list = []
               , map = Map.empty
               , dom = Set.empty
               , rng = Set.empty}
    : constraints

  fun list {list, map, dom, rng} = list

  exception InvalidConstraint of set * set

  exception Overlap of constraints * set * set

  fun add ((S, T), C as {list, map, dom, rng}) =
      if Set.size S <> Set.size T then
        raise InvalidConstraint (S, T)
      else if not (Set.isEmpty (Set.intersect S dom))
              orelse not (Set.isEmpty (Set.intersect T rng))
      then
        raise Overlap (C, S, T)
      else if Set.size S = 0 then
        C
      else
        {  list = (S, T) :: list
         , map  = Set.fold (fn s => fn C => Map.add (s, (S, T), C)) map S
         , dom  = Set.union dom S
         , rng  = Set.union rng T}

  fun add_list (cs, C) = foldr add C cs

  val from_list = foldr add empty

  exception IncompatibleConstraints of constraints * constraints

  fun plus ((C1 as {list = list1, map = map1, dom = dom1, rng = rng1}),
            (C2 as {list = list2, map = map2, dom = dom2, rng = rng2})) =
      if not (Set.isEmpty (Set.intersect dom1 dom2)) orelse
         not (Set.isEmpty (Set.intersect rng1 rng2)) then
        raise IncompatibleConstraints (C1, C2)
      else
        {  list = list1 @ list2
         , map  = Map.plus (map1, map2)
         , dom  = Set.union dom1 dom2
         , rng  = Set.union rng1 rng2}

  fun check_constraints
        add
        ((C1 as {list = list1, map = map1, dom = dom1, rng = rng1}),
         (C2 as {list = list2, map = map2, dom = dom2, rng = rng2})) =
      if not (Set.eq dom1 dom2) orelse not (Set.eq rng1 rng2) then
        raise IncompatibleConstraints (C1, C2)
      else
        let
          fun check_constraint [] C = (true, C)
            | check_constraint ((S, T)::Cs) C =
              if Set.size S <> 0 then
                let
                  val (S', T') = valOf (Map.lookup map1 (Set.someElement S))
                  val S'' = Set.intersect S S'
                  val T'' = Set.intersect T T'
                in
                  if Set.size S'' <> Set.size T'' then
                    (false, empty)
                  else
                    check_constraint
                      ((Set.difference S S'', Set.difference T T'')::Cs)
                      (add ((S'', T''), C))
                end
              else
                check_constraint Cs C
        in
          check_constraint list2 empty
        end

  fun combine (C1, C2) =
      case check_constraints add (C1, C2) of
        (true, C)  => SOME C
      | (false, _) => NONE

  val are_combineable = #1 o (check_constraints (fn _ => empty))

  exception InvalidRestriction of constraints * (set * set)

  fun restrict (C as {list, map, dom, rng}, res as (dom', rng')) =
      let
        fun restrict' ((S, T), C) =
            let
              val S' = Set.intersect S dom'
              val T' = Set.intersect T rng'
            in
              add ((S', T'), C)
            end
      in
        if Set.size dom' = Set.size rng'
          andalso Set.size dom' = Set.size (Set.intersect dom dom')
          andalso Set.size rng' = Set.size (Set.intersect rng rng')
        then
          SOME (foldr restrict' empty list)
          handle InvalidConstraint _ => NONE
        else
          raise InvalidRestriction (C, res)
      end
end
