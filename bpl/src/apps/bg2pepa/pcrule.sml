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

(** Abstract data type for Precondition-Change-Rules (PC-rules or simply
 * PCRs).
 * @version $LastChangedRevision: 2717 $
 *)
functor PCRule(
  structure Info : INFO
  structure Name : NAME
  structure NameMap : MONO_FINMAP
  structure WordMap : MONO_FINMAP
    where type dom = word
  structure ControlSet : MONO_SET
  structure Node : NODE
  structure NodeMap : MONO_FINMAP
  structure EdgeMap : MONO_FINMAP
  structure BgAspects : BGASPECTS
    where type root = word
  structure ConcreteBigraph : CONCRETE_BIGRAPH
    where type translation = {rho_V : Node.node NodeMap.map,
                              rho_E : Name.name EdgeMap.map}
  structure Instantiation : INSTANTIATION
  structure BgVal : BGVAL
  structure BgBDNF : BGBDNF
  structure ReactionRule : RULE
  sharing type Info.info =
               ReactionRule.info
  sharing type ControlSet.Set =
               ConcreteBigraph.controlset
  sharing type BgVal.bgval =
               BgBDNF.bgval =
               ConcreteBigraph.absbg = 
               ReactionRule.bgval
  sharing type Instantiation.inst =
               ReactionRule.inst =
               BgAspects.inst
  sharing type BgBDNF.bgbdnf =
               ReactionRule.bgbdnf
  sharing type BgBDNF.BR =
               ReactionRule.BR
  sharing type Instantiation.interface =
               BgVal.interface =
               BgBDNF.interface =
               BgAspects.interface
  sharing type Name.name =
               NameMap.dom =
               BgVal.name =
               BgAspects.name
=(*  sharing type Edge.edge =*)
               EdgeMap.dom =
               BgAspects.edge
  sharing type Node.node =
               NodeMap.dom =
               BgAspects.node
  sharing type BgAspects.ChildSet.Set =
               ConcreteBigraph.childset
  sharing type BgAspects.place =
               ConcreteBigraph.place
  sharing type BgAspects.aspect =
               ConcreteBigraph.aspect
  sharing type BgAspects.value =
               ConcreteBigraph.value
  sharing type BgAspects.change =
               ConcreteBigraph.change
  sharing type BgAspects.AspectMap.map =
               ConcreteBigraph.aspectmap
) : PCRULE
  where type preconds      = (BgAspects.aspect * BgAspects.value) list
    and type changes       = BgAspects.change list
    and type nodeset       = ConcreteBigraph.nodeset
    and type edgeset       = ConcreteBigraph.edgeset
    and type controlset    = ConcreteBigraph.controlset
    and type conbg         = ConcreteBigraph.conbg
    and type reaction_rule = ReactionRule.rule
    and type 'a entitymap  = 'a BgAspects.EntityMap.map
    and type 'a aspectmap  = 'a BgAspects.AspectMap.map
    and type translation   = ConcreteBigraph.translation
    and type instantiation = {rho_V : Node.node NodeMap.map,
                              rho_E : Name.name EdgeMap.map,
                              map_Y : BgAspects.link NameMap.map,
                              map_R : BgAspects.place WordMap.map} =
struct

  open BgAspects

  type preconds      = (aspect * value) list
  type changes       = change list
  type nodeset       = ConcreteBigraph.nodeset
  type edgeset       = ConcreteBigraph.edgeset
  type controlset    = ConcreteBigraph.controlset
  type conbg         = ConcreteBigraph.conbg
  type reaction_rule = ReactionRule.rule
  type translation   = ConcreteBigraph.translation
  type instantiation = {rho_V : Node.node NodeMap.map,
                        rho_E : Name.name EdgeMap.map,
                        map_Y : link NameMap.map,
                        map_R : place WordMap.map}
  type 'a entitymap  = 'a EntityMap.map
  type 'a aspectmap  = 'a AspectMap.map

  fun map2string lb rb sep
                 (Fold : (('dom * 'rng) * (string * bool)
                          -> (string * bool))
                         -> (string * bool) -> 'map -> (string * bool))
                 entry2string map =
    lb ^ (#1 (Fold
                (fn (e, (s, first)) =>
                    ((if first then s else s ^ sep)
                     ^ (entry2string e), false))
                ("", true) map)) ^ rb

  fun map2string' lb rb sep elb erb esep
                  (Fold : (('dom * 'rng) * (string * bool)
                           -> (string * bool))
                          -> (string * bool) -> 'map -> (string * bool))
                  dom2string rng2string map =
      map2string lb rb sep Fold
                 (fn (k, v) => elb ^ (dom2string k) 
                               ^ esep ^ (rng2string v) ^ erb)
                 map

  type pcrule = {name : string, redex : conbg, changes : changes,
                 reactum : conbg, table : (value * value) aspectmap}

  (* FIXME check that the rule has no inner names *)
  fun make (pcrule as {name, preconds, changes}) =
    (* FIXME Check consistency of preconditions and changes. *)
    let
      val redex   = ConcreteBigraph.make' preconds

      val reactum = ConcreteBigraph.app changes redex

      (* FIXME inefficient implementation *)
      val pre_vals  = ConcreteBigraph.unmk'' redex
      val post_vals = ConcreteBigraph.unmk'' reactum
      fun merge_entry (aspect, value) =
        (value, valOf (AspectMap.lookup post_vals aspect))
      val table = AspectMap.ComposeMap merge_entry pre_vals
    in
      {name     = name,
       redex    = redex,
       changes  = changes,
       reactum  = reactum,
       table    = table}
    end

  fun unmk (pcrule as {name, redex, changes, ...} : pcrule) =
    {name     = name,
     preconds = ConcreteBigraph.unmk' redex,
     changes  = changes}

  fun unmk' (pcrule as {name, redex, changes, ...} : pcrule) =
    {name    = name,
     redex   = redex,
     changes = changes}

  fun unmk'' (pcrule as {name, table, ...} : pcrule) =
    {name  = name,
     table = table}

  fun name ({name, ...} : pcrule) = name

  fun redex ({redex, ...} : pcrule) = redex

  fun reaction_rule ({name, redex, reactum, ...} : pcrule) =
    let
      val redex   = (BgBDNF.regularize o BgBDNF.make
                     o ConcreteBigraph.abstract)
                      redex
      val reactum = ConcreteBigraph.abstract reactum
      (* FIXME currently we assume identity instantiations *)
      val inst    = Instantiation.make' {I = BgBDNF.innerface redex,
                                         J = BgVal.innerface reactum}
(*      val inst    = inst {changes = changes,
                                    I = BgBDNF.innerface redex,
                                    J = BgVal.innerface reactum}*)
    in
      ReactionRule.make {name = name,
                         redex = redex, react = reactum,
                         inst = inst, info = Info.noinfo}
    end

  fun support (rule : pcrule) =
    (ConcreteBigraph.support o #redex) rule

  fun width ({redex, ...} : pcrule) = ConcreteBigraph.width redex

  fun controls ({redex, reactum, ...} : pcrule) =
    ControlSet.union' (ConcreteBigraph.controls redex)
                      (ConcreteBigraph.controls reactum)
    
  (* FIXME rho_* should be injections *)
  fun translate (trns as {rho_V, rho_E})
                {name, redex, changes, reactum, table} =
    let
      val redex' = ConcreteBigraph.translate trns redex

      fun trans_v v
        = (valOf (NodeMap.lookup rho_V v))
          handle Option =>
                 raise Fail "FIXME translation doesn't map the node v"
      fun trans_e e
        = (valOf (EdgeMap.lookup rho_E e))
          handle Option =>
                 raise Fail "FIXME translation doesn't map the edge e"
      fun trans_c (s as (CSite _)) = s
        | trans_c (CNode v)        = CNode (trans_v v)
      fun trans_p (r as (PRoot _)) = r
        | trans_p (PNode v)        = PNode (trans_v v)
      fun trans_l (n as (LName _)) = n
        | trans_l (LEdge e)        = LEdge (trans_e e)
      fun trans_pnt (n as (PName _)) = n
        | trans_pnt (PPort (v, i))   = PPort (trans_v v, i)
      fun trans_ent (ENode v) = ENode (trans_v v)
        | trans_ent (EEdge e) = EEdge (trans_e e)
        | trans_ent e         = e

      fun trans_aspect (Presence e)    = Presence    (trans_ent e)
        | trans_aspect (NodeControl v) = NodeControl (trans_v v)
        | trans_aspect (ChildParent c) = ChildParent (trans_c c)
        | trans_aspect (PointLink p)   = PointLink   (trans_pnt p)

      fun trans_change (Del c)             = Del (trans_c c)
        | trans_change (Mov (c, p))        = Mov (trans_c c, trans_p p)
        | trans_change (Cop (s, p))        = Cop (s, trans_p p)
        | trans_change (Add (v, c, ls, p)) = 
          Add (trans_v v, c, map trans_l ls, trans_p p)
        | trans_change (Con ((v, i), l))   =
          Con ((trans_v v, i), trans_l l)

      fun trans_val (Place p) = Place (trans_p p)
        | trans_val (Link l)  = Link  (trans_l l)
        | trans_val v         = v

      fun trans_vals (v1, v2) = (trans_val v1, trans_val v2)
    in
      {name     = name,
       redex    = redex',
       changes  = map trans_change changes,
       reactum  = ConcreteBigraph.translate trns reactum,
       table    = AspectMap.Fold
                    (fn ((aspect, vals), table') =>
                        AspectMap.add (trans_aspect aspect,
                                       trans_vals vals, table'))
                    AspectMap.empty table}
    end

  (* compute the instantiation of a rule in an agent given a support
   * translation as well as maps for roots and names.
   *
   * FIXME
   * - rho_* should be injections
   * - rho_* should have co-domains disjoint from the co-domains
   *   of map_*
   *)
  fun instantiate (inst as {rho_V, rho_E, map_Y, map_R})
                  (pcrule as {table, redex, ...} : pcrule) =
    let
      fun inst_v v
        = (valOf (NodeMap.lookup rho_V v))
          handle Option =>
                 raise Fail "FIXME instantiation doesn't map the node v"
      fun inst_e e
        = (valOf (EdgeMap.lookup rho_E e))
          handle Option =>
                 raise Fail "FIXME instantiation doesn't map the edge e"
      fun inst_y y
        = (valOf (NameMap.lookup map_Y y))
          handle Option =>
                 raise Fail ("FIXME instantiation doesn't map name "
                             ^ Name.unmk y)
      fun inst_r r
        = (valOf (WordMap.lookup map_R r))
          handle Option =>
                 raise Fail ("FIXME instantiation doesn't map root "
                             ^ Word.toString r)
      fun inst_c (s as (CSite _)) = s
        | inst_c (CNode v)        = CNode (inst_v v)
      fun inst_p (PRoot r)        = inst_r r
        | inst_p (PNode v)        = PNode (inst_v v)
      fun inst_l (LName y)        = inst_y y
        | inst_l (LEdge e)        = LEdge (inst_e e)
      fun inst_pnt (n as (PName _)) =
          raise
            Fail "FIXME (pure) reaction rules must not have inner names"
        | inst_pnt (PPort (v, i))   = PPort (inst_v v, i)
      fun inst_ent (ENode v) = ENode (inst_v v)
        | inst_ent (EEdge e) = EEdge (inst_e e)
        | inst_ent (EName y) = (case inst_y y of
                                  LName y' => EName y'
                                | LEdge y' => EEdge y')
        | inst_ent (ERoot r) = (case inst_r r of
                                  PRoot r' => ERoot r'
                                | PNode v' => ENode v')

      fun inst_aspect (Presence e)    = Presence    (inst_ent e)
        | inst_aspect (NodeControl v) = NodeControl (inst_v v)
        | inst_aspect (ChildParent c) = ChildParent (inst_c c)
        | inst_aspect (PointLink p)   = PointLink   (inst_pnt p)

      fun inst_val (Place p)   = Place (inst_p p)
        | inst_val (Link l)    = Link  (inst_l l)
        | inst_val v           = v

      fun inst_vals (v1, v2) = (inst_val v1, inst_val v2)

      fun link2ent (LName y) = EName y
        | link2ent (LEdge e) = EEdge e
      fun place2ent (PRoot r) = ERoot r
        | place2ent (PNode v) = ENode v

      (* We don't support rules that alter parameters
       * (incl. moving them).
       * This means that we can ignore parameters altogether. *)
      fun inst_aspect_vals ((ChildParent (CSite s), (value, value')),
                            tableXcount_changes) =
        if valueEq value value' then
          tableXcount_changes
        else
          raise Fail "FIXME we don't support altering parameters"
          (* Outer names can be instantiated to links with an arbitrary
           * number of points so we return the relative change.
           * NB: several names can be instantiated to the same link. *)
        | inst_aspect_vals ((Presence (EName y),
                             (Present point_count,
                              Present point_count')),
                            (table', count_changes)) =
        let
          val e' = link2ent (inst_y y)
          val c = case EntityMap.lookup count_changes e' of
                    NONE   => 0
                  | SOME c => c
          val c' = (Word.toInt point_count') - (Word.toInt point_count)
        in
          (table', EntityMap.add (e', c + c', count_changes))
        end
          (* Roots are similar to outer names in that they can be
           * instantiated to a location with an arbitrary number of
           * children.
           * FIXME this and the above case should be combined... *)
        | inst_aspect_vals ((Presence (ERoot r),
                             (Present child_count,
                              Present child_count')),
                            (table', count_changes)) =
        let
          val e' = place2ent (inst_r r)
          val c = case EntityMap.lookup count_changes e' of
                    NONE   => 0
                  | SOME c => c
          val c' = (Word.toInt child_count') - (Word.toInt child_count)
        in
          (table', EntityMap.add (e', c + c', count_changes))
        end
        (* Nodes that has at least one child-site can have an arbitrary
         * number of children, so we return the relative change to the
         * number of children. *)
        | inst_aspect_vals ((Presence (ENode v),
                             vals as (Present child_count,
                                      Present child_count')),
                            (table', count_changes)) =
        let
          val has_site
            = not (ChildSet.all
                     (fn (CNode _) => true | _ => false)
                     (ConcreteBigraph.children redex (PNode v)))

          val e' = ENode (inst_v v)

          (* NB: this is only sound because we don't allow sites to be
           *     moved/deleted/copied *)
          val c' = (Word.toInt child_count') - (Word.toInt child_count)
        in
          if has_site then
            (table', EntityMap.add (e', c', count_changes))
          else
            (AspectMap.add (Presence e', vals, table'),
             count_changes)
        end
        | inst_aspect_vals ((aspect, vals), (table', count_changes)) =
        (AspectMap.add (inst_aspect aspect, inst_vals vals, table'),
         count_changes)
    in
(      AspectMap.Fold inst_aspect_vals
                      (AspectMap.empty, EntityMap.empty)
                      table
) handle e =>
( print (name pcrule ^ "\n")
; print (map2string' "[" "]" ",\n " "(" ")" ", " AspectMap.Fold
         aspect2string (fn (v1, v2) => value2string v1 ^ ", " ^ value2string v2) table)
; raise e)
    end
end
