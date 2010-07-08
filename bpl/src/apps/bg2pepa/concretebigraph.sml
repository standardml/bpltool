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

(** Abstract data type for modelling concrete bigraphs.
 * @version $LastChangedRevision: 2717 $
 *)
functor ConcreteBigraph(
  structure Info : INFO
  structure Name : NAME
  structure NameSet : MONO_SET
  structure NameMap : MONO_FINMAP
  structure WordMap : MONO_FINMAP
    where type dom = word
  structure Control : CONTROL
  structure ControlSet : MONO_SET
  structure ControlMap : MONO_FINMAP
  structure Interface : INTERFACE
  structure Ion : ION
  structure Link : LINK
  structure Permutation : PERMUTATION
  structure Wiring : WIRING
  structure BgVal : BGVAL
  structure BgBDNF : BGBDNF
  structure BgAspects : BGASPECTS
    where type site      = word
      and type root      = word
      and type portindex = word
  structure Node : NODE
  structure NodeSet : MONO_SET
  structure NodeMap : MONO_FINMAP
  structure Edge : EDGE
  structure EdgeSet : MONO_SET
  structure EdgeMap : MONO_FINMAP
  sharing type Info.info =
               BgVal.info
  sharing type Name.name =
               NameSet.elt =
               NameMap.dom =
               Ion.name =
               Link.name =
               Wiring.name =
               BgVal.name =
               BgAspects.name
=(*  sharing type Edge.edge =*)
               EdgeSet.elt =
               EdgeMap.dom =
               BgAspects.edge
  sharing type NameSet.Set =
               EdgeSet.Set =
               Interface.nameset =
               Link.nameset =
               Permutation.nameset =
               Wiring.nameset
  sharing type NameMap.map =
               Wiring.namemap
  sharing type Interface.interface =
               BgVal.interface =
               BgBDNF.interface =
               BgAspects.interface
  sharing type Link.link =
               Wiring.link
  sharing type Permutation.permutation =
               BgVal.permutation =
               BgBDNF.permutation
  sharing type Node.node =
               NodeSet.elt =
               NodeMap.dom =
               BgAspects.node
  sharing type BgVal.bgval =
               BgBDNF.bgval
  sharing type Control.control =
               ControlSet.elt =
               ControlMap.dom =
               Ion.control =
               BgAspects.control =
               BgVal.control
  sharing type Ion.ion =
               BgVal.ion =
               BgBDNF.ion
  sharing type Wiring.wiring =
               BgVal.wiring =
               BgBDNF.wiring
) : CONCRETE_BIGRAPH
  where type nodeset       = NodeSet.Set
    and type 'a nodemap    = 'a NodeMap.map
    and type edgeset       = EdgeSet.Set
    and type nameset       = NameSet.Set
    and type controlset    = ControlSet.Set
    and type 'a controlmap = 'a ControlMap.map
    and type childset      = BgAspects.ChildSet.Set
    and type absbg         = BgVal.bgval
    and type node          = Node.node
    and type place         = BgAspects.place
    and type pointset      = BgAspects.PointSet.Set
    and type link          = BgAspects.link
    and type aspect        = BgAspects.aspect
    and type value         = BgAspects.value
    and type change        = BgAspects.change
    and type 'a aspectmap  = 'a BgAspects.AspectMap.map
    and type translation   = {rho_V : Node.node NodeMap.map,
                              rho_E : Name.name EdgeMap.map} =
struct

open BgAspects

type absbg         = BgVal.bgval
type nodeset       = NodeSet.Set
type 'a nodemap    = 'a NodeMap.map
type edgeset       = EdgeSet.Set
type nameset       = NameSet.Set
type controlset    = ControlSet.Set
type 'a controlmap = 'a ControlMap.map
type childset      = ChildSet.Set
type pointset      = PointSet.Set
type 'a aspectmap  = 'a AspectMap.map

type translation = {rho_V : Node.node NodeMap.map,
                    rho_E : Name.name EdgeMap.map}

type edit_script = unit (*FIXME*)

(** The concrete bigraph data type. *)
type conbg = {V : NodeSet.Set,
              E : EdgeSet.Set,
              ctrl : control NodeMap.map,
              prnt : place ChildMap.map,
              (* the inverse of the prnt map.
               * NB: prnt_inv(n) = NONE is equivalent to
               *     prnt_inv(n) = SOME ChildSet.empty  *)
              prnt_inv : ChildSet.Set PlaceMap.map,
              (* the link map is represented by a map for
               * innernames and one for ports *)
              nlink : link NameMap.map,
              (* NB: plink(n) = NONE is equivalent to
               *     plink(n) = SOME WordMap.empty  *)
              plink : (link WordMap.map) NodeMap.map,
              (* the inverse of te link map.
               * NB: link_inv(l) = NONE is equivalent to
               *     link_inv(l) = SOME PointSet.empty  *)
              link_inv : PointSet.Set LinkMap.map,
              innerface : interface,
              outerface : interface}
(* utility functions for using the maps *)
(* set the control of n to be c.
 * Will _not_ raise an exception if n is already in the map. *)
fun add_ctrl c n ctrl =
  NodeMap.add (n, c, ctrl)
(* As above, but will raise an exception if n is already in the map. *)
fun add_ctrl' c n ctrl =
  NodeMap.add' (fn _ => false) (n, c, ctrl)
fun get_ctrl ctrl n =
  case NodeMap.lookup ctrl n of
    NONE => raise Fail "FIXME this shouldn't happen for a consistent conbg"
  | SOME c => c
(* remove the control for node n.
 * Will _not_ raise an exception if n is not in the map. *)
fun remove_ctrl ctrl n =
  case NodeMap.remove (n, ctrl) of
    NONE => ctrl
  | SOME ctrl' => ctrl'
(* As above, but will raise an exception if n is not in the map. *)
fun remove_ctrl' ctrl n =
  case NodeMap.remove (n, ctrl) of
    NONE => raise Fail "FIXME node has no control"
  | SOME ctrl' => ctrl'
(* set the parent of c to be p.
 * Will _not_ raise an exception if c is already in the map. *)
fun add_prnt p c prnt =
  ChildMap.add (c, p, prnt)
(* As above, but will raise an exception if c is already in the map. *)
fun add_prnt' p c prnt =
  ChildMap.add' (fn _ => false) (c, p, prnt)
fun get_children prnt_inv p =
  case PlaceMap.lookup prnt_inv p of
    NONE   => ChildSet.empty
  | SOME m => m
fun add_child p c prnt_inv =
  PlaceMap.add (p,
                ChildSet.insert c (get_children prnt_inv p),
                prnt_inv)
fun remove_child prnt_inv p c =
  PlaceMap.add (p,
                ChildSet.remove c (get_children prnt_inv p),
                prnt_inv)
(* set the ports of n to be as given by n_ps.
 * Will _not_ raise an exception if n is already in the map. *)
fun add_ports n_ps n plink =
  NodeMap.add (n, n_ps, plink)
(* As above, but will raise an exception if n is already in the map. *)
fun add_ports' n_ps n plink =
  NodeMap.add' (fn _ => false) (n, n_ps, plink)
fun get_ports plink n =
  case NodeMap.lookup plink n of
    NONE   => WordMap.empty
  | SOME m => m
fun get_port (n, i) plink =
  case WordMap.lookup (get_ports plink n) i of
    NONE => raise
              Fail "FIXME this shouldn't happen for a consistent conbg"
  | SOME l => l
fun add_port l (n, i) plink =
  add_ports 
    (WordMap.add (i, l, (get_ports plink n)))
    n plink
fun get_points link_inv l =
  case LinkMap.lookup link_inv l of
    NONE   => PointSet.empty
  | SOME m => m
fun add_point l p link_inv =
  LinkMap.add (l,
               PointSet.insert p (get_points link_inv l),
               link_inv)
fun remove_point link_inv l p =
  LinkMap.add (l,
               PointSet.remove p (get_points link_inv l),
               link_inv)

fun link2name (LName n) = n
  | link2name (LEdge e) = e

fun nodes (conbg : conbg) = #V conbg
fun edges (conbg : conbg) = #E conbg
fun innernames (conbg : conbg) = (Interface.glob o #innerface) conbg
fun outernames (conbg : conbg) = (Interface.glob o #outerface) conbg

fun support (conbg : conbg) =
  {nodes = nodes conbg, edges = edges conbg}

fun width ({outerface, ...} : conbg) =
  Interface.width outerface

fun controls ({ctrl, ...} : conbg) =
  NodeMap.fold (fn (c, s) => ControlSet.insert' c s)
               ControlSet.empty ctrl

fun ctrl ({ctrl, ...} : conbg) = ctrl

fun ctrl_inv ({ctrl, ...} : conbg) =
  NodeMap.Fold
    (fn ((v, K), ctrl_inv) =>
        ControlMap.add
          (K,
           case ControlMap.lookup ctrl_inv K of
             NONE   => NodeSet.singleton v
           | SOME V => NodeSet.insert v V,
           ctrl_inv))
    ControlMap.empty ctrl

fun children ({prnt_inv, ...} : conbg) p = get_children prnt_inv p

fun points ({link_inv, ...} : conbg) l = get_points link_inv l

fun idle_names ({outerface, link_inv, ...} : conbg) =
  NameSet.fold
    (fn y => fn idle_ns =>
        if PointSet.isEmpty (get_points link_inv (LName y)) then
          NameSet.insert y idle_ns
        else
          idle_ns)
    NameSet.empty (Interface.glob outerface)

fun translate {rho_V, rho_E}
              {V, E, ctrl, prnt, prnt_inv,
               nlink, plink, link_inv, innerface, outerface} =
  let
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
    fun trans_point (n as(PName _)) = n
      | trans_point (PPort (v, i))  = PPort (trans_v v, i)
    fun id x = x
  in
    if NodeSet.size V = NodeMap.size rho_V andalso
       EdgeSet.size E = EdgeMap.size rho_E then
      {V         = NodeSet.map trans_v V,
       E         = EdgeSet.map trans_e E,
       ctrl      = NodeMap.translate trans_v id ctrl,
       prnt      = ChildMap.translate trans_c trans_p prnt,
       prnt_inv  = PlaceMap.translate
                     trans_p (ChildSet.map trans_c) prnt_inv,
       nlink     = NameMap.translate id trans_l nlink,
       plink     = NodeMap.translate
                     trans_v
                     (WordMap.translate id trans_l)
                     plink,
       link_inv  = LinkMap.translate
                     trans_l (PointSet.map trans_point) link_inv,
       innerface = innerface,
       outerface = outerface}
    else
      raise Fail "FIXME translation maps too few or too many \
                 \nodes/edges"
  end

(* Exception used to signal that a node is part of a non-tree. *)
exception NotTree of node

(* Construct a concrete bigraph from a set of aspects.
 * NB: ordering matters: aspects must be declared before they are
 *     referenced.
 * FIXME: check that the above requirement is adhered to.
 * FIXME: check that aspects with (Present count) have exactly
 *        count children/points. *)
local
  (* Insert a word into an interval representation of a word set.
   * Raise exception if the word is already in the set. *)
  exception DuplicateWord
  fun insert_word (i, []) = [(i, i)]
    | insert_word (i, (rs as ((r as (l, h)) :: rs'))) =
      if i + 0w1 < l then (i, i) :: rs  else
      if i + 0w1 = l then (i, h) :: rs' else
      if i >= l andalso i <= h then
        raise DuplicateWord
      else
      if i = h + 0w1 then case rs' of
                            [] => [(l, i)]
                          | (l', h') :: rs'' => if i + 0w1 = l' then
                                                  (l, h') :: rs''
                                                else
                                                 (l, i) :: rs'
      else r :: (insert_word (i, rs'))
  (* as above, except that duplicates are ignored *)
  fun insert_word' (i, rs) =
    (insert_word (i, rs)) handle DuplicateWord => rs

  (* Add an aspect to a concrete bigraph.
   * Repeated aspects FIXME *)
  fun insert_aspect ((Presence (ENode v), Present count),
                     (V, E, ctrl, prnt, prnt_inv, nlink, plink,
                      link_inv, sites, roots, innerns, outerns)) =
      ((NodeSet.insert v V, E, ctrl, prnt, prnt_inv,
        nlink, plink, link_inv, sites, roots, innerns, outerns)
       handle NodeSet.DuplicatesRemoved =>
              raise Fail "FIXME node n is already in the graph")
    | insert_aspect ((Presence (EEdge e), Present count),
                     (V, E, ctrl, prnt, prnt_inv, nlink, plink,
                      link_inv, sites, roots, innerns, outerns)) =
      ((V, EdgeSet.insert e E, ctrl, prnt, prnt_inv, nlink, plink,
        link_inv, sites, roots, innerns, outerns)
       handle EdgeSet.DuplicatesRemoved =>
              raise Fail "FIXME edge e is already in the graph")
    | insert_aspect ((Presence (EName n), Present count),
                     (V, E, ctrl, prnt, prnt_inv, nlink, plink,
                      link_inv, sites, roots, innerns, outerns)) =
      ((V, E, ctrl, prnt, prnt_inv, nlink, plink, link_inv, sites,
        roots, innerns, NameSet.insert n outerns)
       handle NameSet.DuplicatesRemoved =>
              raise Fail "FIXME name n is already in the graph")
    | insert_aspect ((Presence (ERoot r), Present count),
                     (V, E, ctrl, prnt, prnt_inv, nlink, plink,
                      link_inv, sites, roots, innerns, outerns)) =
      ((V, E, ctrl, prnt, prnt_inv, nlink, plink, link_inv, sites,
        insert_word (r, roots),
        innerns, outerns)
       handle DuplicateWord =>
              raise Fail "FIXME root r is already in the graph")
    | insert_aspect ((Presence e, Absent), _) =
      raise Fail "FIXME entities shouldn't be declared absent"
    | insert_aspect ((NodeControl n, Control c),
                     (V, E, ctrl, prnt, prnt_inv, nlink, plink,
                      link_inv, sites, roots, innerns, outerns)) =
      if NodeSet.member n V then
        (V, E, add_ctrl' c n ctrl,
         prnt, prnt_inv, nlink, plink, link_inv, sites, roots,
         innerns, outerns)
        handle NodeMap.DATACHANGED =>
               raise Fail "FIXME node n already has a control"
      else
        raise Fail "FIXME node n is not in the graph"
    | insert_aspect ((ChildParent c, Place p),
                     (V, E, ctrl, prnt, prnt_inv, nlink, plink,
                      link_inv, sites, roots, innerns, outerns)) =
      let
        val sites' = case c of
                       CSite s =>
                       (insert_word (s, sites)
                        handle DuplicateWord =>
                               raise Fail "FIXME site s is \
                                          \already in the graph")
                     | _ => sites
      in
        (V, E, ctrl,
         add_prnt' p c prnt,
         add_child p c prnt_inv,
         nlink, plink, link_inv, sites', roots, innerns, outerns)
        handle ChildMap.DATACHANGED =>
               raise Fail "FIXME child c already has a parent"
      end
    | insert_aspect ((PointLink (p as (PName n)), Link l),
                     (V, E, ctrl, prnt, prnt_inv, nlink, plink,
                      link_inv, sites, roots, innerns, outerns)) =
      ((V, E, ctrl, prnt, prnt_inv,
        NameMap.add' (fn _ => false) (n, l, nlink),
        plink,
        add_point l p link_inv,
        sites, roots,
        NameSet.insert n innerns,
        outerns)
       handle NameMap.DATACHANGED =>
              raise Fail "FIXME inner name n already has a connection"
            | NameSet.DuplicatesRemoved =>
              raise Fail "FIXME inner name n is already in the graph")
    | insert_aspect ((PointLink (p as (PPort (n, i))), Link l),
                     (V, E, ctrl, prnt, prnt_inv, nlink, plink,
                      link_inv, sites, roots, innerns, outerns)) =
      let
        val n_ps' = (WordMap.add'
                       (fn _ => false)
                       (i, l, get_ports plink n))
                     handle WordMap.DATACHANGED =>
                            raise Fail "FIXME port (n,i) already \
                                       \has a connection"
      in
      ((V, E, ctrl, prnt, prnt_inv, nlink,
        NodeMap.add (n, n_ps', plink),
        add_point l p link_inv,
        sites, roots, innerns, outerns)
       handle NameMap.DATACHANGED =>
              raise Fail "FIXME inner name n already has a connection"
            | NameSet.DuplicatesRemoved =>
              raise Fail "FIXME inner name n is already in the graph")
      end
    | insert_aspect ((aspect, value), _) =
      raise Fail "FIXME invalid value for aspect"

  (* Verify that n is a tree by removing it and nodes reachable nodes
   * from the given set of nodes.
   * Also verify that all ports are connected to a link,
   * and that nodes match their signature (i.e. they have the right
   * ports).
   * Raises NotTree if a node is reachable from more than one path.
   * NB: this is a slow implementation as we implement marking of
   *     nodes by removing them from a set...
   *
   * FIXME make sure that nodes are assigned a control *)
  fun verify_tree ctrl prnt_inv plink (CSite c) V = V
    | verify_tree ctrl prnt_inv plink (c as (CNode n)) V =
    let
      (* verify that the node has been assigned a control *)
      val c = case NodeMap.lookup ctrl n of
                NONE => raise Fail "FIXME node n has not been \
                                   \assigned a control"
              | SOME c => c
      (* 'mark' the node *)
      val V' = (NodeSet.remove n V) handle NodeSet.NotFound =>
                                           raise NotTree n
      val num_ports = (Word.fromInt o Control.free) c
      val n_ps = get_ports plink n
      val n_ps_indices = WordMap.dom n_ps
      val n_ps_indices_ranges = foldr insert_word [] n_ps_indices
    in
      (* verify that all n's ports are in plink *)
      case n_ps_indices_ranges of
        [] =>
        if 0w0 = num_ports then
          verify_trees ctrl prnt_inv plink
                       (get_children prnt_inv (PNode n)) V'
        else
          raise Fail "FIXME ports [0 ; num_ports-1] \
                     \of node n are not connected"
      | [(0w0, max_port)] =>
        if max_port + 0w1 = num_ports then
          verify_trees ctrl prnt_inv plink
                       (get_children prnt_inv (PNode n)) V'
        else if max_port < num_ports then
          raise Fail "FIXME ports [max_port ; num_ports-1] \
                     \of node n are not connected"
        else
          raise Fail "FIXME node n doesn't have ports \
                     \[num_ports ; max_port]"
      | n_p_indices_ranges =>
        raise Fail "FIXME all ports [0 ; num_ports - 1] (and only \
                   \ those) of node n should be connected"
    end
  and verify_trees ctrl prnt_inv plink ps V =
    ChildSet.fold (verify_tree ctrl prnt_inv plink) V ps
  fun verify_roots ctrl prnt_inv plink 0w0 V = V
    | verify_roots ctrl prnt_inv plink i V =
      let
        val r  = i - 0w1
        val V' = verify_trees
                   ctrl prnt_inv plink
                   (get_children prnt_inv (PRoot r)) V
      in
        verify_roots ctrl prnt_inv plink r V'
      end
in
  fun make' aspect_values =
    let
      val (V, E, ctrl, prnt, prnt_inv, nlink, plink,
           link_inv, sites, roots, innerns, outerns)
        = foldl insert_aspect
                (NodeSet.empty, EdgeSet.empty, NodeMap.empty,
                 ChildMap.empty, PlaceMap.empty, NameMap.empty,
                 NodeMap.empty, LinkMap.empty, [], [],
                 NameSet.empty, NameSet.empty)
                aspect_values
      (* Verify that site and root numbers are consistent *)
      val innerwidth = case sites of
                         [] => 0
                       | [(0w0,w)] => Word.toInt w + 1
                       | _ => raise Fail
                                "FIXME site numbers are inconsistent"
      val outerwidth = case roots of
                         [] => 0
                       | [(0w0,w)] => Word.toInt w + 1
                       | _ => raise Fail
                                "FIXME root numbers are inconsistent"
    in
      (* Verify that the prnt map is a forest and
       * that the link map is total:
       * - all ports must in the map
       * - all inner names must be in the map
       *   (must already be the case) *)
      if (outerwidth = 0 andalso NodeSet.isEmpty V) orelse
        (outerwidth > 0
         andalso NodeSet.isEmpty
                   (verify_roots ctrl prnt_inv plink
                                 (Word.fromInt outerwidth) V))
      then
        {V = V, E = E, ctrl = ctrl, prnt = prnt, prnt_inv = prnt_inv,
         nlink = nlink, plink = plink, link_inv = link_inv,
         innerface = Interface.make' {width = innerwidth,
                                      glob = innerns},
         outerface = Interface.make' {width = outerwidth,
                                      glob = outerns}}
      else
        raise Fail "FIXME some nodes are not reachable from a root"
    end
end

(* Deconstruct a concrete bigraph into its aspects. *)
fun unmk' {V, E, ctrl, prnt, prnt_inv,
           nlink, plink, link_inv, innerface, outerface} =
  let
    fun roots2aspects acc ~1 = acc
      | roots2aspects acc r =
      let
        val r' = Word.fromInt r
      in
        roots2aspects
          ((Presence (ERoot r'),
            Present
              ((Word.fromInt o ChildSet.size o (get_children prnt_inv))
                 (PRoot r')))
           :: acc)
          (r - 1)
      end
    fun name2aspect n acc =
      (Presence (EName n),
       Present
         ((Word.fromInt o PointSet.size o (get_points link_inv))
            (LName n)))
      :: acc
    fun edge2aspect e acc =
      (Presence (EEdge e),
       Present
         ((Word.fromInt o PointSet.size o (get_points link_inv))
            (LEdge e)))
      :: acc
    fun node2aspects v acc =
      (Presence (ENode v),
       Present
         ((Word.fromInt o ChildSet.size o (get_children prnt_inv))
            (PNode v)))
      :: (NodeControl v, Control (get_ctrl ctrl v)) :: acc
    fun prnt2aspect ((c, p), acc) =
      (ChildParent c, Place p) :: acc
    fun nlink2aspect ((n, l), acc) =
      (PointLink (PName n), Link l) :: acc
    fun portlink2aspect n ((p_index, l), acc) =
      (PointLink (PPort (n, p_index)), Link l) :: acc
    fun plink2aspects ((n, n_ps), acc) =
      WordMap.Fold (portlink2aspect n) acc n_ps
  in
     roots2aspects
    (NameSet.fold name2aspect
    (EdgeSet.fold edge2aspect
    (NodeSet.fold node2aspects
    (ChildMap.Fold prnt2aspect
    (NameMap.Fold nlink2aspect
    (NodeMap.Fold plink2aspects
     []
     plink) nlink) prnt) V) E)
     (Interface.glob outerface))
     (Interface.width outerface - 1)
end

local
  fun add_aspect ((aspect, value), map) =
    AspectMap.add (aspect, value, map)
in
  fun unmk'' conbg = foldr add_aspect AspectMap.empty (unmk' conbg)
end

(* Construct a term representation of the bigraph *)
fun abstract {V, E, ctrl, prnt, prnt_inv,
              nlink, plink, link_inv, innerface, outerface} =
  let
    val i = Info.noinfo
    val Wir = BgVal.Wir i
    val Ion = BgVal.Ion' i
    val Per = BgVal.Per i
    val Ten = BgVal.Ten i
    val Par = BgVal.Par i
    val Pri = BgVal.Pri i
    fun ** (v1, v2)  = Ten [v1, v2]  infix 5 **
    fun || (v1, v2)  = Par [v1, v2]  infix 5 ||
    fun `|` (v1, v2) = Pri [v1, v2]  infix 6 `|`
    val oo = BgVal.Com' i            infix 7 oo

    val id_1 = Per (Permutation.id_n 1)

    fun child2term (CSite s) (site_perm_inv, acc) =
      ((Word.toInt s, NameSet.empty) :: site_perm_inv, id_1::acc)
      | child2term (CNode n) (site_perm_inv, acc) =
      let
        val free = map (link2name o #2)
                       (ListSort.sort
                          (fn ((i,_),(j,_)) => Word.compare (i,j))
                          (WordMap.list (get_ports plink n)))
        val ion_n = Ion.make {ctrl = get_ctrl ctrl n,
                              free = free,
                              bound = []}
        val (site_perm_inv', children)
          = children2term site_perm_inv
                          (get_children prnt_inv (PNode n))
      in
        (site_perm_inv', (Ion ion_n oo children)::acc)
      end

    and children2term site_perm_inv cs =
      let
        val (site_perm_inv', cs_terms)
          = ChildSet.fold child2term (site_perm_inv, []) cs
      in
        (site_perm_inv', Pri cs_terms)
      end

    fun roots2terms 0w0 site_perm_inv terms =
      (site_perm_inv, terms)
      | roots2terms i site_perm_inv terms =
      let
        val r = i - 0w1
        val (site_perm_inv', term)
          = children2term site_perm_inv
                          (get_children prnt_inv (PRoot r))
      in
        roots2terms r site_perm_inv' (term :: terms)
      end

    val (site_perm_inv, terms)
      = roots2terms (Word.fromInt (Interface.width outerface))
                    [] []
    (* inner name links
     * inner names -> outer names * inner names -> edges *)
    val (ios, ies)
      = foldr (fn ((n, LName n'), (ios, ies)) =>
                  (Link.make {inner = NameSet.singleton n,
                              outer = SOME n'} :: ios,
                   ies)
                | ((n, LEdge e), (ios, ies)) =>
                  (ios, Link.make {inner = NameSet.singleton n,
                                   outer = SOME e} :: ies))
              ([],[]) (NameMap.list nlink)

    val edges_open =    Wir (Wiring.make' ies)
                     || (Par terms)
                        oo (Per (Permutation.invert
                                   (Permutation.make site_perm_inv)))
  in
    BgVal.simplify
      (   Wir (Wiring.make_intro (Interface.glob outerface))
       ** Wir (Wiring.make' ios)
              (* NB: E might contain idle edges. *)
       || Wir (Wiring.close
                 (NameSet.intersect
                    E (Interface.glob (BgVal.outerface edges_open))))
          oo edges_open)
  end


(* Create an concrete bigraph instance from an abstract bigraph.
 * 1. normalize b :
 *      (X * (/Y_1 * ... * /Y_m) * (z_1/V_1 * ... * z_n/V_n) * id_k)
 *    o (alpha * P_1 * ... * P_k) o pi
 * 2. X is ignored as its implicit in the interface
 * 3. choose edge names e_i for each Y_i
 * 4. make nlink from  (es/Ys * zs/Vs)(alpha)
 * 5. FIXME
 *)
fun concretize b =
  let
    val innerface = BgVal.innerface b
    val outerface = BgVal.outerface b

    val norm_b = BgBDNF.make b
    val {wir, D, ...} = BgBDNF.unmkB' norm_b
    val {ren = alpha, Ps, perm = pi} = BgBDNF.unmkD' D
    val Ps_outerns
      = foldr (fn (P, ns) =>
                  NameSet.union
                    ((Interface.glob o BgBDNF.outerface) P) ns)
              NameSet.empty Ps

    val {intro = _, closures = Ys, function = zVs}
      = Wiring.partition wir
    val {opened = eYs, newnames = E} = Wiring.openup Ys
    val eYsxzVs = Wiring.* (eYs, zVs)

    (* links of (zVs * eYs)(alpha) to nlink *)
    val nlink
      = NameMap.composemap
          (fn n => if NameSet.member n E then LEdge n else LName n)
          (Wiring.unmk_ren
             (Wiring.o
                ((Wiring.restrict eYsxzVs (Wiring.outernames alpha)),
                 alpha)))
    val link_inv
      = NameMap.Fold
          (fn ((n, l), link_inv) => add_point l (PName n) link_inv)
          LinkMap.empty nlink

    (* Construct a map from the outer names of the place graph
     * to links *)
    val name2link_map
      = NameMap.composemap
          (fn n => if NameSet.member n E then LEdge n else LName n)
          (Wiring.unmk_sub
             (Wiring.restrict eYsxzVs Ps_outerns))
    fun name2link n = valOf (NameMap.lookup name2link_map n)

    fun concS (S, (p, s, V, ctrl, prnt, prnt_inv, plink, link_inv)) =
      case BgBDNF.unmkS' S of
        BgBDNF.SCon' _ =>
        (p, s - 0w1, V, ctrl,
         add_prnt p
           (CSite (Word.fromInt
                     (Permutation.invmap pi (Word.toInt s))))
           prnt,
         add_child p (CSite s) prnt_inv,
         plink, link_inv)
      | BgBDNF.SMol' M =>
        let
          val {KyX, N, ...} = BgBDNF.unmkM M
          val {ctrl = K, free = y, ...} = Ion.unmk KyX
          val v = Node.fresh NONE
          val (_, port_link_map, link_inv')
            = foldl (fn (n, (p, pl_map, link_inv)) =>
                        let
                          val l = name2link n
                        in
                          (p + 0w1,
                           WordMap.add (p, l, pl_map),
                           add_point l (PPort (v, p)) link_inv)
                        end)
                    (0w0, WordMap.empty, link_inv) y

          val (_, s', V', ctrl', prnt', prnt_inv', plink', link_inv'')
            = concN (N, (PNode v, s, V, ctrl, prnt, prnt_inv,
                         plink, link_inv'))
        in
          (p, s',
           NodeSet.insert v V',
           NodeMap.add (v, K, ctrl'),
           add_prnt p (CNode v) prnt',
           add_child p (CNode v) prnt_inv',
           NodeMap.add (v, port_link_map, plink'),
           link_inv'')
        end
    and concN (N, (p, s, V, ctrl, prnt, prnt_inv, plink, link_inv)) =
      foldr concS (p, s, V, ctrl, prnt, prnt_inv, plink, link_inv)
            ((#Ss o BgBDNF.unmkG o #G o BgBDNF.unmkN) N)
    fun concP (P, (r, (_, s, V, ctrl, prnt, prnt_inv,
                       plink, link_inv))) =
        (r - 0w1,
         concN ((#N o BgBDNF.unmkP') P,
                (PRoot r, s, V, ctrl, prnt, prnt_inv,
                 plink, link_inv)))

    val (_, (_, _, V, ctrl, prnt, prnt_inv, plink, link_inv'))
      = foldr concP (Word.fromInt (Interface.width outerface - 1),
                     (PRoot 0w0 (*dummy*),
                      Word.fromInt (Interface.width innerface - 1),
                      NodeSet.empty, NodeMap.empty, ChildMap.empty,
                      PlaceMap.empty, NodeMap.empty, link_inv)) Ps
  in
    {V = V,
     E = E,
     ctrl = ctrl,
     prnt = prnt,
     prnt_inv = prnt_inv,
     nlink = nlink,
     plink = plink,
     link_inv = link_inv',
     innerface = innerface,
     outerface = outerface}
  end


(* Apply a list of changes to a concrete bigraph.
 * NB!
 * - Should the ordering matter?
 * - What should happen if a node with children is deleted?
 *   (currently we move these children to the parent...)
 *   We could require that the node has no children...
 * - How should we handle deletion and copying of sites/parameters?
 *   
 *)
local
      (* Delete parameter s.
       * This must result in a renumbering.
       * The order of changes can make a difference!
       *)
  fun app' (Del (c as (CSite s)),
            {V, E, ctrl, prnt, prnt_inv,
             nlink, plink, link_inv, innerface, outerface}) =
raise Fail "FIXME not implemented"
(*          (let *)
(*            val p = valOf (ChildMap.lookup prnt c) *)
(*          in *)
(*            (\* FIXME renumber sites! *\) *)
(*            {V, E, ctrl, *)
(*             ChildMap.remove (c, prnt), *)
(*             remove_child prnt_inv p c, *)
(*             nlink, plink, link_inv, innerface, outerface} *)
(*          end *)
(*          handle Option => raise Fail "FIXME graph does not have site s") *)
      (* Delete a node and its ports.
       * Its children will be moved to the parent. *)
    | app' (Del (c as (CNode n)),
            {V, E, ctrl, prnt, prnt_inv,
             nlink, plink, link_inv, innerface, outerface}) =
      (let
         val p = valOf (ChildMap.lookup prnt c)
         val children = get_children prnt_inv (PNode n)
         val n_ps = get_ports plink n
         val link_inv'
           = WordMap.Fold
               (fn ((p, l), link_inv) =>
                   remove_point link_inv l (PPort (n, p)))
               link_inv n_ps
       in
         {V = NodeSet.remove n V,
          E = E,
          ctrl = remove_ctrl ctrl n,
          prnt = ChildSet.fold
                   (add_prnt p)
                   (valOf (ChildMap.remove (c, prnt)))
                   children,
          prnt_inv = ChildSet.fold
                       (add_child p)
                       (remove_child prnt_inv p c)
                       children,
          nlink = nlink,
          plink = valOf (NodeMap.remove (n, plink)),
          link_inv = link_inv',
          innerface = innerface, outerface = outerface}
       end
       handle Option => raise Fail "FIXME graph does not have node n")
    | app' (Mov (c as (CSite s), p),
            {V, E, ctrl, prnt, prnt_inv,
             nlink, plink, link_inv, innerface, outerface}) =
      (* FIXME raise relevant exceptions*)
      {V = V, E = E, ctrl = ctrl,
       prnt = add_prnt p c prnt,
       prnt_inv = add_child
                    p c
                    (remove_child prnt_inv
                                  (valOf (ChildMap.lookup prnt c))
                                  c),
       nlink = nlink, plink = plink, link_inv = link_inv,
       innerface = innerface, outerface = outerface}
    | app' (Mov (c as (CNode n), p),
            {V, E, ctrl, prnt, prnt_inv,
             nlink, plink, link_inv, innerface, outerface}) =
      (* FIXME raise relevant exceptions*)
      {V = V, E = E, ctrl = ctrl,
       prnt = add_prnt p c prnt,
       prnt_inv = add_child
                    p c
                    (remove_child
                       prnt_inv (valOf (ChildMap.lookup prnt c)) c),
       nlink = nlink, plink = plink, link_inv = link_inv,
       innerface = innerface, outerface = outerface}
      (* Copy parameter s to parent p.
       * This means adding a site and increasing the inner width.
       * NB: FIXME *)
    | app' (Cop (s, p),
            {V, E, ctrl, prnt, prnt_inv,
             nlink, plink, link_inv, innerface, outerface}) =
raise Fail "FIXME not implemented"
(*         let *)
          
(*         in *)
(*           {V, E, ctrl, prnt, prnt_inv, *)
(*            nlink, plink, link_inv, innerface, outerface} *)
(*         end *)
      (* Add node n_ls : c as a child of p. *)
    | app' (Add (n, c, ls, p),
            {V, E, ctrl, prnt, prnt_inv,
             nlink, plink, link_inv, innerface, outerface}) =
      (* FIXME raise relevant exceptions *)
      let
        val child = CNode n
        val outerns = Interface.glob outerface
        val (_, n_ps, link_inv')
          = foldl
              (fn (l, (i, n_ps, link_inv)) =>
                  ( (case l of
                       LName n => if NameSet.member n outerns then
                                    ()
                                  else
                                    raise 
                                      Fail "FIXME name n is not \
                                           \in the outerface of \
                                           \the graph"
                     | LEdge e => if EdgeSet.member e E then
                                    ()
                                  else
                                    raise 
                                      Fail "FIXME edge e is not \
                                           \in the graph")
                  ; (i + 0w1, WordMap.add (i, l, n_ps),
                     add_point l (PPort (n, i)) link_inv)))
              (0w0, WordMap.empty, link_inv) ls
      in
        {V = NodeSet.insert n V,
         E = E,
         ctrl = add_ctrl c n ctrl,
         prnt = add_prnt p child prnt,
         prnt_inv = add_child p child prnt_inv,
         nlink = nlink,
         plink = add_ports n_ps n plink,
         link_inv = link_inv',
         innerface = innerface, outerface = outerface}
      end
      (* Connect port (n,i) to link l.
       * FIXME should we remove idle edges? *)
    | app' (Con (p as (n, i), l),
            {V, E, ctrl, prnt, prnt_inv,
             nlink, plink, link_inv, innerface, outerface}) =
      let
        val outerns = Interface.glob outerface
        val port = PPort p
        val l' = get_port p plink
        val link_inv' = remove_point link_inv l' port
      in
        if (case l of LName n => NameSet.member n outerns
                    | LEdge e => EdgeSet.member e E) then
          {V = V, E = E, ctrl = ctrl,
           prnt = prnt, prnt_inv = prnt_inv, nlink = nlink,
           plink = add_port l p plink,
           link_inv = add_point l port link_inv',
           innerface = innerface, outerface = outerface}
        else
          raise Fail "FIXME link l is not in the graph"
      end
in
  fun app cs conbg = foldl app' conbg cs
end
   
(* Naive algorithm for finding an optimal editing script.
 * For forests of unordered trees the algorithm is as follows
 * (where e is the empty forest): 
 * 
 * ES(e, e)  = (0, [])
 * ES(F1 || T, e) = (
 *)
fun edit_script {V = V1, E = E1, ctrl = ctrl1,
                 prnt = prnt1, prnt_inv = prnt_inv1,
                 nlink = nlink1, plink = plink1, link_inv = link_inv1,
                 innerface = innerface1, outerface = outerface1}
                {V = V2, E = E2, ctrl = ctrl2,
                 prnt = prnt2, prnt_inv = prnt_inv2,
                 nlink = nlink2, plink = plink2, link_inv = link_inv2,
                 innerface = innerface2, outerface = outerface2} =
  raise Fail "FIXME not implemented"
end
