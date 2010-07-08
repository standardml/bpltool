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

(** Abstract data type for modelling and generating bigraph aspect state
 * spaces.
 * @version $LastChangedRevision: 2717 $
 *)
functor BgAspectsSSGen (
  structure WordSet : MONO_SET
    where type elt = word
  structure WordMap : MONO_FINMAP
    where type dom = word
  structure Name : NAME
  structure NameSet : MONO_SET
  structure NameMap : MONO_FINMAP
  structure Control : CONTROL
  structure ControlSet : MONO_SET
  structure ControlMap : MONO_FINMAP
  structure ControlSetMap : MONO_FINMAP
  structure Node : NODE
  structure NodeSet : MONO_SET
  structure NodeSubset : SUBSET
  structure NodeMap : MONO_FINMAP
  structure Edge : EDGE
  structure EdgeSet : MONO_SET
  structure EdgeSubset : SUBSET
  structure EdgeMap : MONO_FINMAP
  structure BgVal : BGVAL
  structure BgAspects : BGASPECTS
    where type root = word
  structure ConcreteBigraph : CONCRETE_BIGRAPH
    where type translation = {rho_V : Node.node NodeMap.map,
                              rho_E : Name.name EdgeMap.map}
  structure PCRule : PCRULE
    where type preconds    = (BgAspects.aspect * BgAspects.value) list
      and type changes     = BgAspects.change list
      and type linkset     = BgAspects.LinkSet.Set
      and type translation = {rho_V : Node.node NodeMap.map,
                              rho_E : Name.name EdgeMap.map}
      and type instantiation = {rho_V : Node.node NodeMap.map,
                                rho_E : Name.name EdgeMap.map,
                                map_Y : BgAspects.link NameMap.map,
                                map_R : BgAspects.place WordMap.map}
  structure PCRuleMap : MONO_FINMAP
    where type dom = PCRule.pcrule
  type instantiation = {rho_V : Node.node NodeMap.map,
                        rho_E : Name.name EdgeMap.map,
                        map_Y : BgAspects.link NameMap.map,
                        map_R : BgAspects.place WordMap.map}
  structure InstantiationMap : MONO_FINMAP
    where type dom = instantiation
  structure ActionSet : MONO_SET
    where type elt = PCRule.pcrule * instantiation
  sharing type Name.name =
               NameSet.elt =
               NameMap.dom =
               BgVal.name =
               BgAspects.name
=(*  sharing type Edge.edge =*)
               EdgeSet.elt =
               EdgeMap.dom =
               BgAspects.edge
  sharing type NameSet.Set =
               ConcreteBigraph.nameset
  sharing type Control.control =
               ControlSet.elt =
               ControlMap.dom =
               BgAspects.control
  sharing type ControlSet.Set =
               ControlSetMap.dom =
               ConcreteBigraph.controlset =
               PCRule.controlset
  sharing type ControlMap.map =
               ConcreteBigraph.controlmap
  sharing type Node.node =
               NodeSet.elt =
               NodeMap.dom =
               BgAspects.node =
               ConcreteBigraph.node
  sharing type NodeSet.Set =
               NodeSubset.set =
               ConcreteBigraph.nodeset =
               PCRule.nodeset
  sharing type EdgeSet.Set =
               EdgeSubset.set =
               ConcreteBigraph.edgeset =
               PCRule.edgeset
  sharing type BgAspects.EntityMap.map =
               PCRule.entitymap
  sharing type BgAspects.aspect =
               ConcreteBigraph.aspect
  sharing type BgAspects.value =
               ConcreteBigraph.value =
               PCRule.value
  sharing type BgAspects.AspectMap.map =
               ConcreteBigraph.aspectmap =
               PCRule.aspectmap
  sharing type ConcreteBigraph.conbg =
               PCRule.conbg
  sharing type BgVal.bgval =
               ConcreteBigraph.absbg
) : BGASPECTSSSGEN
  where type absbg       = BgVal.bgval
    and type pcrule      = PCRule.pcrule
    and type state       = BgAspects.value BgAspects.AspectMap.map
    and type transitions = (((BgAspects.value InstantiationMap.map)
                                              PCRuleMap.map)
                                              BgAspects.ValueMap.map)
                                              BgAspects.AspectMap.map =
struct

type absbg  = BgVal.bgval
type pcrule = PCRule.pcrule

open BgAspects

(* FIXME debug printing *)
val out = ref (TextIO.stdOut)
fun print' s = TextIO.output (!out, s)

(* A state space is an initial state and a map representing transitions:
 *    aspect -> value -> rule -> instantiation -> value *)
type state = value AspectMap.map
type transitions = 
  (((value InstantiationMap.map)
           PCRuleMap.map)
           ValueMap.map)
           AspectMap.map
type instantiation = {rho_V : Node.node NodeMap.map,
                      rho_E : Name.name EdgeMap.map,
                      map_Y : link NameMap.map,
                      map_R : place WordMap.map}

fun list2string lb rb sep elt2string list =
  lb ^ (#1 (foldl
              (fn (e, (s, first)) =>
                  ((if first then s else s ^ sep)
                   ^ (elt2string e), false))
              ("", true) list)) ^ rb

fun set2string lb rb sep
               (fold : ('elt -> (string * bool)
                        -> (string * bool))
                       -> (string * bool) -> 'set -> (string * bool))
               elt2string set =
  lb ^ (#1 (fold
              (fn e => fn (s, first) =>
                  ((if first then s else s ^ sep)
                   ^ (elt2string e), false))
              ("", true) set)) ^ rb

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

(*FIXME we only print rule names *)
fun rule2string rule = PCRule.name rule
fun rule2pepa rule = "r__"
                     ^ String.translate (fn #" " => "_"
                                          | c    => Char.toString c)
                                        (PCRule.name rule)

fun instantiation2string' lb rb sep {rho_V, rho_E, map_Y, map_R} =
  lb    ^ "rho_V = " ^ map2string' "[" "]" ", " "" "" " |-> "
                                   NodeMap.Fold
                                   Node.unmk Node.unmk rho_V
  ^ sep ^ "rho_E = " ^ map2string' "[" "]" ", " "" "" " |-> "
                                   EdgeMap.Fold
                                   Name.unmk Name.unmk rho_E
  ^ sep ^ "map_Y = " ^ map2string' "[" "]" ", " "" "" " |-> "
                                   NameMap.Fold
                                   Name.unmk link2string map_Y
  ^ sep ^ "map_R = " ^ map2string' "[" "]" ", " "" "" " |-> "
                                   WordMap.Fold
                                   Word.toString place2string map_R
  ^ rb
val instantiation2string = instantiation2string' "{" "}" ", "

fun instantiation2pepa {rho_V, rho_E, map_Y, map_R} =
      "rho_V__" ^ map2string' "" "" "__" "" "" "__" NodeMap.Fold
                              Node.unmk Node.unmk rho_V
  ^ "__rho_E__" ^ map2string' "" "" "__" "" "" "__" EdgeMap.Fold
                              Name.unmk Name.unmk rho_E
  ^ "__map_Y__" ^ map2string' "" "" "__" "" "" "__" NameMap.Fold
                              Name.unmk link2pepa map_Y
  ^ "__map_R__" ^ map2string' "" "" "__" "" "" "__" WordMap.Fold
                              Word.toString place2pepa map_R

fun action2pepa (rule, instantiation) =
    (rule2pepa rule) ^ "__" ^ (instantiation2pepa instantiation)

fun action2pepa' inst_ids (rule, instantiation) =
    (rule2pepa rule) ^ "__inst__"
    ^ (Int.toString
         (valOf (InstantiationMap.lookup
                   inst_ids instantiation)))
val state2string =
  map2string' "[" "]" ", " "(" ")" ", " AspectMap.Fold
              aspect2string value2string

val state2pepa =
  map2string' "" "" " <*>\n" "" "" "__" AspectMap.Fold
              aspect2pepa value2pepa
fun state2pepa' inst_ids aspect_actions state =
  #1 (AspectMap.Fold
        (fn ((aspect, value), (s, first, acts)) =>
            let
              val aspect_acts
                = case AspectMap.lookup aspect_actions aspect of
                    NONE => ActionSet.empty
                  | SOME acts => acts
            in
              ((if first then
                  "\n"
                else
                  "(" ^ s ^ ")"
                  ^ (set2string
                       "\n <" ">\n" ",\n  " ActionSet.fold
                       (action2pepa' inst_ids)
                       (ActionSet.intersect acts aspect_acts)))
               ^ (aspect2pepa aspect)
               ^ "__" ^ (value2pepa value),
               false,
               ActionSet.union' acts aspect_acts)
            end)
        ("", true, ActionSet.empty) state)

(*   val imap2string *)
(*     = map2string' "[" "]" ", " "" "" " -> " InstantiationMap.Fold *)
(*                   instantiation2string value2string *)
(*   val rmap2string *)
(*     = map2string' "[" "]" ", " "" "" " -> " PCRuleMap.Fold *)
(*                   rule2string imap2string *)
(*   val vmap2string *)
(*     = map2string' "[" "]" ", " "" "" " -> " ValueMap.Fold *)
(*                   value2string rmap2string *)
(*   val trns2string *)
(*     = map2string' "[" "]" ", " "" "" " -> " AspectMap.Fold *)
(*                   aspect2string vmap2string *)
val imap2string
  = map2string' "\n\t\t\t[" "]" ",\n\t\t\t " "" "" " -> "
                InstantiationMap.Fold
                (instantiation2string' "{" "}" ",\n\t\t\t  ")
                value2string
fun imap2string' inst_ids
  = map2string' "\n\t\t\t[" "]" ",\n\t\t\t " "" "" " -> "
                InstantiationMap.Fold
                (fn inst =>
                    "inst "
                    ^ (Int.toString
                         (valOf (InstantiationMap.lookup
                                   inst_ids inst))))
                value2string
val rmap2string
  = map2string' "\n\t\t[" "]" ",\n\t\t " "" "" " -> " PCRuleMap.Fold
                rule2string imap2string
fun rmap2string' inst_ids
  = map2string' "\n\t\t[" "]" ",\n\t\t " "" "" " -> " PCRuleMap.Fold
                rule2string (imap2string' inst_ids)
val vmap2string
  = map2string' "\n\t[" "]" ",\n\t " "" "" " -> " ValueMap.Fold
                value2string rmap2string
fun vmap2string' inst_ids
  = map2string' "\n\t[" "]" ",\n\t " "" "" " -> " ValueMap.Fold
                value2string (rmap2string' inst_ids)
val trns2string
  = map2string' "[" "]" ",\n " "" "" " -> " AspectMap.Fold
                aspect2string vmap2string
fun trns2string' inst_ids
  = map2string' "[" "]" ",\n " "" "" " -> " AspectMap.Fold
                aspect2string (vmap2string' inst_ids)
fun inst_ids2string inst_ids =
  let
    val inst_ids_list = InstantiationMap.list inst_ids
    val inst_ids_sorted
      = ListSort.sort
          (fn ((inst1, id1), (inst2, id2)) => 
              Int.compare (id1, id2))
          inst_ids_list
  in
    #1 (foldl (fn ((inst, id), (s, first)) =>
                  ((if first then "" else s ^ "\n")
                   ^ (Int.toString id) ^ " = "
                   ^ (instantiation2string' "{" "}" ",\n     " inst),
                   false))
              ("", true) inst_ids_sorted)
  end


fun imap2pepa aspect_str rule_str
  = map2string "" "" "\n + " InstantiationMap.Fold
               (fn (instantiation, value) =>
                   "(" ^ rule_str ^ "__"
                   ^ (instantiation2pepa instantiation) ^ ", 1)."
                   ^ aspect_str ^ "__" ^ (value2pepa value))
fun imap2pepa' inst_ids aspect_str rule_str
  = map2string "" "" "\n + " InstantiationMap.Fold
               (fn (instantiation, value) =>
                   "(" ^ rule_str ^ "__inst__"
                   ^ (Int.toString
                        (valOf (InstantiationMap.lookup
                                  inst_ids instantiation)))
                   ^ ", 1)."
                   ^ aspect_str ^ "__" ^ (value2pepa value))
fun rmap2pepa aspect_str
  = map2string "" "" "\n + " PCRuleMap.Fold
               (fn (rule, imap) =>
                   imap2pepa aspect_str (rule2pepa rule) imap)
fun rmap2pepa' inst_ids aspect_str
  = map2string "" "" "\n + " PCRuleMap.Fold
               (fn (rule, imap) =>
                   imap2pepa' inst_ids aspect_str (rule2pepa rule) imap)
fun vmap2pepa aspect_str
  = map2string "" ";\n" ";\n" ValueMap.Fold
               (fn (value, rmap) =>
                   aspect_str ^ "__" ^ (value2pepa value) ^ " =\n   "
                   ^ (if PCRuleMap.isEmpty rmap then
                        "(dead__" ^ aspect_str ^ ", 1)."
                        ^ aspect_str ^ "__" ^ (value2pepa value)
                        ^ "; // final state"
                      else
                        rmap2pepa aspect_str rmap))
fun vmap2pepa' inst_ids aspect_str
  = map2string "" ";\n" ";\n" ValueMap.Fold
               (fn (value, rmap) =>
                   aspect_str ^ "__" ^ (value2pepa value) ^ " =\n   "
                   ^ (if PCRuleMap.isEmpty rmap then
                        "(dead__" ^ aspect_str ^ ", 1)."
                        ^ aspect_str ^ "__" ^ (value2pepa value)
                        ^ "; // final state"
                      else
                        rmap2pepa' inst_ids aspect_str rmap))
val trns2pepa
  = map2string "" "" "\n" AspectMap.Fold
               (fn (aspect, vmap) =>
                   vmap2pepa (aspect2pepa aspect) vmap)
fun trns2pepa' inst_ids
  = map2string "" "" "\n" AspectMap.Fold
               (fn (aspect, vmap) =>
                   vmap2pepa' inst_ids (aspect2pepa aspect) vmap)

fun inst_ids2pepa inst_ids =
  let
    val inst_ids_list = InstantiationMap.list inst_ids
    val inst_ids_sorted
      = ListSort.sort
          (fn ((inst1, id1), (inst2, id2)) => 
              Int.compare (id1, id2))
          inst_ids_list
  in
    #1 (foldl (fn ((inst, id), (s, first)) =>
                  ((if first then "" else s ^ "\n")
                   ^ "// inst " ^ (Int.toString id) ^ " = "
                   ^ (instantiation2string' "{" "}" ",\n//           " inst),
                   false))
              ("", true) inst_ids_sorted)
  end
               

(* utility functions for working with transition maps *)
fun get_aspect_trns aspect trns =
  case AspectMap.lookup trns aspect of
    SOME aspect_trns => aspect_trns
  | NONE => ValueMap.singleton (Absent, PCRuleMap.empty)
(* ( print' (((Int.toString o length o AspectMap.dom) trns) ^ "\n") *)
(* (\* ; map (fn a => print' (aspect2string a ^ "\n")) (AspectMap.dom trns) *\) *)
(* ; print' (trns2string trns ^ "\n") *)
(* ; print' ("\n" ^ (aspect2string aspect) ^ "\n") *)
(* ; raise Fail "FIXME we should never lookup aspects that are \ *)
(*                          \ not in a transition" *)
(* ) *)

fun get_value_trns value aspect_trns =
  case ValueMap.lookup aspect_trns value of
    SOME value_trns => value_trns
  | NONE => PCRuleMap.empty

fun get_rule_trns rule value_trns =
  case PCRuleMap.lookup value_trns rule of
    SOME rule_trns => rule_trns
  | NONE => InstantiationMap.empty

fun add_trns trns aspect value rule instantiation value' =
  let
    val aspect_trns  = get_aspect_trns aspect trns
    val value_trns   = get_value_trns value aspect_trns
    val rule_trns    = get_rule_trns rule value_trns
    (* If value' is new for aspect, add it explicitly to the map. *)
    val aspect_trns'
      = if ValueMap.inDomain value' aspect_trns then
          aspect_trns
        else
          ValueMap.add (value', PCRuleMap.empty, aspect_trns)
  in
    AspectMap.add        (aspect,
    ValueMap.add         (value,
    PCRuleMap.add        (rule,
    InstantiationMap.add (instantiation,
                          value',
                          rule_trns), value_trns), aspect_trns'), trns)
  end

fun add_aspect_act aspect_acts aspect act =
  let
    val acts
      = case AspectMap.lookup aspect_acts aspect of
          NONE      => ActionSet.singleton act
        | SOME acts => ActionSet.insert act acts
  in
    AspectMap.add (aspect, acts, aspect_acts)
  end

fun add_trns' (trns, aspect_acts)
              aspect value rule instantiation value' =
  (add_trns trns aspect value rule instantiation value',
   add_aspect_act aspect_acts aspect (rule, instantiation))

(* Create a state space with no transitions corresponding to a 
 * concrete bigraph. *)
fun init_state_space conbg idle_edges
                     redexes_have_edge redexes_have_siteless_node =
  let
    fun add_state ((aspect, value), trns) =
      let
        val values = ValueMap.singleton (value, PCRuleMap.empty)
      in
        AspectMap.add (aspect, values, trns)
      end
      
    val init_state = ConcreteBigraph.unmk'' conbg
    val init_state' = EdgeSet.fold
                        (fn e => fn init_state =>
                            AspectMap.add
                              (Presence (EEdge e),
                               Present 0w0,
                               init_state))
                        init_state idle_edges
    (* Set all point counts to zero if no redex has an edge *)
    val init_state''
      = AspectMap.ComposeMap
          (fn (Presence e, value) =>
              (case e of
                 EEdge _ => if redexes_have_edge then
                              value
                            else
                              Present 0w0
               | EName _ => if redexes_have_edge then
                              value
                            else
                              Present 0w0
               | ENode _ => if redexes_have_siteless_node then
                              value
                            else
                              Present 0w0
               | ERoot _ => Present 0w0)
            | (_, value) => value)
          init_state'
  in
    (init_state'',
     AspectMap.Fold add_state AspectMap.empty init_state'')
  end

structure NodeInjection = SetInjection(structure SSet    = NodeSet
                                       structure RSet    = NodeSet
                                       structure RSubset = NodeSubset
                                       structure Map     = NodeMap)
structure EdgeInjection = SetInjection(structure SSet    = EdgeSet
                                       structure RSet    = EdgeSet
                                       structure RSubset = EdgeSubset
                                       structure Map     = EdgeMap)

structure RootPlaceMaps = SetMap (structure SSet = WordSet
                                  structure RSet = PlaceSet
                                  structure Map  = WordMap)
structure NameLinkMaps = SetMap (structure SSet = NameSet
                                 structure RSet = LinkSet
                                 structure Map  = NameMap)

structure NameEdgeUnion = DisjointUnion (structure SSet = NameSet
                                         structure RSet = EdgeSet
                                         structure USet = LinkSet
                                         val inl = LName
                                         val inr = LEdge
                                         fun cas (f, _) (LName n) = f n
                                           | cas (_, f) (LEdge e) = f e)
structure RootNodeUnion = DisjointUnion (structure SSet = WordSet
                                         structure RSet = NodeSet
                                         structure USet = PlaceSet
                                         val inl = PRoot
                                         val inr = PNode
                                         fun cas (f, _) (PRoot r) = f r
                                           | cas (_, f) (PNode v) = f v)

fun instantiations rule V_a E_a Y_a R_a =
  let
    val {nodes = V_R, edges = E_R} = PCRule.support rule

    val R_R = WordSet.tabulate (PCRule.width rule) Word.fromInt
    val Y_R = ConcreteBigraph.outernames (PCRule.redex rule)

    val V_injs = NodeInjection.injections V_R V_a
    val E_injs = EdgeInjection.injections E_R E_a

    (* extend the injections of nodes/edges with mappings of the
     * roots/names of the rule *)
    fun add_map inj (map, acc) = (inj, map) :: acc
    fun add_maps maps_S disjoint_union_S
                 ((inj, inj_rng_C), acc) =
      foldr (add_map inj) acc (maps_S (disjoint_union_S inj_rng_C))

    val add_root_maps = add_maps (RootPlaceMaps.maps R_R)
                                 (RootNodeUnion.disjoint_union R_a)
    val add_name_maps = add_maps (NameLinkMaps.maps Y_R)
                                 (NameEdgeUnion.disjoint_union Y_a)
  in
    ListUtil.cartesian_product (foldr add_root_maps [] V_injs)
                               (foldr add_name_maps [] E_injs)
  end

(* Find all control-respecting instantiations.
 * FIXME formalize. *)
(* fun ctrl_resp_instantiations rule ctrl_rel_inv ctrl_inv_a *)
(*                              E_a Y_a R_a = *)
(*   let *)
(*     val redex = PCRule.redex rule *)
(*     val {nodes = V_R, edges = E_R} = ConcreteBigraph.support redex *)
(*     val ctrl_R = ConcreteBigraph.ctrl redex *)
(*     val ctrl_inv_R = ConcreteBigraph.ctrl_inv redex *)
(*     fun get_nodes K = case ControlMap.lookup ctrl_inv_R K of *)
(*                         NONE   => NodeSet.empty *)
(*                       | SOME V => V *)
(*     fun get_nodes' map class_K =  *)
(*         case ControlSetMap.lookup map class_K of *)
(*           NONE   => NodeSet.empty *)
(*         | SOME V => V *)

(*     val R_R = WordSet.tabulate (ConcreteBigraph.width redex) *)
(*                                        Word.fromInt *)
(*     val Y_R = ConcreteBigraph.outernames redex *)

(* (\*     val V_injs = NodeInjection.injections V_R V_a *\) *)
(*     local *)
(*       fun get_predecessor_classes class_K = *)
(*         case ControlSetMap.lookup FIXME class_K of *)
(*           NONE    => [] *)
(*         | SOME cs => cs *)

(*       fun FOO [] map V_injs = [(V_injs, map)] *)
(*         | FOO ((ctrl_rel_K, (class_K, class_K_nodes)) :: eq_classes) *)
(*               map V_injs = *)
(*         let *)
(*           (\* Find the redex nodes with a control in this equivalence *)
(*            * class *\) *)
(*           val V_R_class_K *)
(*             = ControlSet.fold *)
(*                 (fn K => fn V_R_class_K => *)
(*                     NodeSet.union V_R_class_K (get_nodes K)) *)
(*                 NodeSet.empty class_K *)

(*           (\* Add the agent nodes of [K] to the map *\) *)
(*           val map' = ControlSetMap.add (class_K, class_K_nodes, map) *)
(*           (\* The list of equivalence classes that can reach [K]. *)
(*            * Nodes in these classes are valid choices for the redex nodes *)
(*            * in class [K]. *\) *)
(*           val classes = class_K :: (get_predecessor_classes class_K) *)

(*           fun BAR [] V_R_class_K map inj = *)
(*               if NodeSet.isEmpty V_R_class_K then *)
(*                   FOO eq_classes map (ext_injs V_injs inj) *)
(*               else *)
(*                 FIXME some nodes haven't been injected... *)
(*                 [] *)
(*             | BAR (class_K :: classes) V_R_class_K map inj = *)
(*               let *)
(*                 (\* Get the 'unused' agent nodes of class [K] *\) *)
(*                 val V_a_class_K = get_nodes' map class_K *)
(*                 (\* Get the injections of any subset of the redex nodes *)
(*                  * in class [K] into V_a_class_K *\) *)
(*               in *)
                
(*               end *)
(*         in *)
(*           BAR classes V_R_class_K map NodeMap.empty *)
(*         end *)
(*     in *)
(*       fun component_injections (ctrls, eq_classes) = *)
(*         FOO eq_classes ControlSetMap.empty [] *)
(*     end *)

(*     val V_injs *)
(*       = ControlSetMap.Fold *)
(*           (fn ((ctrls, (ctrl_rel_K, (class_K, class_K_nodes))), V_injs) => *)
(*               ControlSet.fold *)
(*                 (fn (K, FIXME) => *)
(*                     let *)
(*                       (\* find the set of nodes in the agent that might *)
(*                        * have control K*\) *)
(*                       val  *)
(*                       (\* get the redex nodes with the control K *\) *)
(*                       val V_R_K = get_nodes K *)
(*                     in *)
                      
(*                     end) *)
(*                 FIXME class_K) *)
(*           [] components *)
(*     val E_injs = EdgeInjection.injections E_R E_a *)

(*     (\* extend the injections of nodes/edges with mappings of the *)
(*      * roots/names of the rule *\) *)
(*     fun add_map inj (map, acc) = (inj, map) :: acc *)
(*     fun add_maps maps_S disjoint_union_S *)
(*                  ((inj, inj_rng_C), acc) = *)
(*       foldr (add_map inj) acc (maps_S (disjoint_union_S inj_rng_C)) *)

(*     val add_root_maps = add_maps (RootPlaceMaps.maps R_R) *)
(*                                  (RootNodeUnion.disjoint_union R_a) *)
(*     val add_name_maps = add_maps (NameLinkMaps.maps Y_R) *)
(*                                  (NameEdgeUnion.disjoint_union Y_a) *)
(*   in *)
(*     ListUtil.cartesian_product (foldr add_root_maps [] V_injs) *)
(*                                (foldr add_name_maps [] E_injs) *)
(*   end *)

(* As above, but only supports equivalence classes of controls. *)
fun ctrl_resp_instantiations' rule ctrl_components_a E_a Y_a R_a =
  let
    val redex = PCRule.redex rule
    val {nodes = V_R, edges = E_R} = ConcreteBigraph.support redex
    val ctrl_R = ConcreteBigraph.ctrl redex
    val ctrl_inv_R = ConcreteBigraph.ctrl_inv redex
    fun get_nodes' map class_K = 
        case ControlSetMap.lookup map class_K of
          NONE   => NodeSet.empty
        | SOME V => V

    val (dont_care_links, dont_care_root) = PCRule.dont_cares rule
    val (dont_care_names, dont_care_edges)
      = LinkSet.fold
          (fn (LName x) => (fn (ns, es) =>
              (NameSet.insert x ns, es))
            | (LEdge e) => (fn (ns, es) =>
              (ns, EdgeSet.insert e es)))
          (NameSet.empty, EdgeSet.empty) dont_care_links

    val R_R = if dont_care_root then
                WordSet.empty
              else
                WordSet.tabulate (ConcreteBigraph.width redex)
                                         Word.fromInt
    val Y_R = NameSet.difference
                (ConcreteBigraph.outernames redex)
                dont_care_names
    val E_R = EdgeSet.difference E_R dont_care_edges

(*     val V_injs = NodeInjection.injections V_R V_a *)
    (* Group the nodes of the redex according to their equivalence class
     * and then generate all injections for each class.
     * The generate the combinations. *)
    local
      fun get_nodes_R K = case ControlMap.lookup ctrl_inv_R K of
                            NONE   => NodeSet.empty
                          | SOME V => V
      fun get_class_nodes_R class_K =
          ControlSet.fold
            (fn K => fn V => NodeSet.union V (get_nodes_R K))
            NodeSet.empty class_K

      fun merge_injs (inj, inj_rng_C) (inj', inj'_rng_C) =
          (NodeMap.mergeMap
             (fn _ => raise Fail "FIXME injections should have \
                                 \disjoint domains")
             inj inj',
           NodeSet.union inj_rng_C inj'_rng_C)
      fun cartesian_product_node_injs V_injs V_injs' =
          foldr (fn (i, acc) =>
                    foldr (fn (i', acc) => (merge_injs i i') :: acc)
                          acc V_injs')
                [] V_injs

      fun class_injections ([(_, (class_K, class_K_nodes_a))], V_injs) =
          let
            val class_K_nodes_R = get_class_nodes_R class_K
          in
            if NodeSet.isEmpty class_K_nodes_R then
              cartesian_product_node_injs
                V_injs [(NodeMap.empty, class_K_nodes_a)]
            else
              cartesian_product_node_injs
                V_injs (NodeInjection.injections class_K_nodes_R
                                                 class_K_nodes_a)
          end
        | class_injections ([], _) =
          raise Fail "FIXME empty control component \
                     \(this must not happen)"
        | class_injections (_, _)  =
          raise Fail "FIXME control components with more than one \
                     \equivalence class are not supported"
    in
      val V_injs
        = ControlSetMap.fold class_injections
                             [(NodeMap.empty, NodeSet.empty)]
                             ctrl_components_a
    end
    val E_injs = EdgeInjection.injections E_R E_a

    (* extend the injections of nodes/edges with mappings of the
     * roots/names of the rule *)
    fun add_map inj (map, acc) = (inj, map) :: acc
    fun add_maps maps_S disjoint_union_S
                 ((inj, inj_rng_C), acc) =
      foldr (add_map inj) acc (maps_S (disjoint_union_S inj_rng_C))

    val add_root_maps = add_maps (RootPlaceMaps.maps R_R)
                                 (RootNodeUnion.disjoint_union R_a)
    val add_name_maps = add_maps (NameLinkMaps.maps Y_R)
                                 (NameEdgeUnion.disjoint_union Y_a)

val insts =     ListUtil.cartesian_product (foldr add_root_maps [] V_injs)
                               (foldr add_name_maps [] E_injs)
val _ = print' ("number of instantiations: " ^ (Int.toString (length insts)) ^ "\n")
  in
insts
(*     ListUtil.cartesian_product (foldr add_root_maps [] V_injs) *)
(*                                (foldr add_name_maps [] E_injs) *)
  end


local
  structure Set = ControlSet
  structure Map = ControlMap
  type set = Set.Set
  type rel = set Map.map

  val domain : rel -> set = Set.fromList o Map.dom

  fun lookup R s =
    (case Map.lookup R s of
       NONE    => Set.empty
     | SOME S => S)

  fun transitive_step s (changed, R) =
    let
      val S  = lookup R s
      val S' = Set.fold (fn s' => fn S' => Set.union' S' (lookup R s'))
                        S S
    in
      (changed orelse (Set.size S) <> (Set.size S'),
       Map.add (s, S', R))
    end

  fun transitive_closure' dom R =
    (case Set.fold transitive_step (false, R) dom of
       (false, _) => R
     | (true, R') => transitive_closure' dom R')
  fun transitive_closure R = transitive_closure' (domain R) R


  fun reflexive_closure' dom R =
    Set.fold (fn s => fn R' =>
                 Map.add (s, Set.insert' s (lookup R s), R'))
             R dom
  fun reflexive_closure R = reflexive_closure' (domain R) R

in
  (* Find the transitive closure of a relation R \subseteq S x S
   * represented as a map S -> 2^S  *)
  fun transitive_reflexive_closure' dom R =
    reflexive_closure' dom (transitive_closure' dom R)
  fun transitive_reflexive_closure R =
    transitive_reflexive_closure' (domain R) R
  
end


(* Do a variety of analyses on a set of rules
 * FIXME specify analyses
 *)
fun analyze_rules controls_a ctrl_inv_a rules =
  let
    (* Utility functions *)
    fun get_ctrls ctrl_rel K =
      (case ControlMap.lookup ctrl_rel K of
         NONE    => ControlSet.empty
       | SOME Ks => Ks)
    fun get_ctrls' ctrl_rel_inv Ks =
      (case ControlSetMap.lookup ctrl_rel_inv Ks of
         NONE       => (ControlSet.empty, NodeSet.empty)
       | SOME Ks'_V => Ks'_V)
    fun get_nodes ctrl_inv K =
      (case ControlMap.lookup ctrl_inv K of
         NONE   => NodeSet.empty
       | SOME V => V)

    (* Extract the following from each rule and aggregate:
     * - what controls are used in the rule (agg = union)
     * - how do controls change (e.g. a K can stay a K or become an L)
     * - how many idle names occur in the redex (aggregate = max)
     * - does the redex have an edge? (aggregate = exists)
     * - does the redex have a node which doesn't contain a site?
     *                                                  (agg = exists)
     *)
    fun analyze_rule (rule, {controls, ctrl_rel, max_idle_name_count,
                             have_edge, have_idle_edge,
                             have_siteless_node}) =
      let
        val redex = PCRule.redex rule
        val {nodes, edges} = PCRule.support rule

        fun analyze_aspect ((NodeControl _, (Control K, Control K')),
                            {controls, ctrl_rel, idle_name_count,
                             has_edge, has_idle_edge, siteless_nodes}) =
            {controls = ControlSet.insert' K controls,
             ctrl_rel = ControlMap.add
                          (K,
                           ControlSet.insert'
                               K' (get_ctrls ctrl_rel K),
                           ctrl_rel),
             idle_name_count = idle_name_count,
             has_edge = has_edge,
             has_idle_edge = has_idle_edge,
             siteless_nodes = siteless_nodes}
          | analyze_aspect ((Presence (EName _), (Present 0w0, Present _)),
                            {controls, ctrl_rel, idle_name_count,
                             has_edge, has_idle_edge, siteless_nodes}) =
            {controls = controls,
             ctrl_rel = ctrl_rel,
             idle_name_count = idle_name_count + 1,
             has_edge = has_edge,
             has_idle_edge = has_idle_edge,
             siteless_nodes = siteless_nodes}
          | analyze_aspect ((Presence (EEdge _), (Present c, _)),
                            {controls, ctrl_rel, idle_name_count,
                             has_edge, has_idle_edge, siteless_nodes}) =
            {controls = controls,
             ctrl_rel = ctrl_rel,
             idle_name_count = idle_name_count,
             has_edge = true,
             has_idle_edge = has_idle_edge orelse c = 0w0,
             siteless_nodes = siteless_nodes}
          | analyze_aspect ((ChildParent (CSite _), (Place (PNode v), _)),
                            {controls, ctrl_rel, idle_name_count,
                             has_edge, has_idle_edge, siteless_nodes}) =
            {controls = controls,
             ctrl_rel = ctrl_rel,
             idle_name_count = idle_name_count,
             has_edge = has_edge,
             has_idle_edge = has_idle_edge,
             siteless_nodes = NodeSet.remove' v siteless_nodes}
          | analyze_aspect ((aspect, (value, value')), acc) = acc

        val table = PCRule.table rule
        val {controls = controls',
             ctrl_rel = ctrl_rel',
             idle_name_count,
             has_edge,
             has_idle_edge,
             siteless_nodes}
          = AspectMap.Fold analyze_aspect
                           {controls = controls,
                            ctrl_rel = ctrl_rel,
                            idle_name_count = 0,
                            has_edge = false,
                            has_idle_edge = false,
                            siteless_nodes = nodes}
                           table
      in
        {controls = controls',
         ctrl_rel = ctrl_rel',
         max_idle_name_count = Int.max (max_idle_name_count,
                                        idle_name_count),
         have_edge = have_edge orelse has_edge,
         have_idle_edge = have_idle_edge orelse has_idle_edge,
         have_siteless_node = have_siteless_node orelse
                                not (NodeSet.isEmpty siteless_nodes)}
      end
    (* ctrl_rel approximates the "dynamic control relation": given that a
     * node has control K, which controls might it have after 0 or more
     * reactions.
     *
     * We determine this by finding the reflexive transitive closure of
     * the one step relation which can be readily extracted from the
     * reaction rules.
     *
     * We also construct the inverse relation ctrl_rel_inv:
     * given that a node at some point has control K, which controls
     * might it have had initially (and what nodes in the agent have
     * a control in that set)?
     *)
    val {controls, ctrl_rel, max_idle_name_count,
         have_edge, have_idle_edge, have_siteless_node}
      = foldl analyze_rule
              {controls = ControlSet.empty,
               ctrl_rel = ControlMap.empty,
               max_idle_name_count = 0,
               have_edge = false,
               have_idle_edge = false,
               have_siteless_node = false}
              rules

    val ctrl_rel
      = transitive_reflexive_closure'
          (ControlSet.union' controls controls_a)
          ctrl_rel
    (* FIXME find 
     * ctrl_rel([K]) -> ([K], agent nodes with a control in [K]) *)
    val ctrl_rel_inv
      = ControlMap.Fold
          (fn ((K, Ks), inv) =>
              let
                val (class_K, class_K_nodes) = get_ctrls' inv Ks
              in
                ControlSetMap.add
                  (Ks,
                   (ControlSet.insert K class_K,
                    NodeSet.union class_K_nodes
                                  (get_nodes ctrl_inv_a K)),
                   inv)
              end)
          ControlSetMap.empty ctrl_rel

    (* We wish to find control respecting injections rho of the nodes
     * of a rule R --> R' into the nodes of the agent a. I.e we require
     *
     *   rho(v) = v'  =>  ctrl_R(v) \in ctrl_rel(ctrl_a(v'))
     *
     * Thus v' has to be drawn from the set
     *
     *   ctrl_inv_a(ctrl_rel_inv(ctrl_R(v)))
     *
     *
     * ctrl_rel gives a preorder <= on the set of controls:
     *
     *   K <= K'  iff  ctrl_rel(K) \subseteq ctrl_rel(K')
     *
     * Quotienting with the obvious equivalence, we get a strict partial
     * order <~ on equivalence classes of controls
     *
     *  [K] <~ [K']  iff  ctrl_rel(K) \subset ctrl_rel(K')
     *
     * (the equivalence classes are immediately available as the codomain
     * of ctrl_rel_inv.)
     *
     * The weakly connected components of the DAG given by <~ correspond
     * to controls that might be assigned to some common nodes.
     *
     * FIXME this is gibberish:
     * A topological sort of such a component gives us an ordering in
     * which to choose subsets of nodes corresponding to a control, in
     * the sense that:
     * we choose nodes from the smallest groups of nodes first.
     * E.g. if we have
     *
     *   agent  v1:A, v2:B, v3:B
     *   rule   v'1:A * v'2:B -> v'1:B * v'2:B
     *
     * then v1 might at some point be a legal injection for v'2, but
     * since no other nodes can become an A, we are forced to choose
     * v'1 -> v1 and then we can never choose v'2 -> v1.
     * By considering injections of nodes with control A in the redex
     * before nodes with control B, we will never explore dead ends when
     * constructiong injections.
     *)
    (* We sort the equivalence classes reverse topologically by sorting
     * their images under ctrl_rel by decreasing size.
     * NB: the elements of eq_classes are pairs
     *     (ctrl_rel(K), ([K], nodes with a control in [K])). *)
    local
      val eq_classes
        = ListSort.sort
            (fn ((ctrl_rel_K1, _), (ctrl_rel_K2, _)) =>
                Int.compare (ControlSet.size ctrl_rel_K2,
                             ControlSet.size ctrl_rel_K1))
            (ControlSetMap.list ctrl_rel_inv)
      (* We partition the DAG of equivalence classes into its weakly
       * connected components, each identified by the union of its
       * controls : ctrlset -> component
       * component = (ctrl_rel(K), ([K], {v | v:K' /\ K' \in [K]})) list
       *)
      fun find_components [] map = map
        | find_components
            ((c as (ctrl_rel_K, (class_K, class_K_nodes)))
             :: eq_classes) map =
        let
          val (ctrls, comp, rest)
            = foldr
                (fn (c' as (ctrl_rel_K', (class_K', class_K'_nodes)),
                     (ctrls, comp, rest)) =>
                    if ControlSet.all
                         (fn K'' =>
                             not (ControlSet.member K'' ctrl_rel_K))
                         ctrl_rel_K'
                    then
                      (ctrls, comp, c' :: rest)
                    else
                      (ControlSet.union ctrls class_K',
                       c' :: comp, rest))
                (class_K, [], []) eq_classes
        in
          find_components rest (ControlSetMap.add
                                  (ctrls,
                                   c :: comp,
                                   map))
        end
    in
      val ctrl_components = find_components eq_classes ControlSetMap.empty
    end
  in
    {controls = controls,
     ctrl_components = ctrl_components,
     max_idle_name_count = max_idle_name_count,
     have_edge = have_edge,
     have_idle_edge = have_idle_edge,
     have_siteless_node = have_siteless_node}
  end


(* Assumptions (FIXME verify that these are met):
 * - agent is ground
 * - rule names are unique
 * - rules are local, i.e. FIXME
 *
 * FIXME: idle eges in a redex R are significant if the changes connect one
 *        or more points to it. We must handle the case where idle
 *        edges in R are mapped to implicit edges in the agent!
 *        It seems that it would be sufficient to add a number of idle
 *        edges to the concrete agent - but how many?
 *        As most as many edges as we have ports can be used at
 *        any point, so maybe we can just add |ports| - |edges| idle
 *        edges to the agent? 
 *        Almost, but not quite: there is one special case that we should
 *        take into account:
 *
 *        a rule R --> R' which can capture idle edges (i.e. R has idle
 *        names Y), e.g:
 *
 *          A[x] || y/  -->  A[y] || x/ ,
 *
 *        cannot fire in contexts C on the form
 *
 *          C' o (id_X * /Y * id_1),
 *
 *        if there are fewer than |Y| idle edges (i.e. no edges to
 *        connect to some of the names of Y). The needed number of edges
 *        can at most be |Y| for a rule, and for a given set of rules
 *        we can simply take the maximum number of idle names over all 
 *        the redexes.
 *
 *        Thus, the number of idle edges we should add is
 *
 *          |ports| - |edges| + max_{R-->R' \in rules}(idle_names(R))
 * 
 *)
fun gen_state_space {agent, rules} =
  let
    val agent_conc = ConcreteBigraph.concretize agent
    val V_a        = ConcreteBigraph.nodes agent_conc
    val ctrl_inv_a = ConcreteBigraph.ctrl_inv agent_conc
    val E_a        = ConcreteBigraph.edges agent_conc
    val Y_a        = ConcreteBigraph.outernames agent_conc
    val R_a        = WordSet.tabulate
                       (ConcreteBigraph.width agent_conc) Word.fromInt
    val controls_a =  ConcreteBigraph.controls agent_conc
    val {controls = controls_R, ctrl_components,
         max_idle_name_count, have_edge, have_idle_edge,
         have_siteless_node}
        = analyze_rules controls_a ctrl_inv_a rules

    (* We need to know the (potential) number of ports so that we can
     * - add a sufficient number of idle edges.
     * - add a sufficient number of states for each edge
     *)
    val controls
      = ControlSet.union' controls_R controls_a
    val max_node_ports = ControlSet.fold (fn c => fn mp =>
                                             Int.max (Control.free c, mp))
                                         0 controls
    val max_ports = max_node_ports * (NodeSet.size V_a)
    (* Similarly, we must know the maximum number of nodes so that we can
     * add a sufficient number of states for each node/root. *)
    val max_nodes = NodeSet.size V_a

    (* Add a sufficient number of idle edges to the concrete agent. *)
    local
      fun fresh_edges' edges 0 = edges
        | fresh_edges' edges n = 
        fresh_edges' (EdgeSet.insert (Name.fresh NONE) edges) (n - 1)
    in
      val fresh_edges = fresh_edges' EdgeSet.empty
    end
    val E_a_idle
      = if max_idle_name_count > 0 orelse have_idle_edge then
          fresh_edges (max_ports - (EdgeSet.size E_a) + max_idle_name_count)
        else
          EdgeSet.empty
val _ = print' ("max ports: " ^ (Int.toString max_ports) ^ "\n")
val _ = print' ("|E_a|: " ^ (Int.toString (EdgeSet.size E_a)) ^ "\n")
val _ = print' ("max idle names: " ^ (Int.toString max_idle_name_count) ^ "\n")




    (* FIXME we don't handle rules where sites are moved.
     * trns : transition map
     * aspect_acts : (rule, instantiation) AspectMap.map *)
    fun add_aspect_transition rule inst
                              ((ChildParent (CSite _),
                                (value, value')),
                               (trns, aspect_acts)) =
      let
        val site_fixed
          = case (value, value') of
              (Place (PRoot r1), Place (PRoot r2)) => r1 = r2
            | (Place (PNode v1), Place (PNode v2)) => Node.== (v1, v2)
            | _                                    => false
      in
        if site_fixed then
          (trns, aspect_acts)
        else
          raise Fail "FIXME moving sites is not supported"
      end
      | add_aspect_transition rule inst
                              ((aspect, (value, value')),
                               (trns, aspect_acts)) =
        add_trns' (trns, aspect_acts) aspect value rule inst value'

    (* Add states and transitions for a link/place which implements
     * counting of the number of points/children it has. *)
    fun add_counting_states rule inst aspect min i j trns =
      if i < min then
        trns
      else
        add_counting_states
          rule inst aspect min (i - 1) (j - 1)
          (add_trns trns aspect (Present (Word.fromInt i))
                    rule inst (Present (Word.fromInt j)))

    fun add_instantiation rule (((V_inj, R_map), (E_inj, Y_map)),
                                (trns, aspect_acts,
                                 inst_ids, inst_count)) =
      let
        val inst = {rho_V = V_inj, rho_E = E_inj,
                    map_Y = Y_map, map_R = R_map}
        val (inst_ids', inst_count')
          = case InstantiationMap.lookup inst_ids inst of
              SOME _ => (inst_ids, inst_count)
            | NONE   => (InstantiationMap.add
                           (inst, inst_count, inst_ids),
                         inst_count + 1)
        val (aspect_changes, count_changes)
          = PCRule.instantiate' inst rule
        val (trns', aspect_acts')
          = AspectMap.Fold (add_aspect_transition rule inst)
                           (trns, aspect_acts)
                           aspect_changes
        (* Add transitions for counting aspects (Presence e). *)
        val (trns'', aspect_acts'')
          = EntityMap.Fold
              (fn ((e, change), (trns, aspect_acts)) =>
                  let
                    val (max_ent, is_root, is_link, is_node)
                      = case e of
                          ERoot _ => (max_nodes, true, false, false)
                        | ENode _ => (max_nodes - 1, false, false, true)
                        | _       => (max_ports, false, true, false)
                    val (min, max, j)
                      = if is_root
                           orelse (is_link andalso not have_edge)
                           orelse (is_node
                                   andalso not have_siteless_node) then
                          (* we don't need to be counting *)
                          (0, 0, 0)
                        else if change < 0 then
                          (~change, max_ent, max_ent + change)
                        else
                          (0, max_ent - change, max_ent)
                    val aspect = Presence e
                  in
                    (add_counting_states
                       rule inst aspect
                       min max j trns,
                     add_aspect_act aspect_acts aspect
                                    (rule, inst))
                  end)
              (trns', aspect_acts') count_changes
      in
        (trns'',
         aspect_acts'',
         inst_ids',
         inst_count')
      end

    fun add_rule (rule, trns) =
(print' ("adding rule: " ^ (PCRule.name rule) ^ "\n")
 ;     if PCRule.width rule = 1 then
        foldr (add_instantiation rule)
              trns
              (instantiations
                 rule V_a (EdgeSet.union E_a E_a_idle) Y_a R_a)
      else
        raise Fail "FIXME we only support rules of width 1"
)    (* as the previous, but only generates control respecting
     * instantiations.*)
    fun add_rule' (rule, trns) =
(print' ("adding rule: " ^ (PCRule.name rule) ^ "\n")
 ;     if PCRule.width rule = 1 then
        foldr (add_instantiation rule)
              trns
              (ctrl_resp_instantiations'
                 rule ctrl_components (EdgeSet.union E_a E_a_idle) Y_a R_a)
      else
        raise Fail "FIXME we only support rules of width 1"
)
    val (init_state, init_trans)
      = init_state_space agent_conc E_a_idle have_edge have_siteless_node
(*     val (transitions, aspect_acts, inst_ids, inst_count) *)
(*       = foldl add_rule *)
(*               (init_trans, AspectMap.empty, InstantiationMap.empty, 0) *)
(*               rules *)
    val (transitions, aspect_acts, inst_ids, inst_count)
      = foldl add_rule'
              (init_trans, AspectMap.empty, InstantiationMap.empty, 0)
              rules

(* (\* FIXME temp. output of the state space *\) *)
val out_old = !out
val _ = out := TextIO.openOut "bgstatespace2.pepa"
val _ = print' ("\n// instantiations\n// --------------\n" ^ (inst_ids2pepa inst_ids))
val _ = print' ("\n\n// transitions\n// -----------\n" ^ (trns2pepa' inst_ids transitions) ^ "\n")
(* val _ = print' ("\n// state\n// -----\n" ^ (state2pepa init_state) ^ "\n") *)
val _ = print' ("\n// state\n// -----\n" ^ (state2pepa' inst_ids aspect_acts init_state) ^ "\n")
val _ = TextIO.flushOut (!out)
val _ = TextIO.closeOut (!out)
val _ = out := out_old

(* val _ = print' ("\n// instantiations\n// --------------\n" ^ (inst_ids2string inst_ids)) *)
(* val _ = print' ("\n\n// transitions\n// -----------\n" ^ (trns2string' inst_ids transitions) ^ "\n") *)
(* val _ = print' ("\n// state\n// -----\n" ^ (state2string init_state) ^ "\n") *)
  in
    {initial     = init_state,
     transitions = transitions}
(*      transitions = init_trans} *)
  end
end
