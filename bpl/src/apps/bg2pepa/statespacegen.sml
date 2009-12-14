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
functor StateSpaceGen (
  structure WordSet : MONO_SET
    where type elt = word
  structure WordMap : MONO_FINMAP
    where type dom = word
  structure Name : NAME
  structure NameSet : MONO_SET
  structure NameMap : MONO_FINMAP
  structure Control : CONTROL
  structure ControlSet : MONO_SET
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
               ControlSet.elt
  sharing type ControlSet.Set =
               ConcreteBigraph.controlset
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
) : STATESPACEGEN
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
(* ( print (((Int.toString o length o AspectMap.dom) trns) ^ "\n") *)
(* (\* ; map (fn a => print (aspect2string a ^ "\n")) (AspectMap.dom trns) *\) *)
(* ; print (trns2string trns ^ "\n") *)
(* ; print ("\n" ^ (aspect2string aspect) ^ "\n") *)
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
fun init_state_space conbg =
  let
    fun add_state ((aspect, value), trns) =
      let
        val values = ValueMap.singleton (value, PCRuleMap.empty)
      in
        AspectMap.add (aspect, values, trns)
      end
      
    val init_state = ConcreteBigraph.unmk'' conbg
  in
    (init_state,
     AspectMap.Fold add_state AspectMap.empty init_state)
  end

(* Find all instantiations from a rule into a set of (aspect, {value})
 * pairs AV, which respects values FIXME define.
 * 
 * 
 *)
fun value_respecting_instantiations rule AV =
  let
    val {name, preconds, changes} = PCRule.unmk rule
  in
    
  end

(* Compute the states that are reachable by one application of
 * any of the rules, assumming that aspects can have any of the
 * values in AV = {(aspect, {value})}.
 *
 * result = empty
 * for each rule r = (AV_R, AV_R') do
 *   for each value respecting instantiation rho : r -> AV do
 *     result U= rho(AV_R')
 * *)
fun step rules (trns, AV) =
  let
    val 
  in
   trns' 
  end

(* Assumptions (FIXME verify that these are met):
 * - agent is ground
 * - rule names are unique
 * - rules are local, i.e. FIXME
 *
 * FIXME: idle eges in R are significant if the changes connect one
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
    val E_a        = ConcreteBigraph.edges agent_conc
    val Y_a        = ConcreteBigraph.outernames agent_conc
    val R_a        = WordSetTabulate.tabulate
                       (ConcreteBigraph.width agent_conc) Word.fromInt

    (* We need to know the (potential) number of ports so that we can
     * - add a sufficient number of idle edges.
     * - add a sufficient number of states for each edge
     *)
    val controls = ConcreteBigraph.controls agent_conc
    val max_node_ports = ControlSet.fold (fn c => fn mp =>
                                             Int.max (Control.free c, mp))
                                         0 controls
    val max_ports = max_node_ports * (NodeSet.size V_a)
    (* Similarly, we must know the maximum number of nodes so that we can
     * add a sufficient number of states for each node/root. *)
    val max_nodes = NodeSet.size V_a

    (* As discussed in the function comment, we need the maximum number
     * of idle names in the redexes. *)
    val max_idle_names = foldl Int.max 0
                               (map (NameSet.size
                                     o ConcreteBigraph.idle_names
                                     o PCRule.redex) rules)
    (* Add a sufficient number of idle edges to the concrete agent. *)
    local
      fun fresh_edges' edges 0 = edges
        | fresh_edges' edges n = 
        fresh_edges' (EdgeSet.insert (Name.fresh NONE) edges) (n - 1)
    in
      val fresh_edges = fresh_edges' EdgeSet.empty
    end
    val E_a_idle
      = fresh_edges (max_ports - (EdgeSet.size E_a) + max_idle_names)
val _ = print ("max ports: " ^ (Int.toString max_ports) ^ "\n")
val _ = print ("|E_a|: " ^ (Int.toString (EdgeSet.size E_a)) ^ "\n")
val _ = print ("max idle names: " ^ (Int.toString max_idle_names) ^ "\n")

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
          = PCRule.instantiate inst rule
        val (trns', aspect_acts')
          = AspectMap.Fold (add_aspect_transition rule inst)
                           (trns, aspect_acts)
                           aspect_changes
        val (trns'', aspect_acts'')
          = EntityMap.Fold (fn ((e, change), (trns, aspect_acts)) =>
                               let
                                 val max_ent = case e of
                                                 ERoot _ => max_nodes
                                               | ENode _ => max_nodes - 1
                                               | _       => max_ports
                                 val (min, max, j)
                                   = if change < 0 then
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
      if PCRule.width rule = 1 then
        foldr (add_instantiation rule)
              trns
              (instantiations
                 rule V_a (EdgeSet.union E_a E_a_idle) Y_a R_a)
      else
        raise Fail "FIXME we only support rules of width 1"

    val (init_state, init_trans) = init_state_space agent_conc
    val (transitions, aspect_acts, inst_ids, inst_count)
      = foldl add_rule
              (init_trans, AspectMap.empty, InstantiationMap.empty, 0)
              rules

(* (\* FIXME temp. output of the state space *\) *)
(* val _ = print ("\n// instantiations\n// --------------\n" ^ (inst_ids2pepa inst_ids)) *)
(* val _ = print ("\n\n// transitions\n// -----------\n" ^ (trns2pepa' inst_ids transitions) ^ "\n") *)
(* (\* val _ = print ("\n// state\n// -----\n" ^ (state2pepa init_state) ^ "\n") *\) *)
(* val _ = print ("\n// state\n// -----\n" ^ (state2pepa' inst_ids aspect_acts init_state) ^ "\n") *)

(* val _ = print ("\n// instantiations\n// --------------\n" ^ (inst_ids2string inst_ids)) *)
(* val _ = print ("\n\n// transitions\n// -----------\n" ^ (trns2string' inst_ids transitions) ^ "\n") *)
(* val _ = print ("\n// state\n// -----\n" ^ (state2string init_state) ^ "\n") *)
  in
    {initial     = init_state,
     transitions = transitions}
  end
end
