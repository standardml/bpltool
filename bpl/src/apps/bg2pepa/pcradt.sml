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

(** PCR Abstract Data Types module.
 * @version $LastChangedRevision: 2725 $
 *)
functor PCRADT (
  structure BGADT : BG_ADT
    where type ErrorHandler.ppstream    = PrettyPrint.ppstream
      and type ErrorHandler.break_style = PrettyPrint.break_style
      and type ErrorHandler.origin      = Origin.origin
) : PCR_ADT
      where type name          = BGADT.Name.name
and type edge          = BGADT.Name.name
        and type control       = BGADT.Control.control
        and type inst          = BGADT.Instantiation.inst
        and type interface     = BGADT.Interface.interface
        and type reaction_rule = BGADT.Rule.rule
        and type site          = word
        and type root          = word
        and type portindex     = word =
struct

type name          = BGADT.Name.name
type control       = BGADT.Control.control
type inst          = BGADT.Instantiation.inst
type interface     = BGADT.Interface.interface
type reaction_rule = BGADT.Rule.rule

structure ErrorHandler = BGADT.ErrorHandler

structure Name = BGADT.Name
structure NameSubset
  = Subset (
      structure LazyList = LazyList
      structure Set      = BGADT.NameSet)

structure ControlOrder =
struct
  type T = BGADT.Control.control
  fun lt c1 c2 = (BGADT.Control.compare (c1, c2)) = LESS
end
structure ControlSet = OrderSet (ControlOrder)
structure ControlMap = OrderFinMap (ControlOrder)

structure ControlSetOrder
  = SetCompare (
      structure Set      = ControlSet
      structure EltOrder = ControlOrder)
structure ControlSetMap = OrderFinMap (ControlSetOrder)

structure Node 
  = Node(
      structure ErrorHandler = ErrorHandler
      structure Control      = BGADT.Control)
structure NodeSet    = Node.NodeSet
structure NodeSubset
  = Subset (
      structure LazyList = LazyList
      structure Set      = NodeSet)
structure NodeMap = OrderFinMap(type T = Node.node
                                fun lt n1 n2 = Node.< (n1,n2))
type node = Node.node

structure Edge = Edge(structure ErrorHandler = ErrorHandler)
structure EdgeSet = Edge.EdgeSet
structure EdgeSubset
  = Subset (
      structure LazyList = LazyList
      structure Set      = EdgeSet)
(*type edge = Edge.edge*)
type edge = BGADT.Name.name

structure BgAspects
  = BgAspects (
      structure Name = BGADT.Name
      structure Control = BGADT.Control
      structure Interface = BGADT.Interface
      structure Instantiation = BGADT.Instantiation
      structure Node = Node
      structure Edge = Edge)

open BgAspects
type port = node * portindex

structure WordOrder =
struct
  type T = word
  fun lt (i1:T) i2 = i1 < i2
end
structure WordSet = OrderSet (WordOrder)
structure WordMap = OrderFinMap (WordOrder)

structure ConcreteBigraph
  = ConcreteBigraph(
      structure Info        = BGADT.Info
      structure Name        = BGADT.Name
      structure NameSet     = BGADT.NameSet
      structure NameMap     = BGADT.NameMap
      structure WordMap     = WordMap
      structure Control     = BGADT.Control
      structure ControlSet  = ControlSet
      structure ControlMap  = ControlMap
      structure Interface   = BGADT.Interface
      structure Ion         = BGADT.Ion
      structure Link        = BGADT.Link
      structure Permutation = BGADT.Permutation
      structure Wiring      = BGADT.Wiring
      structure BgVal       = BGADT.BgVal
      structure BgBDNF      = BGADT.BgBDNF
      structure BgAspects   = BgAspects
      structure Node        = Node
      structure NodeSet     = NodeSet
      structure NodeMap     = NodeMap
      structure Edge        = Edge
(*      structure EdgeSet     = EdgeSet*)
      structure EdgeSet     = BGADT.NameSet
      structure EdgeMap     = BGADT.NameMap)
type conbg = ConcreteBigraph.conbg
type absbg = ConcreteBigraph.absbg

structure PCRule
  = PCRule(
      structure Info            = BGADT.Info
      structure WordMap         = WordMap
      structure Name            = BGADT.Name
      structure NameMap         = BGADT.NameMap
      structure ControlSet      = ControlSet
      structure Node            = Node
      structure NodeMap         = NodeMap
(*      structure EdgeSet         = EdgeSet*)
      structure EdgeMap         = BGADT.NameMap
      structure BgAspects       = BgAspects
      structure ConcreteBigraph = ConcreteBigraph
      structure Instantiation   = BGADT.Instantiation
      structure BgVal           = BGADT.BgVal
      structure BgBDNF          = BGADT.BgBDNF
      structure ReactionRule    = BGADT.Rule)
type pcrule = PCRule.pcrule

(* Rule names are assumed unique! *)
structure PCRuleOrder =
struct
  type T = PCRule.pcrule
  fun lt r1 r2 = String.< (PCRule.name r1, PCRule.name r2)
end
structure PCRuleMap = OrderFinMap (PCRuleOrder)

structure SupportOrder =
struct
  structure WordSetCompare
    = SetCompare (
        structure Set      = WordSet
        structure EltOrder = WordOrder)
  structure NameSetCompare
    = SetCompare (
        structure Set      = BGADT.Name.NameSet
        structure EltOrder = BGADT.Name.Order)
  structure NodeSetCompare
    = SetCompare (
        structure Set      = Node.NodeSet
        structure EltOrder = Node.Order)

  type T = {nodes : NodeSet.Set, edges : BGADT.NameSet.Set,
            roots : WordSet.Set, names : BGADT.NameSet.Set}
  fun lt {nodes = V1, edges = E1, roots = R1, names = Y1}
         {nodes = V2, edges = E2, roots = R2, names = Y2} =
    case WordSetCompare.compare R1 R2 of
      LESS    => true
    | GREATER => false
    | EQUAL   =>
   (case NameSetCompare.compare Y1 Y2 of
      LESS    => true
    | GREATER => false
    | EQUAL   =>
   (case NodeSetCompare.compare V1 V2 of
      LESS    => true
    | GREATER => false
    | EQUAL   => NameSetCompare.lt E1 E2))
end
structure SupportMap = OrderFinMap (SupportOrder)

type instantiation = {rho_V : Node.node NodeMap.map,
                      rho_E : BGADT.Name.name BGADT.NameMap.map,
                      map_Y : BgAspects.link BGADT.NameMap.map,
                      map_R : BgAspects.place WordMap.map}
structure InstantiationOrder =
struct
  structure WordMapCompare
    = MapCompare (
        structure Map      = WordMap
        structure DomOrder = WordOrder)
  structure NameMapCompare
    = MapCompare (
        structure Map      = BGADT.NameMap
        structure DomOrder = BGADT.Name.Order)
  structure EdgeMapCompare
    = MapCompare (
        structure Map      = BGADT.NameMap
        structure DomOrder = BGADT.Name.Order)
  structure NodeMapCompare
    = MapCompare (
        structure Map      = NodeMap
        structure DomOrder = Node.Order)

  type T = instantiation
  fun lt {rho_V = rho_V1, rho_E = rho_E1,
          map_Y = map_Y1, map_R = map_R1}
         {rho_V = rho_V2, rho_E = rho_E2,
          map_Y = map_Y2, map_R = map_R2} =
    case NodeMapCompare.compare Node.Order.lt rho_V1 rho_V2 of
      LESS    => true
    | GREATER => false
    | EQUAL   =>
   (case EdgeMapCompare.compare BGADT.Name.Order.lt rho_E1 rho_E2 of
      LESS    => true
    | GREATER => false
    | EQUAL   =>
   (case NameMapCompare.compare BgAspects.LinkOrder.lt map_Y1 map_Y2 of
      LESS    => true
    | GREATER => false
    | EQUAL   =>
   (case WordMapCompare.compare BgAspects.PlaceOrder.lt map_R1 map_R2 of
      LESS    => true
    | _       => false)))
end
structure InstantiationMap = OrderFinMap (InstantiationOrder)

type action = PCRule.pcrule * instantiation
structure ActionOrder =
struct
  type T = action
  fun lt (rule1, inst1) (rule2, inst2) =
    if PCRuleOrder.lt rule1 rule2 then
      true
    else if PCRuleOrder.lt rule2 rule1 then
      false
    else
      InstantiationOrder.lt inst1 inst2
end
structure ActionSet = OrderSet (ActionOrder)

structure BgAspectsSSGen
  = BgAspectsSSGen(
      structure Word             = Word
      structure WordSet          = WordSet
      structure WordMap          = WordMap
      structure Name             = BGADT.Name
      structure NameSet          = BGADT.NameSet
      structure NameMap          = BGADT.NameMap
      structure Control          = BGADT.Control
      structure ControlSet       = ControlSet
      structure ControlMap       = ControlMap
      structure ControlSetMap    = ControlSetMap
      structure Node             = Node
      structure NodeSet          = NodeSet
      structure NodeSubset       = NodeSubset
      structure NodeMap          = NodeMap
      structure Edge             = Edge
(*      structure EdgeSet          = EdgeSet*)
      structure EdgeSet          = BGADT.NameSet
(*       structure EdgeSubset       = EdgeSubset *)
      structure EdgeSubset       = NameSubset
      structure EdgeMap          = BGADT.NameMap
      structure BgVal            = BGADT.BgVal
      structure BgAspects        = BgAspects
      structure ConcreteBigraph  = ConcreteBigraph
      structure PCRule           = PCRule
      structure PCRuleMap        = PCRuleMap
      type instantiation         = instantiation
      structure InstantiationMap = InstantiationMap
      structure ActionSet        = ActionSet)
type state       = BgAspectsSSGen.state
type transitions = BgAspectsSSGen.transitions
end
