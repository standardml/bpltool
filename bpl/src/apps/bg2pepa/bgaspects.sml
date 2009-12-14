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

(** Abstract data type for modelling concrete bigraph "aspects".
 * @version $LastChangedRevision: 2717 $
 *)
functor BgAspects(structure Name          : NAME
                  structure Control       : CONTROL
                  structure Interface     : INTERFACE
                  structure Instantiation : INSTANTIATION
                  structure Node          : NODE
                  structure Edge          : EDGE
                 ) :> BGASPECTS
                  where type name      = Name.name
                    and type control   = Control.control 
                    and type interface = Interface.interface
                    and type inst      = Instantiation.inst
                    and type node      = Node.node
(*                    and type edge      = Edge.edge
*)                    and type edge      = Name.name
                    and type site      = word
                    and type root      = word
                    and type portindex = word =
struct

type name = Name.name
type node = Node.node
(*type edge = Edge.edge
*)type edge = Name.name
type site = word
type root = word
type control = Control.control
type portindex = word
type inst = Instantiation.inst
type interface = Interface.interface

(* utility function for converting a compare to a less-than *)
fun compare2lt compare x y =
  case compare (x, y) of
    LESS => true
  | _    => false

type port = node * portindex
structure PortOrder =
struct
  type T = port
  fun lt ((n1, p1) : port) ((n2, p2) : port)
    = case Node.compare (n1, n2) of
        LESS    => true
      | GREATER => false
      | EQUAL   => p1 < p2
end
structure PortSet = OrderSet (PortOrder)
structure PortMap = OrderFinMap (PortOrder)

datatype place =
         PNode of node
       | PRoot of word
fun placeEq (PNode v1) (PNode v2) = Node.== (v1, v2)
  | placeEq (PRoot r1) (PRoot r2) = r1 = r2
  | placeEq _          _          = false
structure PlaceOrder =
struct
  type T = place
  fun lt (PNode n1) (PNode n2) = Node.< (n1,n2)
    | lt (PRoot s1) (PRoot s2) = s1 < s2
    | lt (PNode n1) _ = true
    | lt _ _          = false
end
structure PlaceSet = OrderSet (PlaceOrder)
structure PlaceMap = OrderFinMap (PlaceOrder)

datatype child =
         CNode of node
       | CSite of word
structure ChildOrder = 
struct
  type T = child
  fun lt (CNode n1) (CNode n2) = Node.< (n1,n2)
    | lt (CSite s1) (CSite s2) = s1 < s2
    | lt (CNode n1) _ = true
    | lt _ _          = false
end
structure ChildSet = OrderSet (ChildOrder)
structure ChildMap = OrderFinMap (ChildOrder)

datatype link =
         LName of name
       | LEdge of edge
fun linkEq (LName n1) (LName n2) = Name.== (n1, n2)
  | linkEq (LEdge e1) (LEdge e2) = Name.== (e1, e2)
  | linkEq _          _          = false
structure LinkOrder =
struct
  type T = link
  fun lt (LName n1) (LName n2) = Name.< (n1,n2)
    | lt (LEdge e1) (LEdge e2) = Name.< (e1,e2)
    | lt (LName n1) _ = true
    | lt _ _          = false
end
structure LinkSet = OrderSet (LinkOrder)
structure LinkMap = OrderFinMap (LinkOrder)

datatype point =
         PName of name
       | PPort of port
structure PointOrder =
struct
  type T = point
  fun lt (PName n1) (PName n2) = Name.< (n1,n2)
    | lt (PPort p1) (PPort p2) = PortOrder.lt p1 p2
    | lt (PName n1) _ = true
    | lt _ _          = false
end
structure PointSet = OrderSet (PointOrder)
structure PointMap = OrderFinMap (PointOrder)

datatype entity =
         ENode of node | EEdge of edge
       | EName of name | ERoot of word
structure EntityOrder =
struct
  type T = entity
  fun lt (ENode n1) (ENode n2) = Node.< (n1, n2)
    | lt (EEdge e1) (EEdge e2) = Name.< (e1, e2)
    | lt (EName n1) (EName n2) = Name.< (n1, n2)
    | lt (ERoot r1) (ERoot r2) = r1 < r2
    | lt (ENode _) _ = true
    | lt _ (ENode _) = false
    | lt (EEdge _) _ = true
    | lt _ (EEdge _) = false
    | lt (EName _) _ = true
    | lt _ _         = false
end
structure EntitySet = OrderSet (EntityOrder)
structure EntityMap = OrderFinMap (EntityOrder)

datatype aspect =
         Presence of entity
       | NodeControl of node
       | ChildParent of child
       | PointLink of point
structure AspectOrder =
struct
  type T = aspect
  fun lt (Presence e1)    (Presence e2)    = EntityOrder.lt e1 e2
    | lt (NodeControl n1) (NodeControl n2) = Node.< (n1, n2)
    | lt (ChildParent c1) (ChildParent c2) = ChildOrder.lt c1 c2 
    | lt (PointLink p1)   (PointLink p2)   = PointOrder.lt p1 p2
    | lt (Presence _) _    = true
    | lt _ (Presence _)    = false
    | lt (NodeControl _) _ = true
    | lt _ (NodeControl _) = false
    | lt (ChildParent _) _ = true
    | lt _ _               = false
end
structure AspectSet = OrderSet (AspectOrder)
structure AspectMap = OrderFinMap (AspectOrder)

datatype value =
         Present of word
       | Absent
       | Control of control
       | Place of place
       | Link of link
fun valueEq (Present n1) (Present n2) = n1 = n2
  | valueEq  Absent       Absent      = true
  | valueEq (Control c1) (Control c2) = Control.eq c1 c2
  | valueEq (Place p1)   (Place p2)   = placeEq p1 p2
  | valueEq (Link l1)    (Link l2)    = linkEq l1 l2
  | valueEq _            _            = false
structure ValueOrder =
struct
  type T = value
  fun lt (Present n1) (Present n2) = n1 < n2
    | lt  Absent       Absent      = false
    | lt (Control c1)
         (Control c2)  = compare2lt Control.compare c1 c2
    | lt (Place p1)
         (Place p2)    = PlaceOrder.lt p1 p2
    | lt (Link l1)
         (Link l2)     = LinkOrder.lt l1 l2
    | lt (Present _) _ = true
    | lt _ (Present _) = false
    | lt Absent _      = true
    | lt _ Absent      = false
    | lt (Control _) _ = true
    | lt _ (Control _) = false
    | lt (Place _) _   = true
    | lt _ _           = false
end
structure ValueSet = OrderSet (ValueOrder)
structure ValueMap = OrderFinMap (ValueOrder)

datatype change =
         Del of child
       | Mov of child * place
       | Cop of site * place
       | Add of node * control * link list * place
       | Con of port * link

fun root2string r = "0w" ^ (Word.toString r)
val site2string = root2string
val root2pepa = Word.toString
val site2pepa = root2pepa

fun place2string' sp v2s r2s (PNode n) =
                  "PNode" ^ sp ^ (v2s n)
  | place2string' sp v2s r2s (PRoot r) =
                  "PRoot" ^ sp ^ (r2s r)
val place2string = place2string' " " Node.unmk root2string
val place2pepa   = place2string' "__" Node.unmk root2pepa
fun place2pepa (PNode n) = "Node__" ^ (Node.unmk n)
  | place2pepa (PRoot r) = "Root__" ^ (root2pepa r)

fun child2string' sp v2s s2s (CNode v) =
                  "CNode" ^ sp ^ (v2s v)
  | child2string' sp v2s s2s (CSite s) =
                  "CSite" ^ sp ^ (s2s s)
val child2string = child2string' " " Node.unmk site2string
val child2pepa   = child2string' "__" Node.unmk site2pepa
fun child2pepa (CNode v) = "Node__" ^ (Node.unmk v)
  | child2pepa (CSite s) = "Site__" ^ (site2pepa s)

fun link2string' sp n2s e2s (LName n) = 
                 "LName" ^ sp ^ (n2s n)
  | link2string' sp n2s e2s (LEdge e) = 
                 "LEdge" ^ sp ^ (e2s e)
val link2string = link2string' " " Name.unmk Name.unmk
val link2pepa   = link2string' "__" Name.unmk Name.unmk
fun link2pepa (LName n) = "Name__" ^ (Name.unmk n)
  | link2pepa (LEdge e) = "Edge__" ^ (Name.unmk e)

fun point2string' sp lp rp sep n2s v2s i2s (PName n) =
                  "PName" ^ sp ^ (n2s n)
  | point2string' sp lp rp sep n2s v2s i2s (PPort (v, i)) =
                  "PPort" ^ sp ^ lp ^ (v2s v)
                  ^ sep ^ (i2s i) ^ rp
val point2string = point2string' " " "(" ")" ", 0w"
                                 Name.unmk Node.unmk Word.toString
val point2pepa   = point2string' "__" "" "" "__"
                                 Name.unmk Node.unmk Word.toString
fun point2pepa (PName n)      = "InnerName__" ^ (Name.unmk n)
  | point2pepa (PPort (v, i)) = "Node__" ^ (Node.unmk v)
                                ^ "__port__" ^ (Word.toString i)

fun entity2string' sp v2s e2s n2s r2s (ENode v) =
                   "ENode" ^ sp ^ (v2s v)
  | entity2string' sp v2s e2s n2s r2s (EEdge e) = 
                   "EEdge" ^ sp ^ (e2s e)
  | entity2string' sp v2s e2s n2s r2s (EName n) = 
                   "EName" ^ sp ^ (n2s n)
  | entity2string' sp v2s e2s n2s r2s (ERoot r) = 
                   "ERoot" ^ sp ^ (r2s r)
val entity2string = entity2string' " " Node.unmk Name.unmk
                                   Name.unmk root2string
val entity2pepa   = entity2string' "__" Node.unmk Name.unmk
                                   Name.unmk root2pepa
fun entity2pepa (ENode v) = "Node__" ^ (Node.unmk v)
  | entity2pepa (EEdge e) = "Edge__" ^ (Name.unmk e)
  | entity2pepa (EName n) = "Name__" ^ (Name.unmk n)
  | entity2pepa (ERoot r) = "Root__" ^ (root2pepa r)

fun aspect2string' sp lp rp e2s n2s c2s p2s (Presence e) =
                   "Presence" ^ sp ^ lp ^ (e2s e) ^ rp
  | aspect2string' sp lp rp e2s n2s c2s p2s (NodeControl n) =
                   "NodeControl" ^ sp ^ lp ^ (n2s n) ^ rp
  | aspect2string' sp lp rp e2s n2s c2s p2s (ChildParent c) =
                   "ChildParent" ^ sp ^ lp ^ (c2s c) ^ rp
  | aspect2string' sp lp rp e2s n2s c2s p2s (PointLink p) =
                   "PointLink" ^ sp ^ lp ^ (p2s p) ^ rp
val aspect2string = aspect2string' " " "(" ")"
                                   entity2string Node.unmk
                                   child2string point2string
val aspect2pepa   = aspect2string' "__" "" ""
                                   entity2pepa Node.unmk
                                   child2pepa point2pepa
fun aspect2pepa (Presence e)    = entity2pepa e
  | aspect2pepa (NodeControl n) = "Node__" ^ (Node.unmk n) ^ "__ctrl"
  | aspect2pepa (ChildParent c) = (child2pepa c) ^ "__prnt"
  | aspect2pepa (PointLink p)   = point2pepa p

fun value2string' sp lp rp n2s c2s p2s l2s (Present n) =
    "Present" ^ sp ^ lp ^ (n2s n) ^ rp
  | value2string' sp lp rp n2s c2s p2s l2s  Absent     = "Absent"
  | value2string' sp lp rp n2s c2s p2s l2s (Control c) =
    "Control" ^ sp ^ lp ^ (c2s c) ^ rp
  | value2string' sp lp rp n2s c2s p2s l2s (Place p)   =
    "Place" ^ sp ^ (p2s p)
  | value2string' sp lp rp n2s c2s p2s l2s (Link l)    =
    "Link" ^ sp ^ lp ^ (l2s l) ^ rp
val value2string = value2string' " " "(" ")"
                                 (fn n => "0w" ^ (Word.toString n))
                                 Control.name place2string link2string
val value2pepa   = value2string' "__" "" ""
                                 Word.toString
                                 Control.name place2pepa link2pepa
fun value2pepa (Present n) = "Present" ^ "__" ^ (Word.toString n)
  | value2pepa  Absent     = "Absent"
  | value2pepa (Control c) = Control.name c
  | value2pepa (Place p)   = place2pepa p
  | value2pepa (Link l)    = link2pepa l

(* Extract an instantiation corresponding to a set of changes with
 * respect to the given interfaces. *)
fun inst {changes, I, J} =
  raise Fail "FIXME not impl."

end
