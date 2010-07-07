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
signature BGASPECTS =
sig

type name
type node
type edge
type site
type root
type control
type portindex
type inst
type interface

datatype place =
         PNode of node
       | PRoot of root
structure PlaceOrder : ORDERING    where type T   = place
structure PlaceSet   : MONO_SET    where type elt = place
structure PlaceMap   : MONO_FINMAP where type dom = place
val place2string : place -> string
val place2pepa : place -> string

datatype child =
         CNode of node
       | CSite of site
structure ChildOrder : ORDERING    where type T   = child
structure ChildSet   : MONO_SET    where type elt = child
structure ChildMap   : MONO_FINMAP where type dom = child
val child2string : child -> string
val child2pepa : child -> string

datatype link =
         LName of name
       | LEdge of edge
structure LinkOrder : ORDERING    where type T   = link
structure LinkSet   : MONO_SET    where type elt = link
structure LinkMap   : MONO_FINMAP where type dom = link
val link2string : link -> string
val link2pepa : link -> string

datatype point =
         PName of name
       | PPort of (node * portindex)
structure PointOrder : ORDERING    where type T   = point
structure PointSet   : MONO_SET    where type elt = point
structure PointMap   : MONO_FINMAP where type dom = point
val point2string : point -> string
val point2pepa : point -> string

datatype entity =
         ENode of node | EEdge of edge
       | EName of name | ERoot of root
structure EntityOrder : ORDERING    where type T   = entity
structure EntitySet   : MONO_SET    where type elt = entity
structure EntityMap   : MONO_FINMAP where type dom = entity
val entity2string : entity -> string
val entity2pepa : entity -> string

datatype aspect =
         Presence of entity
       | NodeControl of node
       | ChildParent of child
       | PointLink of point
structure AspectOrder : ORDERING    where type T   = aspect
structure AspectSet   : MONO_SET    where type elt = aspect
structure AspectMap   : MONO_FINMAP where type dom = aspect
val aspect2string : aspect -> string
val aspect2pepa : aspect -> string

datatype value =
         Present of word (* number of children/points *)
       | Absent
       | Control of control
       | Place of place
       | Link of link
structure ValueOrder : ORDERING    where type T   = value
structure ValueSet   : MONO_SET    where type elt = value
structure ValueMap   : MONO_FINMAP where type dom = value
val valueEq : value -> value -> bool
val value2string : value -> string
val value2pepa : value -> string

(** The aspect change data type.*)
datatype change = 
         (** Delete a node or a parameter. *)
         Del of child
         (** Move a node or parameter to the given place. *)
       | Mov of child * place
         (** Copy a parameter to the given place. *)
       | Cop of site * place
         (** Add a node with the given control and connections at the
          * given place. *)
       | Add of node * control * link list * place
         (** Connect a port to the given link. *)
       | Con of (node * portindex) * link

(** Extract an instantiation corresponding to a set of changes with
 * respect to the given interfaces. *)
val inst : {changes : change list, I : interface, J : interface} -> inst

end
