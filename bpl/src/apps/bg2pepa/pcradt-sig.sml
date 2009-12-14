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

(** PCR Abstract Data Types module. This module contains just the abstract
 * data types of the PCR library, ML syntactical sugaring and error
 * handling.
 * For parsing and I/O functionality, see the PCR module.
 * @version $LastChangedRevision: 2725 $
 *)
signature PCR_ADT =
sig
  type name
  type node
  type edge
  type site
  type root
  type portindex
  type control
  type inst
  type interface
  type place
  type child
  type link
  type point
  type aspect
  type value
  type change
  type conbg
  type absbg
  type pcrule
  type reaction_rule
  type state
  type transitions

  structure Name : NAME
  structure Node : NODE
  structure Edge : EDGE
  structure BgAspects : BGASPECTS
  structure ConcreteBigraph : CONCRETE_BIGRAPH
  structure PCRule : PCRULE
    where type preconds = (aspect * value) list
      and type changes  = change list
  structure BgAspectsSSGen : BGASPECTSSSGEN

  sharing type name =
               Name.name =
               BgAspects.name

=(*  sharing type*) edge =
(*               Edge.edge =
*)               BgAspects.edge

  sharing type node =
               Node.node =
               BgAspects.node =
               ConcreteBigraph.node

  sharing type site =
               BgAspects.site

  sharing type root =
               BgAspects.root

  sharing type control =
               BgAspects.control

  sharing type portindex =
               BgAspects.portindex

  sharing type inst =
               BgAspects.inst

  sharing type interface =
               BgAspects.interface =
               ConcreteBigraph.interface

  sharing type place =
               BgAspects.place

  sharing type child =
               BgAspects.child

  sharing type link =
               BgAspects.link

  sharing type point =
               BgAspects.point

  sharing type aspect =
               BgAspects.aspect =
               ConcreteBigraph.aspect

  sharing type change =
               BgAspects.change

  sharing type reaction_rule =
               PCRule.reaction_rule

  sharing type conbg =
               ConcreteBigraph.conbg

  sharing type pcrule =
               PCRule.pcrule =
               BgAspectsSSGen.pcrule
end
