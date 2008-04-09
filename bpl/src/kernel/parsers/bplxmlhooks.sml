(* Copyright (c) 2007  The BPL Group at the IT University of Copenhagen
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

(** Callback functions for SAX parsing a BPL brs expressed in XML.
 * @version $LastChangedRevision$
 * Modified: $Date: 2006/09/04 21:48:46 $ by: $Author: hniss $
 *)

functor BPLXMLHooks (
  structure Info : INFO
  structure Control : CONTROL
  structure Name : NAME
  structure NameSet : MONO_SET
  structure BgTerm : BGTERM
  ) :> BPLXMLHOOKS =
struct
  open IgnoreHooks
  type info = Info.info
  type name = Name.name
  type control = Control.control
  type nameset = NameSet.Set
  type bgterm = BgTerm.bgterm
  type ruledata = {
    name : string,
    redex : bgterm,
    react : bgterm,
    maps : ((int * Name.name list) * (int * Name.name list)) list,
    info : Info.info}

  (* Flag value indicating where a bigraph just parsed should go. *)
  datatype dest = BIGRAPHS | REDEX | REACT
  (* Data structure for holding partial BRS data. *)
  type AppData = {
    signatur : control list,
    bigraphs : bgterm list,
    bigraph  : {outernames : nameset,
                dest : dest,
                edges : nameset, 
                roots : bgterm list} option,
    root     : {localnames : nameset, children : bgterm list} option,
    node     : {bports : name list,
                fports : name list,
                children : bgterm list} list,
    site     : {idx : int, localnames : nameset} option,
    rules    : ruledata list,
    rule     : {redex : bgterm option,
                react : bgterm option,
                maps : ((int * Name.name list) * (int * Name.name list)) list,
                info : info option} option}
  val init = {
    signatur = [], bigraphs = [], bigraph = NONE, root = NONE,
    node = [], site = NONE, rules = [], rule = NONE}
  type AppFinal = {signatur : control list, rules : ruledata list}
  fun getBRS brs = brs
  fun hookFinish _ = {signatur = [], rules = []} (* FIXME *)
end