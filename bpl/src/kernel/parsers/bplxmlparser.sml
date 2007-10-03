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

(** Parser for brs'es expressed in XML.
 * @version $LastChangedRevision: 442 $
 * Modified: $Date: 2006/09/04 21:48:46 $ by: $Author: hniss $
 *)
 
 functor BPLXMLParser (BPLXMLHooks : BPLXMLHOOKS) :> BPLXMLPARSER
   where type info = BPLXMLHooks.info
     and type name = BPLXMLHooks.name
     and type control = BPLXMLHooks.control
     and type bgterm = BPLXMLHooks.bgterm =
 struct
   type info = BPLXMLHooks.info
   type name = BPLXMLHooks.name
   type control = BPLXMLHooks.control
   type bgterm = BPLXMLHooks.bgterm
   type ruledata = BPLXMLHooks.ruledata
   type brs = {signatur : control list, rules : ruledata list}
   structure Parser = Parse
    (structure Dtd = Dtd
     structure Hooks = BPLXMLHooks
     structure ParserOptions = ParserOptions ()
     structure Resolve = ResolveNull)
   fun parse uri dtd =
     BPLXMLHooks.getBRS
       (Parser.parseDocument uri dtd BPLXMLHooks.init)
   fun parseFile filename =
     parse (SOME (Uri.String2Uri ("file://" ^ filename))) NONE
 end