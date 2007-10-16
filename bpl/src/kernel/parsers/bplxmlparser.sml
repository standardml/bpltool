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
 
functor BPLXMLParser (structure BPLXMLHooks : BPLXMLHOOKS)
  :> BPLXMLPARSER
  where type initDatatype = BPLXMLHooks.initDatatype
    and type resulttype = BPLXMLHooks.resulttype
  =
struct
  type initDatatype = BPLXMLHooks.initDatatype
  type resulttype = BPLXMLHooks.resulttype
  structure Parser = Parse
   (structure Dtd = Dtd
    structure Hooks = BPLXMLHooks
    structure ParserOptions = ParserOptions ()
    structure Resolve = ResolveNull)
  fun parse initData uri dtd =
    let
      val dtd =
        case dtd of SOME dtd => dtd | NONE => Dtd.initDtdTables ()
    in
      BPLXMLHooks.getResult
        (Parser.parseDocument
           uri (SOME dtd) (BPLXMLHooks.init (initData, dtd)))
    end
  fun parseFile initData filename =
    parse initData (SOME (Uri.String2Uri ("file:" ^ filename))) NONE
end