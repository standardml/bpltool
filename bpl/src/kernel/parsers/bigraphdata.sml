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

(** Data structure for holding a bigraph or a BRS.
 * @version $LastChangedRevision$
 * Modified: $Date: 2006/09/04 21:48:46 $ by: $Author: hniss $
 *)
functor BigraphData (
  structure Info : INFO
  structure Name : NAME
  structure Control : CONTROL
  structure BgTerm : BGTERM
) : BIGRAPHDATA =
struct
  type info = Info.info
  type name = Name.name
  type control = Control.control
  type bgterm = BgTerm.bgterm
  datatype bigraphdata =
    BRS of {
      name : string,
      redex : bgterm,
      react : bgterm,
      maps : ((int * name list) * (int * name list)) list,
      info : info} list
  | BIGRAPH of bgterm
end  
 