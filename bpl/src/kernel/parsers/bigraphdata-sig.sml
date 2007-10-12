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
 * @version $LastChangedRevision: 442 $
 * Modified: $Date: 2006/09/04 21:48:46 $ by: $Author: hniss $
 *)
signature BIGRAPHDATA =
sig
  type info
  type name
  type control
  type bgterm
  
  (** Data type for holding either a bigraph or a BRS. *)
  datatype bigraphdata =
    BRS of {
      name : string,
      redex : bgterm,
      react : bgterm,
      maps : ((int * name list) * (int * name list)) list,
      info : info} list
  | BIGRAPH of bgterm
end  
 