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
 * @version $LastChangedRevision: 442 $
 * Modified: $Date: 2006/09/04 21:48:46 $ by: $Author: hniss $
 *)

signature BPLXMLHOOKS =
sig
  include Hooks
  (** The type of the initial data needed for parsing. *)
  type initDatatype
	(** The type of the result of parsing. *)
  type resulttype
  (** The initial data structure used for parsing. *)
  val init : initDatatype -> AppData
  (** Return the result of the parsing. *)
  val getResult : AppFinal -> resulttype
end
