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

(** Callback functions for SAX parsing a BPL BRS or bigraph expressed in XML.
 * @version $LastChangedRevision: 442 $
 * Modified: $Date: 2006/09/04 21:48:46 $ by: $Author: hniss $
 *)

functor BigraphXMLHooks (
  structure Info : INFO
  structure Control : CONTROL
  structure BigraphData : BIGRAPHDATA
  structure BgTerm : BGTERM
  sharing type Info.info = BgTerm.info
  sharing type Control.control = BigraphData.control
  sharing type BgTerm.bgterm = BigraphData.bgterm) :> BPLXMLHOOKS
	where type initDatatype = Control.control list
	  and type resulttype = BigraphData.bigraphdata
	   =
struct
  open IgnoreHooks
  type initDatatype = Control.control list
  type resulttype = BigraphData.bigraphdata
  fun init ss = ()
  fun getResult appFinal =
    BigraphData.BIGRAPH (BgTerm.Mer (0, Info.noinfo))
end
