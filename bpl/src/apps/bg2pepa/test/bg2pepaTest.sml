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

(** Testing module for Bigraphs2PEPA stuff.
 * @version $LastChangedRevision: 520 $
 *)

functor Bg2PEPATest(
  structure Assert  : ASSERT
  structure Test    : TEST
  structure ErrorHandler : ERRORHANDLER
  where type ppstream    = PrettyPrint.ppstream
    and type break_style = PrettyPrint.break_style
    and type origin      = Origin.origin) =
struct
		
open Assert

  structure BGADT : BG_ADT
    = BGADT (structure ErrorHandler = ErrorHandler)

  structure PCRADT :> PCR_ADT = PCRADT (structure BGADT = BGADT)

  val suite = (fn () => Test.labelTests [])
end
