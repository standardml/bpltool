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

(** Declarations that open modules at top level for experimenting with
 * bigraph operations at the interactive command prompt of SMLNJ.
 *
 * To get started, start SMLNJ and enter <code>use "apps/smlnj.sml";</code>
 *
 * @version $LastChangedRevision: 315 
 *)

CM.make' "../../kernel/sources.cm";

use "bpl.sml";

local
  fun ipp typename pp
    = Compiler.PPTable.install_pp
        typename
        (pp (Flags.getIntFlag "/misc/indent"))
  val _ = ipp ["BG","BgVal","bgval"] BG.BgVal.ppWithIface
  val BPL_REVISION_STR
    = String.extract ("$LastChangedRevision: 315", 22, NONE)
in
 val _ = print 
  	("\nBPL (revision " ^ BPL_REVISION_STR 
  	 ^ ") interactive prompt.  Type 'help[];' for help.\n")
end
