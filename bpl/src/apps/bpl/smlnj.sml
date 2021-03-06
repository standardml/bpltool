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

(* Get the right CM.make command for the current version of SML/NJ *)
val cur_dir = OS.FileSys.getDir ();
val _ = OS.FileSys.chDir "../../lib/compat/smlnj/";
use "cmmakedefine.sml";
val _ = OS.FileSys.chDir cur_dir;

CM_make "kernelsources.cm";

use "bpl.sml";

local
  fun ipp typename pp
    = Compiler.PPTable.install_pp
        typename
        (pp (Flags.getIntFlag "/misc/indent"))
  val _ = ipp ["BG","BgVal","bgval"] BG.BgVal.ppWithIface
  val _ = ipp ["BG","Match","derivation"] BG.Match.ppTree
  val _ = ipp ["BG","Sugar","mapinfo"] BG.Sugar.ppMapinfo
  val _ = ipp ["BG","Rule","rule"] BG.Rule.pp
in
 val _ = print 
  	("\nBPL (revision " ^ BPL_REVISION_STR 
  	 ^ ") interactive prompt.  Type 'help[];' for help.\n")
end
