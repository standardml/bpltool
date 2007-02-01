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

(* Hack to make the Compilation Manager available for different versions of SML/NJ. *)

(* Insert the following in the files where CM_make should be defined:

val cur_dir = OS.FileSys.getDir ();
val _ = OS.FileSys.chDir "relative/path/to/lib/compat/smlnj/";
use "cmmakedefine.sml";
val _ = OS.FileSys.chDir cur_dir;

*)

case Compiler.version of
  {version_id = major_v::minor_v::vtail, ...} =>
    if major_v >= 110 andalso minor_v >= 20 then
      use "cmmakenew.sml"
    else
      use "cmmakeold.sml"
| _ => use "cmmakeold.sml"
