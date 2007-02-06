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

(** Debug module whith an integer debug level which is controlled by
 * the Flags module: name="/debug/level", short="d", long="debug-level".
 * @version $LastChangedRevision: 121 $
 *)
 
structure Debug : DEBUG =
struct
  type debug_level = int

  val LVL_USER   = 0
  val LVL_LOW    = 1
  val LVL_MEDIUM = 2
  val LVL_HIGH   = 3

  val level =
      Flags.makeIntFlag{name="/debug/level",default=LVL_USER,
                        short="d",long="debug-level",arg="LEVEL",
                        desc="Set debug level to LEVEL (>= 0)"}

  fun debug_enabled dbg_lvl = dbg_lvl <= (!level)
     
  fun debug dbg_lvl f d =
      if debug_enabled dbg_lvl then
        f d
      else
        ()

end
