(* Copyright (c) 2008  The BPL Group at the IT University of Copenhagen
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
 
 (** Basic bigraph definitions to play around with.
 *)

OS.FileSys.chDir "../..";
use "smlnj.sml";

Flags.setIntFlag "/debug/level" 10;
SMLofNJ.Internals.GC.messages false;
use_shorthands true;

val (x, x1, x2, x3) = ("x","x1","x2","x3")
val (y, y1, y2, y3) = ("y","y1","y2","y3")
val (z, z1, z2, z3) = ("z","z1","z2","z3")


val K   = active0("K")
val K1  = active("K1" -: 1)
val K2  = active("K2" -: 2)
val K10 = active("K10" =: 1 --> 0)
val K20 = active("K20" =: 2 --> 0)
val K11 = active("K11" =: 1 --> 1)
val K21 = active("K21" =: 2 --> 1)
val K12 = active("K12" =: 1 --> 2)
val M   = atomic0("M");
val M1  = atomic("M1"  -: 1);
val M2  = atomic("M2"  -: 2);


