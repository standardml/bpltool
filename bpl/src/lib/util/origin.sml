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

(** The origin of some data (e.g. a file and a position in that file).
 * @version $LastChangedRevision$
 *)
 
structure Origin :> ORIGIN where type ppstream = PrettyPrint.ppstream =
struct
  type ppstream = PrettyPrint.ppstream

  datatype pos = NOPOS
               | POS of (int * int) * (int * int)

  datatype origin_dt = UNKNOWN_ORIGIN
                     | FILE_ORIGIN of string * pos

  val unknown_origin = UNKNOWN_ORIGIN

  type origin = origin_dt

  fun mk_file_origin filename pos =
      FILE_ORIGIN (filename, pos)

  fun unmk_origin orig = orig

  fun pp_pos pps NOPOS = ()
    | pp_pos pps (POS ((r1, c1), (r2, c2))) =
      let
	val show = PrettyPrint.add_string pps
      in
        (show ":";
         show (Int.toString r1);
         show ".";
         show (Int.toString c1);
         show "-";
         show (Int.toString r2);
         show ".";
         show (Int.toString c2);
         show ":")
      end

  fun pp indent pps UNKNOWN_ORIGIN = ()
    | pp indent pps (FILE_ORIGIN (filename, pos)) =
      let
	val show = PrettyPrint.add_string pps
	fun <<() = PrettyPrint.begin_block 
		     pps PrettyPrint.INCONSISTENT indent
	fun >>() = PrettyPrint.end_block pps
      in
        (<<();
         show filename;
         pp_pos pps pos;
         >>())
      end

end
