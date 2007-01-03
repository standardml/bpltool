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

structure Partition :> PARTITION =
struct
  type 'a partition = unit

  exception NoPartitions

  fun make _ _ = ()

  fun next _ = raise NoPartitions

  open Array
  infix 8 sub;

  fun setpart n =
      let
        val c = array (n + 1, 1)
        val b = array (n + 1, 1)

        fun print_partition () =
            (foldl (fn (e, first) => if first then false else (print (Int.toString e); false)) true c;
             print "\n")

        fun while' r j =
            if r < n - 1 then
              let
                val r = r + 1
                val _ = update (c, r, 1)
                val j = j + 1
                val _ = update (b, j, r)
               in
                 while' r j
               end
            else j

        fun for i nj =
            if i <= nj then
              (update (c, n, i);
               print_partition ();
               for (i + 1) nj)
            else ()

        fun repeat false 1 j = ()
          | repeat _     r j =
            let
              val j = while' r j
              val _ = for 1 (n - j)
              val r = b sub j
              val _ = update (c, r, c sub r + 1)
              val j = if (c sub r > r - j) then j - 1 else j
            in
              repeat false r j
            end
      in
        repeat true 1 0
      end
end

