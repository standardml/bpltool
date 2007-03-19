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

(* Implementation of list partitioning based on
 *
 * B. Djokic et al. 
 * "A Fast Iterative Algorithm for Generating Set Partitions"
 * 
 * the original (a few bugs fixed):
 * 
 * program setpart2(n);
 *   begin
 *     r := 1;  c[1] := 1;  j := 0;  b[0] := 1;
 *     n1 := n - 1;
 *     repeat
 *       while r < n1 do begin
 *         r    := r + 1;
 *         c[r] := 1;
 *         j    := j + 1;
 *         b[j] := r
 *       end;
 *       for i := 1 to n - j do begin
 *         c[n] := i;
 *         // partition is given by c[1] ... c[n]
 *       end;
 *       r    := b[j];
 *       c[r] := c[r] + 1;
 *       if c[j] > r - j then j := j - 1
 *     until r = 1
 *   end;
 *
 * Our implementation extends the above, by adding a parameter, m,
 * which gives the required number of groups. It uses the setpart2
 * to generate partitions and
 *
 *   (1) adds empty groups if necessary
 *   (2) discards partitions with too many groups
 *)
functor Partition (structure LazyList : LAZYLIST) :> PARTITION =
struct
  open LazyList
  val sub    = Array.sub  infix 8 sub
  val array  = Array.array
  val update = Array.update
  val modify = Array.modify

  type 'a partitiongen = 'a list list lazylist ref

  exception NoPartitions

  fun setpart2 list n m =
      let
        (* c[i] is the group wich element i belongs to (starting from 1). *)
        val c = array (n + 1, 1)
        (* b[j] is the position where the current position, r,
           should backtrack to. *)
        val b = array (n + 1, 1)

        (* array for the m groups *)
        val p = array (m, [])

        (* Creates an 'a list list from the current state of c. *)
        fun make_partition () =
          let
            (* set all groups to the empty list *)
            val () = modify (fn _ => []) p
            (* insert element e into group g of partition p *)
            fun insert e g = update (p, g - 1, e :: (p sub (g - 1)))
          in
            (* insert each element into the group specified by c *)
            (foldr (fn (e, i) => (insert e (c sub i); i - 1)) n list;
             (* return the list *)
             Array.foldr (fn (g, p) => g :: p) [] p)
          end

        (* "functional" implementation of the setpart2 while-loop.
         *
         * no changes
         *)
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

        (* "functional" implementation of the setpart2 for-loop.
         * It succesively places element n in each of the groups
         * i to min(n-j,m).
         *
         * changes:
         *   - inserted check, that no element is placed groups > m (n_j <= m + 1)
         *   - limit changed from n-j to min(n-j,m).
         *)
        fun for i n_j =
            lzmake (fn () =>
            if n_j <= m + 1 andalso i <= n_j andalso i <= m then
              (update (c, n, i);
               Cons (
                 make_partition (),
                 for (i + 1) n_j))
            else Nil)

        (* "functional" implementation of the setpart2 for-loop.
         * the bool argument is used to distinguish between the first (true)
         * and succesive calls (false).
         *
         * no changes
         *)
        fun repeat false 1 j = lzNil
          | repeat _     r j =
            let
              val j  = while' r j
              val ps = for 1 (n - j)
            in
              lzappend
                ps
                (lzmake (fn () =>
                 let
                   val r  = b sub j
                   val () = update (c, r, c sub r + 1)
                   val j  = if (c sub r > r - j) then j - 1 else j
                 in
                   lzunmk (repeat false r j)
                 end))
            end
      in
        repeat true 1 0
      end

  fun make list m = 
      let
        val n = length list
      in
        if m < 0 orelse (m = 0 andalso n > 0) then
          raise NoPartitions
        else if n = 0 then
          ref (lzmake (fn () => Cons (List.tabulate (m, fn _ => []), lzNil)))
        else
          ref (setpart2 list n m)
      end

  fun next pg =
      (case lzunmk (!pg) of
         Nil    => raise NoPartitions
       | Cons (r, g) => (pg := g; r))
end

