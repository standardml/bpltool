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

(* Implementation of list partitioning based on [FIXME] *)
functor Partition (structure LazyList : LAZYLIST) :> PARTITION =
struct
  open LazyList
  val sub    = Array.sub  infix 8 sub
  val array  = Array.array
  val update = Array.update

  type 'a partitiongen = 'a list list lazylist

  exception NoPartitions

  fun setpart2 list n m =
      lzmake (fn () =>
      let
        val c = array (n + 1, 1)
        val b = array (n + 1, 1)

        (* m' is the number of non-empty groups *)
        fun make_partition m' =
	    let
              val partition = List.tabulate (m, fn _ => [])

              (* insert element e into group g of partition p *)
	      fun insert e g p =
                  #2 (foldr (fn (group, (g', p)) =>
                                if g = g' then (0, (e::group)::p)
                                else           (g' - 1, group::p))
                            (m,[]) p)
                  
            in
              #2 (foldr (fn (e, (i, p)) =>
                            (i - 1, insert e (c sub i) p))
                        (n, partition) list)
            end

        fun print_partition () =
            (Array.foldl (fn (e, first) =>
                             if first then
                               false
                             else
                               (print (Int.toString e); false))
                         true c;
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
            lzmake (fn () =>
            if i <= nj andalso i <= m then
              (update (c, n, i);
               Cons (
                 (print_partition ();
                 if i < nj then make_partition (nj - 1)
                           else make_partition nj),
                 for (i + 1) nj))
            else Nil)

        fun repeat false 1 j = lzNil
          | repeat _     r j =
            let
              val j  = while' r j
              val ps = for 1 (n - j)
            in
              lzappend
                ps
                (lzmake (fn () =>
                 if c sub r < m then
                   let
                     val r = b sub j
                     val _ = update (c, r, c sub r + 1)
                     val j = if (c sub r > r - j) then j - 1 else j
                   in
                     lzunmk (repeat false r j)
                   end
                 else
                   Nil))
            end
      in
        lzunmk (repeat true 1 0)
      end)

  fun make list m = 
      let
        val n = length list
      in
        if m < 0 orelse (m = 0 andalso n > 0) then
          raise NoPartitions
        else if n = 0 then
          lzmake (fn () => Cons ([], lzNil))
        else
          setpart2 list n m
      end

  fun next pg =
      (case lzunmk pg of
         Nil    => raise NoPartitions
       | Cons r => r)
end

