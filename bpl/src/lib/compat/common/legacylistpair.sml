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

(** ListPair structure matching the LIST_PAIR signature from the
 * new SML Basis Library, but based on the old SML Basis
 * Library ListPair structure.
 * @version  $LastChangedRevision: 102 $
 *)

structure ListPair :> LIST_PAIR = struct
  open ListPair

  exception UnequalLengths

  fun zipEq (xs, ys) =
  let fun zipEq' acc [] [] = List.rev acc
        | zipEq' acc (x::xs) (y::ys) = zipEq' ((x, y) :: acc) xs ys
        | zipEq' _ _ _ = raise UnequalLengths
  in zipEq' [] xs ys end

  fun foldlEq _ r ([], []) = r
    | foldlEq f r (x::xs, y::ys) = foldlEq f (f (x, y, r)) (xs, ys)
    | foldlEq _ _ _ = raise UnequalLengths

  fun foldrEq _ r ([], []) = r
    | foldrEq f r (x::xs, y::ys) = f (x, y, foldrEq f r (xs, ys))
    | foldrEq _ _ _ = raise UnequalLengths

  fun appEq _ ([], []) = ()
    | appEq f (x::xs, y::ys) = (f (x,y); appEq f (xs, ys))
    | appEq _ _ = raise UnequalLengths

  fun mapEq f (xs, ys) =
  let fun mapEq' acc _ [] [] = List.rev acc
        | mapEq' acc f (x::xs) (y::ys) = mapEq' (f (x,y) :: acc) f xs ys
        | mapEq' _ _ _ _ = raise UnequalLengths
  in mapEq' [] f xs ys end

  fun allEq _ ([], []) = true
    | allEq p (x::xs, y::ys) = p(x, y) andalso allEq p (xs, ys)
    | allEq _ _ = false

end (* structure ListPair *)
