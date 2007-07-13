(* Copyright (c) 2007  The BPL Group at the IT University of Copenhagen
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
 
(* The Pi Calculus modelled in bigraphs. *)

OS.FileSys.chDir "..";
use "smlnj.sml";

(* Pi calculus reaction rule controls *)
val Sum  = passive0 ("Sum")
fun Send n = passive ("Send" ^ Int.toString n -: 1 + n)
fun Get n  = passive ("Get" ^ Int.toString n =: n --> 1)

(* Polyadic pi calculus reaction rule sending n names. *)
fun REACT n =
  let
    val x = "x"
    val ys = List.tabulate (n, fn i => "y" ^ Int.toString i)
    val zs = List.tabulate (n, fn i => "z" ^ Int.toString i)
    val yzs = ListPair.zip (ys, zs)
  in
    {redex = Sum o (Send(n)(x :: ys)                `|` idp(1))
         `|` Sum o (Get(n)[x](map (fn z => [z]) zs) `|` idp(1)),
     react =
       foldr (fn ((y, z), product) => y/z * product) (x//[] * idp(1)) yzs
       o (idp(1) `|` `zs`),
     inst  = [0 |-> 0, 1 |-> 2]}
  end