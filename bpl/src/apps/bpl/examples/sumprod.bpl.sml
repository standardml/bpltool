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
 
(* Sum and product example. *)

OS.FileSys.chDir "..";
use "smlnj.sml";
Flags.setIntFlag "/debug/level" 10;
SMLofNJ.Internals.GC.messages false;
use_shorthands true;

(* Natural numbers represented by nesting zero within successors *)
val Z = atomic     ("Z" -: 1)
val S = active0    ("S")

(* Term containers *)
val Num  = active  ("Num" -: 1)
val Sum  = active0 ("Sum")
val Prod = active0 ("Prod")

(* Transient containers used during computations  *)
val Add  = passive ("Add" -: 2)   (* Addend       *)
val Lier = passive0("Lier")       (* Multiplier   *)
val Cand = passive ("Cand" -: 1)  (* Multiplicand *)

(* Variable names *)
val ( a , b , c , d )
  = ("a","b","c","d")

(* Rules for computing the sum of two numbers. 
 * Two Num nodes in a Sum node will be replaced by a Num node
 * containing their sum.
 *)
val SumInit = "SumInit" :::
  Sum o (Num[a] o `[a]` `|` -//[b] o (Num[b] o `[b]`))
  ----|>
  Sum o (Num[a] o `[a]` `|` -//[b] o (Add[a,b] o `[b]`))

val SumDo = "SumDo" :::
  Z[a] || -//[b] o (Add[a,b] o `[b]`)
  ----|>
  `[a]` * <->

val SumEnd = "SumEnd" :::
  Sum o (Num[a] o `[a]`)
  ----|>
  Num[a] o `[a]`

(* Rules for computing the product of two numbers. 
 * Two Num nodes in a Prod node will be replaced by a Num node
 * containing their product.
 *)
val ProdInit = "ProdInit" :::
  Prod o (Num[a] o `[a]` `|` Num[b] o `[b]`)
  ----|>
  Prod o
    (Lier o `[a]`  `|` Cand[b] o `[b]` `|` -//[c] o (Num[c] o Z[c]))

val ProdStep = "ProdStep" :::
  Prod o (Lier o S o `[a]`  `|` Cand[b] o `[b]` `|` Num[c] o `[c]`)
  --[3&[d] |--> 1&[b]]--|>
  Prod o (Lier o `[a]`      `|` Cand[b] o `[b]` `|`
          Sum o (Num[c] o `[c]` `|` -//[d] o (Num[d] o `[d]`)))

val ProdEnd = "ProdEnd" :::
  Prod o (Lier o Z[a]  `|` Cand[b] o `[b]` `|` Num[c] o `[c]`)
  --[0 |-> 1]--|>
  Num[c] o `[c]` * a//[] * b//[]

(* Sum rules *)
val sumrules = [SumInit, SumDo, SumEnd]

(* Product rules *)
val prodrules = [ProdInit, ProdStep, ProdEnd]

(* Collection of all rules *)
val rules = mkrules (List.@ (sumrules, prodrules))

(* Tactics *)
val TAC_sum =
  react_rule "SumInit" ++ react_rule "SumDo" ++ react_rule "SumEnd"

val TAC_prod =
  react_rule "ProdInit" ++
  REPEAT (react_rule "ProdStep") ++
  react_rule "ProdEnd"

(* 2 + 0 *)
val two_plus_zero =
  Sum o
    (-//[a] o (Num[a] o S o S o Z[a]) `|` -//[b] o (Num[b] o Z[b]))

(* 2 + 1 *)
val two_plus_one = Sum o (Num[a] o S o S o Z[a] `|` Num[b] o S o Z[b])

(* 2 x 0 *)
val two_x_zero = Prod o (Num[a] o S o S o Z[a] `|` Num[b] o Z[b])

(* 2 x 3 *)
val two_x_zero =
  Prod o (Num[a] o S o S o Z[a] `|` Num[b] o S o S o S o Z[b])
