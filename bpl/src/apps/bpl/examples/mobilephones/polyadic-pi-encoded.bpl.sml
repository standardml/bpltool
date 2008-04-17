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
 
(* The Polyadic Pi Calculus modelled in bigraphs using an encoding
 * to avoid the need of an unbounded number of rules.
 *)

OS.FileSys.chDir "../..";
use "smlnj.sml";

(* Polyadic pi calculus reaction rule controls *)
val Sum   = passive0 ("Sum")
val Send  = passive ("Send" -: 1)
val Get   = passive ("Get" -: 1)
val Reacting  = passive0 ("Reacting") (* React token for loop *)
val SendName  = passive ("SendName" -: 1)
val GetName   = passive ("GetName" =: 1 --> 0)

(* System link names *)
val ( x,  y,  z )
  = ("x","y","z")

(* Polyadic pi calculus reaction rules for sending names *)
val REACTinit = "REACTinit" :::
  Sum o merge(2) o
    (Send[x] o idp(1) ||
     idp(1)) `|`
  Sum o merge(2) o
    (Get[x] o idp(1) ||
     idp(1))
  --[0 |-> 0, 1 |-> 2]--|>
  Reacting o merge(2) o
    (Send[x] o idp(1) ||
     Get[x] o idp(1)) handle e => explain e;

val REACTstop = "REACTstop" :::
  Reacting o merge(2) o
    (Send[x] o idp(1) ||
     Get[x] o idp(1))
  --[0 |-> 0, 1 |-> 1]--|>
  x//[] * idp(1) `|` idp(1) handle e => explain e;

val REACTloop = "REACTloop" :::
  Reacting o merge(2) o
    (Send[x] o SendName[y] o idp(1) ||
     Get[x] o GetName[][[z]] o (<[z]> `[z]`))
  --[0 |-> 0, 1 |-> 1]--|>
  Reacting o merge(2) o
    (Send[x] o idp(1) ||
     Get[x] o `[y]`) handle e => explain e;
