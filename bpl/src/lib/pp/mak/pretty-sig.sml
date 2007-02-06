(*
 *   Copyright 2001      Henning Makholm
 *   Copyright 2001,2002 Henning Niss
 * 
 * Permission is hereby granted, to anyone and free of charge, to deal
 * in this software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the software, provided that
 * all copies or substantial portions of the software are accompanied
 * by the above copyright notice and this permission notice.
 * 
 * The software is provided "as is", without warranty of any kind, express
 * or implied, including but not limited to the warranties of
 * merchantability, fitness for a particular purpose and noninfringement.
 * In no event shall the above listed copyright holder(s) be liable for any
 * claim, damages or other liability, whether in an action of contract,
 * tort or otherwise, arising from, out of or in connection with the
 * software or the use or other dealings in the software.
 *)

signature Pretty =
sig

  include PPengine where type 'a pptree = 'a PPengine.pptree
  include PPbuild  where type 'a pptree' = 'a PPengine.pptree

  type device
  val htmlOutput : string * string -> device
  val xtermOutput : string * string -> device
  val lessOutput : string * string -> device
  val plainOutput : string * string -> device

  datatype style = BOLD | ITALICS | SMALL | NORMAL | LARGE | COLOR of string

  type 'x pp = 'x -> style pptree

  val ppNone: 'a pp   (* never prints anything *)
  val ppStar: 'a pp   (* just prints "*" *)
  val ppUnit: unit pp (* just prints "N/A" *)
  val ppBool: bool pp
  val ppInt: int pp
  val ppOpt: 'a pp -> 'a option pp
  val ppCAnno: 'r pp -> ('r list * 'r list * 'r list) pp
  val ppPair: 'a pp -> 'b pp -> ('a * 'b) pp
  val ppList : 'a pp -> 'a list pp (* with round parantheses and no space *)
(*
  val ppSplaySet : 'a pp -> 'a Splayset.set pp
  val ppSplayMap : 'a pp -> 'b pp -> ('a,'b) Splaymap.dict pp
*)

  val ppBinary : 'a pptree' * string * 'a pptree' -> 'a pptree'

  val ppFold: style pptree -> device -> (string * 'a -> 'a) -> 'a -> 'a
  (* ppFold acts like a foldl over the many little strings that together
   * form the pretty-printed output
   *)
  val ppPrint: style pptree -> device -> TextIO.outstream -> unit
  val ppToString: style pptree -> string

end 
