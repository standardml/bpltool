(*
 18/5-2006, Ebbe Elsborg
*)

fun mkprs x [] = []
  | mkprs x (c::cs) = (x,c) :: mkprs x cs

fun mkallprs clist = foldr (fn (c,l) => (mkprs c clist)@l) [] clist

fun mkalltriples [] = []
  | mkalltriples ((p1,p2)::ps) =
    if p1=p2
    then (p1,p2,true) :: mkalltriples ps
    else (p1,p2,false) :: mkalltriples ps

fun mkstr (p1,p2,b) =
    if b
    then "      | " ^ "(" ^ p1 ^ "," ^ p2 ^ ") " ^ "=> " ^ "True" ^ "\n"
    else "      | " ^ "(" ^ p1 ^ "," ^ p2 ^ ") " ^ "=> " ^ "False" ^ "\n"

fun mkstrlist tlist = map mkstr tlist

fun mkoutstr [] = "\n"
  | mkoutstr (s::sr) = s ^ (mkoutstr sr)

(*
val is = TextIO.openIn "chareq.in"
val input = TextIO.inputLine is
(TextIO.closeIn is;
*)
val input = explode "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
val input' = map (fn c => String.str c) input
val outstring = mkoutstr (mkstrlist (mkalltriples (mkallprs input')))

val os = TextIO.openOut "chareq.out";
TextIO.output(os,outstring);
TextIO.flushOut os;
TextIO.closeOut os
