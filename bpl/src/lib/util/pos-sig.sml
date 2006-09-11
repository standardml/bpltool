(* Position info added by the BPL parser
 * (c) 2004  The BPL Group, IT University of Copenhagen
 *)

(* Signature for position info added by the parser.
 *
 * Pos.posinfo data should be read: 
 * (filename, start - (line, column), end - (line,column)) 
 *)
signature POS =
sig
    type posinfo

    val mkPos : string -> int * int -> int * int -> posinfo
    val noInfo : posinfo

    val getPos : posinfo -> (string * (int * int) * ( int * int )) option
end