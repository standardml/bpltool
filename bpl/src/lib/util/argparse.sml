(*
 *   Copyright 2001 Henning Makholm
 *   Copyright 2001 Henning Niss
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

(* COMMAND-LINE PARSING
 *
 * From the MosML compiler.
 *
 * Author: Peter Sestoft.
 *
 * Changed to rely on the Standard Basis instead of 
 * compiler internals. (Henning Niss)
 *
 * Changed to not require space between options and argument if the
 * option consists only of a dash and a non-dash character. (Henning Makholm)
 *)

structure ArgParse :> ArgParse =
struct

exception Bad of string

datatype spec =
    String  of (string -> unit)
  | Int     of (int -> unit)
  | Unit    of (unit -> unit)
  | Real    of (real -> unit)

datatype error =
    Unknown of string
  | Wrong of string * string * string  (* option, actual, expected *)
  | Missing of string

fun stop error =
  let val progname = CommandLine.name()
      val message =
        case error of
            Unknown s => "unknown option: \"" ^ s ^ "\"."
          | Missing s => "option \"" ^ s ^ "\" needs an argument."
          | Wrong (opt, arg, expected)
              => "wrong argument \"" ^ arg ^ "\"; option \""
                   ^ opt ^ "\" expects " ^ expected ^ "."
  in
     raise Bad (message)
  end;

fun lookup k [] = NONE
  | lookup k ((a, v) :: xs) =
    if k = a then SOME v else lookup k xs

fun parse cmdline speclist anonfun =
  let fun p [] = ()
        | p (s::t) =
            if size s > 1 andalso CharVector.sub(s, 0) = #"-"
            then do_key s NONE t
            else (anonfun s; p t)
      and do_key "--" NONE l = app anonfun l
      	| do_key s optarg l =
        let fun argapply f = case (optarg,l)
			     of (SOME a,l) => (f a; p l)
			      | (NONE,a::l) => (f a; p l)
			      | (NONE,[]) => stop (Missing s)
     	in
          (case lookup s speclist
	   of SOME(Unit f) => (f (); p (case optarg
				  of SOME s => "-" ^ s :: l
				   | NONE => l))
            | SOME(String f) => argapply f
            | SOME(Int f) => argapply
 		       (fn arg => case Int.fromString arg
 		       		  of SOME i => f i
                       		   | NONE
 				     => stop (Wrong (s, arg, "an integer")))
            | SOME(Real f) => argapply
 			(fn arg => case Real.fromString arg
 				   of SOME r => f r
                       		    | NONE
 				      => stop (Wrong (s, arg, "a real")))
	    | NONE => if optarg = NONE andalso
			      	  String.size s > 2 andalso
			      	  String.sub(s,1) <> #"-"
		      then do_key
 		       	   (String.substring(s,0,2))
		       	   (SOME(String.extract(s,2,NONE)))
			   l
		      else stop (Unknown s)
          )
	end
  in
    case (case cmdline of NONE => CommandLine.arguments()
    			| SOME line => line) of
        [] => ()
      | ls => p ls
  end


end (* structure Arg *)
