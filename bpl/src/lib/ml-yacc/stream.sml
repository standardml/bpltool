(* ML-Yacc Parser Generator (c) 1989 Andrew W. Appel, David R. Tarditi 
 *
 * $Log: stream.sml,v $
 * Revision 1.3  2006/05/01 07:02:36  hniss
 * More oops.
 *
 * Revision 1.1  2005/10/17 07:32:44  hniss
 * Got miniml parser pretty much working.
 *
 * Revision 1.1  2005/05/23 20:57:22  tcd
 * Moving code under BPL/ml - 2nd step - code moved somewhat around - and readded.
 *
 * Revision 1.1  2004/04/26 12:15:31  hniss
 * SML/NJ lib imports.
 *
 * Revision 1.1  1998/10/22 00:11:57  mael
 * see to_do file for bug fixes; created kittester program for external test of the kit
 *
 * Revision 1.2  1997/08/26 19:18:55  jhr
 *   Replaced used of "abstraction" with ":>".
 *
# Revision 1.1.1.1  1997/01/14  01:38:04  george
#   Version 109.24
#
 * Revision 1.1.1.1  1996/01/31  16:01:43  george
 * Version 109
 * 
 *)

(* Stream: a structure implementing a lazy stream.  The signature STREAM
   is found in base.sig *)

structure Stream :> STREAM =
struct
   datatype 'a str = EVAL of 'a * 'a str ref | UNEVAL of (unit->'a)

   type 'a stream = 'a str ref

   fun get(ref(EVAL t)) = t
     | get(s as ref(UNEVAL f)) = 
	    let val t = (f(), ref(UNEVAL f)) in s := EVAL t; t end

   fun streamify f = ref(UNEVAL f)
   fun cons(a,s) = ref(EVAL(a,s))

end;
