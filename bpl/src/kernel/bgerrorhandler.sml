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

(** Module for printing friendly error messages, based on exceptions.
 * @version $LastChangedRevision$
 *)
functor BGErrorHandler (structure BgBDNF      : BGBDNF
			  structure BgVal       : BGVAL
			  structure BgTerm      : BGTERM
			  structure Interface   : INTERFACE
			  structure Sugar       : SUGAR 
			  structure PrettyPrint : PRETTYPRINT
			  val pageWidth : int ref
			  val indent : int ref
			  val bgvalinfo2pos 
			      : BgVal.info -> (int * int)
			  sharing type BgVal.bgval = BgBDNF.bgval
			  sharing type Interface.interface =
				       BgVal.interface
			  sharing type PrettyPrint.ppstream =
				       BgBDNF.ppstream =
				       BgVal.ppstream =
				       BgTerm.ppstream =
				       Interface.ppstream
			  sharing type BgBDNF.info =
				       BgVal.info
			  sharing type BgBDNF.bgmatch =
				       BgVal.bgmatch)
	:> BGERRORHANDLER =
struct
type 'class bgbdnf = 'class BgBDNF.bgbdnf
type 'class bgrbdnf = 'class BgBDNF.bgrbdnf
type bgval = BgVal.bgval
type bgterm = BgVal.bgterm
type interface = BgVal.interface
val pp_to_string = PrettyPrint.pp_to_string 
fun bgval2string v = pp_to_string (!pageWidth) (BgVal.pp (!indent)) v
fun bgmatch2string m = pp_to_string (!pageWidth) (BgVal.pp_match (!indent)) m 
fun interface2string i = pp_to_string (!pageWidth) (Interface.pp (!indent)) i
		       

fun explain' (BgVal.NotPrime 
	       (bplmodulefile, v, errortext))
  = ("bgval to be abstracted must be prime:\n  "
       ^ bgval2string v ^ " : "
       ^ interface2string (BgVal.innerface v) ^ " -> "
       ^ interface2string (BgVal.outerface v) ^ "\n"
       ^ "(Error detected in " ^ bplmodulefile
       ^ ": " ^ errortext ^ ")")
  | explain' (BgVal.NameMissing 
	       (bplmodulefile, v, errortext))
  = ("bgval abstraction failed because a name is missing in the outer face:\n  v = "
       ^ bgval2string v ^ " : "
       ^ interface2string (BgVal.innerface v) ^ " -> "
       ^ interface2string (BgVal.outerface v) ^ "\n"
       ^ "(Error detected in " ^ bplmodulefile
       ^ ": " ^ errortext ^ ")")
  | explain' (BgVal.NotComposable 
	       (bplmodulefile, v1, v2, errortext))
  = ("bgvals are not composable:\n  v1 = "
       ^ bgval2string v1 ^ " : "
       ^ interface2string (BgVal.innerface v1) ^ " -> "
       ^ interface2string (BgVal.outerface v1) ^ "\n  v2 = "
       ^ bgval2string v2 ^ " : "
       ^ interface2string (BgVal.innerface v2) ^ " -> "
       ^ interface2string (BgVal.outerface v2) ^ "\n"
       ^ "(Error detected in " ^ bplmodulefile
       ^ ": " ^ errortext ^ ")")
  | explain' (BgVal.NotTensorable 
	       (bplmodulefile, vs, errortext))
  = ("bgvals are not tensorable, due to inner or outer name clash:\n" ^
       foldr
	 (fn (v, s) =>
	     "  v = "
	     ^ bgval2string v ^ " : "
	     ^ interface2string (BgVal.innerface v) ^ " -> "
	     ^ interface2string (BgVal.outerface v) ^ "\n" ^ s)
	 ""
	 vs
       ^ "(Error detected in " ^ bplmodulefile
       ^ ": " ^ errortext ^ ")")
  | explain' (BgVal.NotParallelisable
	       (bplmodulefile, vs, errortext))
  = ("cannot make the parallel product of bgvals, \
     \due to outer local or inner name clash:\n" ^
       foldr
	 (fn (v, s) =>
	     "  v = "
	     ^ bgval2string v ^ " : "
	     ^ interface2string (BgVal.innerface v) ^ " -> "
	     ^ interface2string (BgVal.outerface v) ^ "\n" ^ s)
	 ""
	 vs
       ^ "(Error detected in " ^ bplmodulefile
       ^ ": " ^ errortext ^ ")")
  | explain' (BgVal.NotPrimeable
	       (bplmodulefile, vs, errortext))
  = ("cannot make the prime product of bgvals, \
     \due to outer local or inner name clash, or nonlocal inner faces:\n" ^
       foldr
	 (fn (v, s) =>
	     "  v = "
	     ^ bgval2string v ^ " : "
	     ^ interface2string (BgVal.innerface v) ^ " -> "
	     ^ interface2string (BgVal.outerface v) ^ "\n" ^ s)
	 ""
	 vs
       ^ "(Error detected in " ^ bplmodulefile
       ^ ": " ^ errortext ^ ")")
  | explain' (BgBDNF.IrregularBDNF (bplmodulefile, i, b, errortext))
  = ("bigraph is not regular:\n  "
       ^ bgval2string b ^ "\n"
       ^ "(Error detected in " ^ bplmodulefile
       ^ ": " ^ errortext ^ ")")
  | explain' (BgBDNF.MalformedBDNF (bplmodulefile, i, m, errortext))
  = ("bgval is not on an appropriate BDNF form:\n  "
       ^ bgmatch2string m ^ "\n"
       ^ "(Error detected in " ^ bplmodulefile
       ^ ": " ^ errortext ^ ")")
  | explain' (BgBDNF.MalformedRBDNF (bplmodulefile, i, m, errortext))
  = ("bgval is not on an appropriate RBDNF form:\n  "
       ^ bgmatch2string m ^ "\n"
       ^ "(Error detected in " ^ bplmodulefile
       ^ ": " ^ errortext ^ ")")
  | explain' (BgBDNF.UnequalLength (bplmodulefile, vs1, vs2, errortext))
  = ("bgval lists of unequal length:\n  vs1 = ["
     ^ #2 (foldr (fn (v, (notlast, s))
		     => (true, 
			 bgval2string v 
			 ^ (if notlast then ", " else "") ^ s))
		 (false, 
		  "]\n  vs2 = ["
		  ^ #2 (foldr (fn (v, (notlast, s))
				  => (true, 
				      bgval2string v 
				      ^ (if notlast then ", " else "") ^ s))
			      (false, 
			       "]\n" ^ "(Error detected in " ^ bplmodulefile
			       ^ ": " ^ errortext ^ ")")
			      vs2))
		 vs1))
  | explain' (Sugar.DuplicateName (bplmodulefile, K, ys, Xs, errortext))
  = 
  let
    fun list2str f xs s =
	"[" ^ #2 (foldr (fn (y, (notlast, s))
			    => (true, if notlast then
					f y ^ ", " ^ s
				      else
					f y ^ s)) (false, "]" ^ s) xs)
  in
    ("duplicate name in ion: " ^ K
       ^ list2str (fn s => s) ys 
		  (list2str (fn X => list2str (fn s => s) X "")
			    Xs
			    ("\n(Error detected in " ^ bplmodulefile
			     ^ ": " ^ errortext ^ ")")))
  end
  | explain' (Sugar.WrongArity errortext)
  = errortext
  | explain' unknownError = raise unknownError

fun explain (e as (BgVal.NotPrime (bplmodulefile, v, errortext)))
  = ErrorMsg.error 
      (#1 (bgvalinfo2pos (BgVal.info v)))
      (#2 (bgvalinfo2pos (BgVal.info v)))
      (explain' e)
  | explain (e as (BgVal.NameMissing (bplmodulefile, v, errortext)))
  = ErrorMsg.error 
      (#1 (bgvalinfo2pos (BgVal.info v)))
      (#2 (bgvalinfo2pos (BgVal.info v)))
      (explain' e)
  | explain 
      (e as (BgVal.NotComposable (bplmodulefile, v1, v2, errortext)))
  = ErrorMsg.error 
      (#1 (bgvalinfo2pos (BgVal.info v1)))
      (#2 (bgvalinfo2pos (BgVal.info v2)))
      (explain' e)
  | explain (e as (BgVal.NotTensorable (bplmodulefile, vs, errortext)))
  = ErrorMsg.error 
      (#1 (bgvalinfo2pos (BgVal.info (hd vs))))
      (#2 (bgvalinfo2pos (BgVal.info (List.last vs))))
      (explain' e)
  | explain (e as (BgVal.NotParallelisable (bplmodulefile, vs, errortext)))
  = ErrorMsg.error 
      (#1 (bgvalinfo2pos (BgVal.info (hd vs))))
      (#2 (bgvalinfo2pos (BgVal.info (List.last vs))))
      (explain' e)
  | explain (e as (BgVal.NotPrimeable (bplmodulefile, vs, errortext)))
  = ErrorMsg.error 
      (#1 (bgvalinfo2pos (BgVal.info (hd vs))))
      (#2 (bgvalinfo2pos (BgVal.info (List.last vs))))
      (explain' e)
  | explain (e as (BgBDNF.MalformedBDNF (bplmodulefile, i, m, errortext)))
  = ErrorMsg.error 
      (#1 (bgvalinfo2pos i))
      (#2 (bgvalinfo2pos i))
      (explain' e)
  | explain (e as (BgBDNF.UnequalLength (bplmodulefile, vs1, vs2, errortext)))
  = ErrorMsg.error 
      (if null vs1 then 0 else #1 (bgvalinfo2pos (BgVal.info (hd vs1))))
      (if null vs2 then 0 else #2 (bgvalinfo2pos (BgVal.info (List.last vs2))))
      (explain' e)
  | explain 
      (e as (Sugar.DuplicateName (bplmodulefile, K, ys, Xs, errortext)))
  = ErrorMsg.error 0 0 (explain' e)
  | explain (e as (Sugar.WrongArity (errortext)))
  = ErrorMsg.error 0 0 (explain' e)
  | explain unknownError = raise unknownError
end
