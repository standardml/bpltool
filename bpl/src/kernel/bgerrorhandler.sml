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
			structure Ion         : ION
			structure Permutation : PERMUTATION
			structure Name        : NAME
			structure NameSet     : MONO_SET
			structure Sugar       : SUGAR 
			structure PrettyPrint : PRETTYPRINT
			val pageWidth : int ref
			val indent : int ref
			val bgvalinfo2pos 
			    : BgVal.info -> (int * int)
			sharing type BgVal.bgval = BgBDNF.bgval
			sharing type Name.name =
                                     NameSet.elt =
			             Ion.name =
				     BgVal.name
			sharing type NameSet.Set =
			             BgVal.nameset =
                                     Permutation.nameset
		        sharing type Interface.interface =
			             BgVal.interface =
			             Permutation.interface
			sharing type PrettyPrint.ppstream =
				     BgBDNF.ppstream =
				     BgVal.ppstream =
				     BgTerm.ppstream =
				     Interface.ppstream =
				     Permutation.ppstream
			sharing type BgBDNF.info =
				     BgVal.info
			sharing type BgBDNF.bgmatch =
				     BgVal.bgmatch)
	:> BGERRORHANDLER =
struct
type 'class bgbdnf = 'class BgBDNF.bgbdnf
type bgval = BgVal.bgval
type bgterm = BgVal.bgterm
type interface = BgVal.interface
val pp_to_string = PrettyPrint.pp_to_string 
fun bgval2string v = pp_to_string (!pageWidth) (BgVal.pp (!indent)) v
fun bgmatch2string m = pp_to_string (!pageWidth) (BgVal.pp_match (!indent)) m 
fun interface2string i = pp_to_string (!pageWidth) (Interface.pp (!indent)) i
fun permutation2string p = pp_to_string (!pageWidth) (Permutation.pp (!indent)) p

(* Utility function for formatting sets:
 *
 * fst    first character
 * lst    last character
 * del    delimiter
 * e2s    funtion that maps elements to strings
 * fold   folding function
 * set    the set to be formatted
 *)
fun set2string fst lst del e2s fold set =
    fst ^ #2 (fold
	        (fn e => fn (notlast, str) =>
	            (true, if notlast then
	                     e2s e ^ del ^ str
		           else
		             e2s e ^ str))
                (false, lst) set : bool * string)
(* Function for formatting lists *)
fun list2string fst lst del e2s l =
    set2string fst lst del e2s (fn f => foldr (fn (e, b) => f e b)) l
(* Function for formatting NameSets *)
fun nset2string nset =
    set2string "{" "}" ", " Name.unmk NameSet.fold nset
		       

fun explain' (BgVal.NotPrime 
	       (bplmodulefile, v, errortext))
  = ("bgval to be abstracted must be prime:\n  "
       ^ bgval2string v ^ " : "
       ^ interface2string (BgVal.innerface v) ^ " -> "
       ^ interface2string (BgVal.outerface v) ^ "\n"
       ^ "(Error detected in " ^ bplmodulefile
       ^ ": " ^ errortext ^ ")")
  | explain' (BgVal.DuplicateNames 
	       (bplmodulefile, i, nss, errortext))
  = ("duplicate names:\n  ns = "
       ^ list2string "{" "}" ", " (list2string "{" "}" ", " Name.unmk) nss
       ^ " : "
       ^ "(Error detected in " ^ bplmodulefile
       ^ ": " ^ errortext ^ ")")
  | explain' (BgVal.NameClash 
	       (bplmodulefile, i, ns1, ns2, errortext))
  = ("name clash:\n  ns1 = "
       ^ (set2string "{" "}" ", " Name.unmk NameSet.fold ns1)
       ^ "  ns2 = "
       ^ (set2string "{" "}" ", " Name.unmk NameSet.fold ns2)
       ^ " : "
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
  | explain' (BgVal.NotImplemented 
	       (bplmodulefile, v, errortext))
  = ("feature not implemented:\n  v = "
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
  | explain' (BgBDNF.LogicalError (bplmodulefile, errortext))
  = ("an internal error occured:\n  "
       ^ "(Error detected in " ^ bplmodulefile
       ^ ": " ^ errortext ^ ")")
  | explain' (Sugar.DuplicateName (bplmodulefile, K, ys, Xs, errortext))
  = 
  let
    fun list2string f xs s =
	"[" ^ #2 (foldr (fn (y, (notlast, s))
			    => (true, if notlast then
					f y ^ ", " ^ s
				      else
					f y ^ s)) (false, "]" ^ s) xs)
  in
    ("duplicate name in ion: " ^ K
       ^ list2string (fn s => s) ys 
		  (list2string (fn X => list2string (fn s => s) X "")
			    Xs
			    ("\n(Error detected in " ^ bplmodulefile
			     ^ ": " ^ errortext ^ ")")))
  end
  | explain' (Permutation.LogicalError (bplmodulefile, errortext))
  = ("logical error detected in " ^ bplmodulefile ^ ": " ^ errortext)
  | explain' (Permutation.NotPermutation (bplmodulefile, p))
  = ("not a permutation:  p = "
       ^ list2string "[" "]" ", "
           (fn (i, ns) =>
               if NameSet.isEmpty ns then
                 Int.toString i
               else
                 Int.toString i ^ nset2string ns)
           p
       ^ "\n(Error detected in " ^ bplmodulefile ^ ")")
  | explain' (Permutation.Uncomposable (bplmodulefile, p1, p2, errortext))
  = ("permutations are not composable:\n  p1 = "
       ^ permutation2string p1 ^ " : "
       ^ interface2string (Permutation.innerface p1) ^ " -> "
       ^ interface2string (Permutation.outerface p1) ^ "\n  p2 = "
       ^ permutation2string p2 ^ " : "
       ^ interface2string (Permutation.innerface p2) ^ " -> "
       ^ interface2string (Permutation.outerface p2) ^ "\n"
       ^ "\n(Error detected in " ^ bplmodulefile
       ^ ": " ^ errortext ^ ")")
  | explain' (Permutation.NotRegularisable (bplmodulefile, p, Xss))
  = ("permutation is not regularisable:\n  p = "
       ^ permutation2string p ^ " : "
       ^ interface2string (Permutation.innerface p) ^ " -> "
       ^ interface2string (Permutation.outerface p) ^ "\n  Xss = "
       ^ list2string "{" "}" ", " (list2string "{" "}" ", " nset2string) Xss
       ^ "\n(Error detected in " ^ bplmodulefile ^ ")")
  | explain' (Permutation.UnequalLengths (bplmodulefile, Uss, U'ss, errortext))
  = (" lists of unequal length:\n  Uss = "
       ^ list2string "{" "}" ", " (list2string "{" "}" ", " nset2string) Uss
       ^ "\n  U'ss = "
       ^ list2string "{" "}" ", " (list2string "{" "}" ", " nset2string) U'ss
       ^ "\n(Error detected in " ^ bplmodulefile
       ^ ": " ^ errortext ^ ")")
  | explain' (Sugar.WrongArity errortext)
  = errortext
  | explain' unknownError = raise unknownError

fun explain (e as (BgVal.NotPrime (bplmodulefile, v, errortext)))
  = ErrorMsg.error 
      (#1 (bgvalinfo2pos (BgVal.info v)))
      (#2 (bgvalinfo2pos (BgVal.info v)))
      (explain' e)
  | explain (e as (BgVal.DuplicateNames (bplmodulefile, i, ns, errortext)))
  = ErrorMsg.error 
      (#1 (bgvalinfo2pos i))
      (#2 (bgvalinfo2pos i))
      (explain' e)
  | explain (e as (BgVal.NameClash (bplmodulefile, i, ns1, ns2, errortext)))
  = ErrorMsg.error 
      (#1 (bgvalinfo2pos i))
      (#2 (bgvalinfo2pos i))
      (explain' e)
  | explain (e as (BgVal.NameMissing (bplmodulefile, v, errortext)))
  = ErrorMsg.error 
      (#1 (bgvalinfo2pos (BgVal.info v)))
      (#2 (bgvalinfo2pos (BgVal.info v)))
      (explain' e)
  | explain 
      (e as (BgVal.NotImplemented (bplmodulefile, v, errortext)))
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
  | explain (e as (Permutation.LogicalError (bplmodulefile, errortext)))
  = ErrorMsg.error 0 0 (explain' e)
  | explain (e as (Permutation.NotPermutation (bplmodulefile, p)))
  = ErrorMsg.error 0 0 (explain' e)
  | explain (e as (Permutation.Uncomposable (bplmodulefile, p1, p2, errortext)))
  = ErrorMsg.error 0 0 (explain' e)
  | explain (e as (Permutation.NotRegularisable (bplmodulefile, p, Xss)))
  = ErrorMsg.error 0 0 (explain' e)
  | explain (e as (Permutation.UnequalLengths (bplmodulefile, Uss, U'ss, errortext)))
  = ErrorMsg.error 0 0 (explain' e)
  | explain (e as (Sugar.WrongArity (errortext)))
  = ErrorMsg.error 0 0 (explain' e)
  | explain unknownError = raise unknownError
end
