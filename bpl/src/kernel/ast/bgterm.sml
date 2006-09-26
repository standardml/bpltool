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

(** Abstract data type for bigraph terms.
 * The terms are not necessarily well-formed:
 * scope rule may be violated in abstractions, names may clash in
 * interfaces of tensor product operands, bigraph widths may be
 * incompatible in compositions, etc.
 * <p>
 * Each constructor takes an info argument that can contain contextual
 * information (e.g., source file location for the term).
 * @version $LastChangedRevision$
 *)
functor BgTerm (type info
		structure Ion : ION
		structure Wiring : WIRING
		structure Permutation : PERMUTATION
		structure PrettyPrint : PRETTYPRINT
		structure NameSetPP : COLLECTIONPRETTYPRINT
		val noinfo : info
                sharing type PrettyPrint.ppstream =
			     Ion.ppstream =
			     Wiring.ppstream =
			     Permutation.ppstream =
			     NameSetPP.ppstream
		sharing type Wiring.nameset = NameSetPP.collection) : BGTERM =
struct
  type info = info
  type nameset = Wiring.nameset
  type ion = Ion.ion
  type Immutable = Permutation.Immutable
  type 'kind permutation = 'kind Permutation.permutation
  type wiring = Wiring.wiring
  type ppstream = PrettyPrint.ppstream

  val noinfo = noinfo

  datatype bgterm = 
	   Mer of int * info
	 | Con of nameset * info
	 | Wir of wiring * info
	 | Ion of ion * info
	 | Per of Immutable permutation * info
	 | Abs of nameset * bgterm * info
	 | Ten of bgterm list * info
	 | Pri of bgterm list * info
	 | Par of bgterm list * info
	 | Com of bgterm * bgterm * info

  fun WLS i ws = 
      Ten (map
	   (fn w =>
	       Abs (Wiring.outernames w, 
		    Com (Ten ([Wir (w, i),
			      Per (Permutation.id_n 1, i)], 
			      i),
			 Con (Wiring.innernames w, i),
			 i),
		    i))
	   ws,
	   i)

  fun info (Mer (_, i))    = i
    | info (Con (_, i))    = i
    | info (Wir (_, i))    = i
    | info (Ion (_, i))    = i
    | info (Per (_, i))    = i
    | info (Abs (_, _, i)) = i
    | info (Ten (_, i))    = i
    | info (Pri (_, i))    = i
    | info (Par (_, i))    = i
    | info (Com (_, _, i)) = i

  fun pp indent pps =
      let
	open PrettyPrint
	val PrMax = 9
	val PrAbs = 8
	val PrCom = 7
	val PrTen = 6
	val PrPri = 6
	val PrPar = 6
	val PrMin = 0
	val show = add_string pps
	fun << () = begin_block pps INCONSISTENT indent
	fun >> () = end_block pps
	fun brk () = add_break pps (1, 0)
	fun brk0 () = add_break pps (0, 0)
  (* Pretty print using precedences.  Operator precedences
   * (highest binds tightest):
   *  8 (X) abstraction (only affects expressions to the right)
   *  7  o  composition
   *  6  x  tensor product
   * Abstraction reaches as far right as possible.
   * @params pps prf prr t
   * @param pps  PrettyPrint stream
   * @param pal  Precedence attraction of surrounding left expression
   * @param par  Precedence attraction of surrounding right expression
   * @param prr  Precedence resistance of surrounding right expression
   * @param t    Term to print
   *)
	fun ppp pal par prr =
	    let 
	      fun checkprec prec =
		  if pal >= prec orelse par > prec then
		    (fn () => show "(", 
		     PrMin, PrMin, PrMax, 
		     fn () => show ")")
		  else
		    (fn () => (), pal, par, prr, fn () => ())
	      fun pp' (Mer (0, _)) = show "1"
		| pp' (Mer (n, _)) = show ("merge_" ^ Int.toString n)
		| pp' (Con (X, _)) 
		  = (show "'"; NameSetPP.pp indent pps X; show "'")
		| pp' (Wir (w, _)) = Wiring.pp indent pps w
		| pp' (Ion (KyX, _)) = Ion.pp indent pps KyX
		| pp' (Per (pi, _)) = Permutation.pp indent pps pi
		| pp' (Abs (X, b, _)) =
		  let
		    val (showlpar, pal', par', prr', showrpar)
		      = if prr < PrAbs then 
			  (fn () => show "(", 
			   PrMin, PrMin, PrMax,
			   fn () => show ")")
			else
			  (fn () => (), PrMin, par, prr, fn () => ())
		  in
		    <<(); 
		     showlpar();
                     show "("; NameSetPP.pp indent pps X; show ")";
		     brk0();
		     ppp pal' par' prr' b;
		     showrpar();
		     >>()
		  end
		| pp' (Ten (bs, _)) =
		  (case bs of
		    [] => show "idx_0"
		  | [b] => pp' b
		  | (b :: bs) => 
		    let
		      val (showlpar, pal', par', prr', showrpar) 
			= checkprec PrTen
		      fun mappp [] = ()
			| mappp [b]
			  = (brk(); show "* "; ppp PrTen par' prr' b)
			| mappp (b :: b' :: bs) 
			  = (brk();
			     show "* ";
			     ppp PrTen PrTen PrTen b;
			     mappp (b' :: bs))
		    in
		      <<();
		      showlpar();
		      ppp pal' PrTen PrTen b;
		      mappp bs;
		      showrpar();
		      >>()
		    end)
		| pp' (Pri (bs, _)) =
		  (case bs of
		    [] => show "1"
		  | [b] => pp' b
		  | (b :: bs) => 
		    let
		      val (showlpar, pal', par', prr', showrpar) 
			= checkprec PrPri
		      fun mappp [] = ()
			| mappp [b]
			  = (brk(); show "| "; ppp PrPri par' prr' b)
			| mappp (b :: b' :: bs) 
			  = (brk();
			     show "| ";
			     ppp PrPri PrPri PrPri b;
			     mappp (b' :: bs))
		    in
		      <<();
		      showlpar();
		      ppp pal' PrPri PrPri b;
		      mappp bs;
		      showrpar();
		      >>()
		    end)
		| pp' (Par (bs, _)) =
		  (case bs of
		    [] => show "id||_0"
		  | [b] => pp' b
		  | (b :: bs) => 
		    let
		      val (showlpar, pal', par', prr', showrpar) 
			= checkprec PrPar
		      fun mappp [] = ()
			| mappp [b]
			  = (brk(); show "|| "; ppp PrPar par' prr' b)
			| mappp (b :: b' :: bs) 
			  = (brk();
			     show "|| ";
			     ppp PrPar PrPar PrPar b;
			     mappp (b' :: bs))
		    in
		      <<();
		      showlpar();
		      ppp pal' PrPar PrPar b;
		      mappp bs;
		      showrpar();
		      >>()
		    end)
		| pp' (Com (b1, b2, _)) = 
		  let
		    val (showlpar, pal', par', prr', showrpar)
		      = checkprec PrCom
		  in
		    <<();
		    showlpar();
		    ppp pal' PrCom PrCom b1;
		    brk();
		    ppp PrCom par' prr' b2;
		    showrpar();
		    >>()
		  end
	    in
	      pp'
	    end
      in
	ppp PrMin PrMin PrMax
      end
end
