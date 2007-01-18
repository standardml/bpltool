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
functor BgTerm'(structure Info : INFO
		structure Ion : ION
		structure Wiring : WIRING
		structure Permutation : PERMUTATION
		structure NameSet : MONO_SET
		structure NameSetPP : COLLECTIONPRETTYPRINT
                  where type ppstream    = PrettyPrint.ppstream
		sharing type NameSet.Set =
                             Wiring.nameset =
                             NameSetPP.collection)
      : BGTERM =
struct
  type info = Info.info
  type nameset = NameSet.Set
  type ion = Ion.ion
  type Immutable = Permutation.Immutable
  type 'kind permutation = 'kind Permutation.permutation
  type wiring = Wiring.wiring

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

  fun eq (Mer (i1, _))       (Mer (i2, _))                = i1 = i2
    | eq (Con (ns1, _))      (Con (ns2, _))               = NameSet.eq ns1 ns2
    | eq (Wir (w1, _))       (Wir (w2, _))                = Wiring.eq w1 w2
    | eq (Ion (i1, _))       (Ion (i2, _))                = Ion.eq i1 i2
    | eq (Per (p1, _))       (Per (p2, _))                = Permutation.eq p1 p2
    | eq (Abs (ns1, b1, _))  (Abs (ns2, b2, _))           = 
        NameSet.eq ns1 ns2 andalso eq b1 b2
    | eq (Ten (bs1, _))      (Ten (bs2, _))               = 
        ListPair.all (fn (b1, b2) => eq b1 b2) (bs1, bs2)
    | eq (Pri (bs1, _))      (Pri (bs2, _))               = 
        ListPair.all (fn (b1, b2) => eq b1 b2) (bs1, bs2)
    | eq (Par (bs1, _))      (Par (bs2, _))               =
        ListPair.all (fn (b1, b2) => eq b1 b2) (bs1, bs2)
    | eq (Com (b11, b12, _)) (Com (b21, b22, _))          =
        eq b11 b21 andalso eq b12 b22
    | eq _ _                                              = false

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
		    show "o ";
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

  fun size bg =
      case bg of
	  Mer _ => 1
	| Con _ => 1
	| Wir _ => 1
	| Ion _ => 1
	| Per _ => 1
	| Abs(_,b,_) => 1+size b
	| Ten(bs,_) => 1+sizes bs
	| Pri(bs,_) => 1+sizes bs
	| Par(bs,_) => 1+sizes bs
	| Com(b1,b2,_) => 1+size b1+size b2
  and sizes bs = List.foldl (fn (b,s) => size b + s) 0 bs

end

functor BgTerm (structure Info : INFO
		structure Ion : ION
		structure Wiring : WIRING
		structure Permutation : PERMUTATION
		structure NameSet : MONO_SET
		structure NameSetPP : COLLECTIONPRETTYPRINT
                  where type ppstream    = PrettyPrint.ppstream
		sharing type NameSet.Set =
                             Wiring.nameset =
                             NameSetPP.collection)
      :> BGTERM where type info = Info.info
                  and type wiring = Wiring.wiring
                  and type 'kind permutation = 'kind Permutation.permutation
                  and type Immutable = Permutation.Immutable
                  and type ion = Ion.ion
                  and type nameset = NameSet.Set
 =
struct
  structure BgTerm = BgTerm'(structure Info = Info
                             structure Ion = Ion
                             structure Wiring = Wiring
                             structure Permutation = Permutation
                             structure NameSet = NameSet
                             structure NameSetPP = NameSetPP)
  open BgTerm
end