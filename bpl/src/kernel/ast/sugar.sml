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

(** Syntactic sugar for creating bgvals in SML.
 * @version $LastChangedRevision$
 *)
functor Sugar (type info
	       val noinfo : info
	       structure Name : NAME
	       structure NameSet : MONO_SET
	       structure Interface : INTERFACE
	       structure Link : LINK
	       structure LinkSet : MONO_SET
	       structure Control : CONTROL
	       structure Ion : ION
	       structure Wiring : WIRING
	       structure Permutation : PERMUTATION
	       structure BgVal : BGVAL
	       sharing type info = BgVal.info
               sharing type Name.name = 
			    NameSet.elt =
			    Link.name =
			    Ion.name
               sharing type NameSet.Set = 
			    Interface.nameset =
			    BgVal.nameset =
			    Link.nameset =
			    Ion.nameset =
			    Wiring.nameset =
			    Permutation.nameset
	       sharing type Interface.interface =
			    BgVal.interface
               sharing type Link.link = LinkSet.elt
               sharing type LinkSet.Set = Wiring.linkset
	       sharing type Control.control = Ion.control
	       sharing type Ion.ion = BgVal.ion
               sharing type Wiring.wiring = BgVal.wiring
               sharing type Permutation.permutation = BgVal.permutation
               sharing type Permutation.Immutable = BgVal.Immutable
	       ) :> SUGAR 
where type bgval = BgVal.bgval =
struct
type control = Control.control
type name = string
type nameset = NameSet.Set
type bgval = BgVal.bgval
type arities = {boundarity : int, freearity : int}
type mapinfo = int * nameset
type absinfo = nameset
exception WrongArity of string

val Ion = BgVal.Ion noinfo
val Mer = BgVal.Mer noinfo
val Wir = BgVal.Wir noinfo
fun Per x = BgVal.Per noinfo x
val Con = BgVal.Con noinfo
val Abs = BgVal.Abs noinfo
val Ten = BgVal.Ten noinfo
val Par = BgVal.Par noinfo
val Pri = BgVal.Pri noinfo
val Com = BgVal.Com noinfo

fun active f = f (fn v => v)
fun active0 K
  = Ion (Ion.make {ctrl = Control.make K, free = [], bound = []}) 

val passive = active
val passive0 = active0

fun atomic f
  = f (fn v => 
	  let
	    val X = (hd o Interface.loc o BgVal.innerface) v
	    val barrenroot
	      = if NameSet.isEmpty X then
		  Mer 0
		else
		  Ten [Wir (Wiring.introduce X), Mer 0]
	  in
	    Com (v, Abs (X, barrenroot))
	  end)
fun atomic0 K
  = Com (Ion (Ion.make {ctrl = Control.make K, free = [], bound = []}), 
	 Mer 0)

exception DuplicateName of
	  string * string * name list * name list list * string

fun listToString printfun ns =
    "[" 
    ^ #1 (foldr (fn (n, (s, notlast)) =>
		    (printfun n ^ (if notlast then ", " else "") ^ s,
		     true))
		("]", false)
		ns)

val namelistToString = listToString (fn s => s)
val namelistlistToString = listToString namelistToString

fun =: (K, {freearity, boundarity}) k free bound =
    if length free <> freearity then
      raise WrongArity ("Control " ^ K ^ " takes "
			^ Int.toString freearity 
			^ " free names, but was given `"
			^ namelistToString free ^ "'\n")
    else if length bound <> boundarity then
      raise WrongArity ("Control " ^ K ^ " takes "
			^ Int.toString boundarity 
			^ " bound names, but was given `"
			^ namelistlistToString bound ^ "'\n")
    else
      let
	val ion 
	  = Ion.make
	      {ctrl = Control.make K, 
	       free = map Name.make free, 
	       bound 
	       = map (NameSet.fromList o map Name.make) bound}
	      handle 
	      (NameSet.DuplicatesRemoved (X, xs))
	      => raise DuplicateName 
			 ("sugar.sml", K, free, bound,
			  "Duplicate names are not allowed in ions")
      in
	k (Ion ion)
      end
fun -: (K, freearity) k free =
    if length free <> freearity then
      raise WrongArity ("Control " ^ K ^ " takes "
			^ Int.toString freearity
			^ " free names, but was given `"
			^ namelistToString free ^ "'\n")
    else
      let
	val ion = Ion.make {ctrl = Control.make K, 
			    free = map Name.make free, 
			    bound = []}
      in
	k (Ion ion)
      end
fun --> (boundarity, freearity) = {freearity = freearity,
				   boundarity = boundarity}
val <-> = Mer 0
val op @ = Per o Permutation.make o map (fn j => (j, NameSet.empty))
val @@ = Per o Permutation.make
fun & (j, Xs) = (j, NameSet.fromList (map Name.make Xs))
val merge = Mer
fun ` Xs _ = Con (NameSet.fromList (map Name.make Xs))
fun // (y, Xs) = Wir (Wiring.make 
			(LinkSet.singleton 
			   (Link.make 
			      {outer 
			       = if y = "" then
				   NONE
				 else
				   SOME (Name.make y),
			       inner = NameSet.fromList 
					 (map Name.make Xs)})))
fun op / (y, x) = Wir (Wiring.make 
			 (LinkSet.singleton 
			    (Link.make 
			       {outer 
				= if y = "" then
				    NONE
				  else
				    SOME (Name.make y),
				inner = NameSet.singleton
					  (Name.make x)})))
fun op < Xs = NameSet.fromList (map Name.make Xs)
fun op > (X, P) = Abs (X, P)
val id_n = Per o Permutation.id_n
val id_X = Wir o Wiring.id_X o NameSet.fromList o map Name.make
val op o = Com
fun op * (b1, b2) = Ten [b1, b2]
val ** = Ten
fun || (b1, b2) = Par [b1, b2]
val ||| = Par
fun <|> (b1, b2) = Pri [b1, b2]
val <|>> = Pri
fun -/ x = "" / x
fun -// X = // ("", X)
end
