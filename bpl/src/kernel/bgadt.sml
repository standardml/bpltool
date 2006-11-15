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

(** BG Abstract Data Types module.
 * @version $LastChangedRevision$
 *)
functor BGADT (type info
                val noinfo : info
		val bgvalinfo2pos : info -> int * int
		structure PrettyPrint : PRETTYPRINT) 
	: BG_ADT =
struct

type info = info
val noinfo = noinfo

structure Name    = Name
structure NameSet = OrderSet (Name.Order)
structure NameSetCompare 
  = SetCompare 
      (structure Set       = NameSet
       structure EltOrder  = Name.Order)
      
structure IntOrder = struct type T = int fun lt i j = i < j end
structure IntSet = OrderSet (IntOrder)
		   
fun pp_name _ pps x = PrettyPrint.add_string pps (Name.unmk x)

structure NameSetPP 
  = SetPrettyPrint 
      (val       pp_elt      = pp_name
       structure Set         = NameSet
       structure PrettyPrint = PrettyPrint)

structure Interface 
  = Interface 
      (structure NameSet     = NameSet
       structure NameSetPP   = NameSetPP
       structure PrettyPrint = PrettyPrint)

structure Control = Control

structure Link 
  = Link 
      (structure Name           = Name
       structure NameSet        = NameSet
       structure NameSetCompare = NameSetCompare)

structure LinkSet = OrderSet (Link.Order)

structure Ion 
  = Ion
      (structure Control     = Control
       structure Name        = Name
       structure NameSet     = NameSet
       structure NameSetPP   = NameSetPP
       structure PrettyPrint = PrettyPrint)

structure Wiring 
  = Wiring 
      (structure IntSet      = IntSet
       structure Link        = Link
       structure LinkSet     = LinkSet
       structure Name        = Name
       structure NameSet     = NameSet
       structure NameSetPP   = NameSetPP
       structure PrettyPrint = PrettyPrint)

structure Permutation
  = Permutation 
      (structure Name        = Name
       structure NameSet     = NameSet
       structure IntSet      = IntSet
       structure Interface   = Interface
       structure NameSetPP   = NameSetPP
       structure PrettyPrint = PrettyPrint)

structure BgTerm
  = BgTerm 
      (type      info        = info
       val       noinfo      = noinfo
       structure Ion         = Ion
       structure Wiring      = Wiring
       structure Permutation = Permutation
       structure NameSetPP   = NameSetPP
       structure PrettyPrint = PrettyPrint)

structure BgVal 
  = BgVal
      (type      info        = info
       val       noinfo      = noinfo
       structure Name        = Name
       structure NameSet     = NameSet
       structure Link        = Link
       structure LinkSet     = LinkSet
       structure Interface   = Interface
       structure Ion         = Ion
       structure Wiring      = Wiring
       structure Permutation = Permutation
       structure BgTerm      = BgTerm
       structure PrettyPrint = PrettyPrint)
      
structure BgBDNF 
  = BgBDNF
      (type      info        = info
       structure Link        = Link
       structure Name        = Name
       structure NameSet     = NameSet
       structure LinkSet     = LinkSet
       structure Interface   = Interface
       structure Ion         = Ion
       structure Wiring      = Wiring
       structure Permutation = Permutation
       structure BgVal       = BgVal
       structure PrettyPrint = PrettyPrint)

type bgterm = BgTerm.bgterm

type bgval = BgVal.bgval

type M = BgBDNF.M
type S = BgBDNF.S
type G = BgBDNF.G
type N = BgBDNF.N
type P = BgBDNF.P
type D = BgBDNF.D
type DR = BgBDNF.DR
type B = BgBDNF.B
type BR = BgBDNF.BR
type 'class bgbdnf = 'class BgBDNF.bgbdnf

structure Match = Match
  (type      info        = info
   structure Name        = Name
   structure Link        = Link
   structure NameSet     = NameSet
   structure LinkSet     = LinkSet
   structure Permutation = Permutation
   structure Interface   = Interface
   structure Ion         = Ion
   structure Wiring      = Wiring
   structure BgVal       = BgVal
   structure BgBDNF      = BgBDNF
   structure LazyList    = LazyList
   structure PrettyPrint = PrettyPrint)

val pageWidth = ref 70
val indent = ref 1
type ppstream = PrettyPrint.ppstream

structure Sugar 
  = Sugar 
      (type      info        = info
       val       noinfo      = noinfo
       structure Control     = Control
       structure Name        = Name
       structure NameSet     = NameSet
       structure Interface   = Interface
       structure Link        = Link
       structure LinkSet     = LinkSet
       structure Ion         = Ion
       structure Wiring      = Wiring
       structure Permutation = Permutation
       structure BgVal       = BgVal)

structure BGErrorHandler
  = BGErrorHandler 
    (val pageWidth           = pageWidth
     val indent              = indent
     val bgvalinfo2pos       = bgvalinfo2pos
     structure BgBDNF        = BgBDNF
     structure BgVal         = BgVal
     structure BgTerm        = BgTerm
     structure Interface     = Interface
     structure Ion           = Ion
     structure Permutation    = Permutation
     structure Name          = Name
     structure NameSet       = NameSet
     structure Sugar         = Sugar
     structure PrettyPrint   = PrettyPrint)

end
