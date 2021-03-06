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
functor BGADT' (structure ErrorHandler : ERRORHANDLER
                  where type ppstream    = PrettyPrint.ppstream
                    and type break_style = PrettyPrint.break_style
                    and type origin      = Origin.origin) 
  : BG_ADT
      where type ErrorHandler.ppstream    = PrettyPrint.ppstream
        and type ErrorHandler.break_style = PrettyPrint.break_style
        and type ErrorHandler.origin      = Origin.origin =
struct

structure ErrorHandler = ErrorHandler

structure Info = Info

structure Name    = Name(structure ErrorHandler = ErrorHandler)
(*structure NameSet = OrderSet (Name.Order)*)
structure NameSet = Name.NameSet
structure NameSetCompare 
  = SetCompare 
      (structure Set       = NameSet
       structure EltOrder  = Name.Order)
structure NameMap = OrderFinMap (Name.Order)

structure IntOrder = struct type T = int fun lt i j = i < j end
structure IntSet = OrderSet (IntOrder)
		   
structure NameSetPP 
  = SetPrettyPrint 
      (val       pp_elt      = Name.pp
       structure Set         = NameSet
       structure PrettyPrint = PrettyPrint)

structure ListPP
  = PolyListPrettyPrint
      (structure PrettyPrint = PrettyPrint)

structure NameBijectionConstraints
  = BijectionConstraints
      (structure Set = NameSet
       structure Map = NameMap)

structure Interface 
  = Interface' 
      (structure NameSet     = NameSet
       structure NameSetPP   = NameSetPP)

structure Control = Control

structure Link 
  = Link'
      (structure Name           = Name
       structure NameSet        = NameSet
       structure NameSetCompare = NameSetCompare)

structure LinkSet = OrderSet (Link.Order)

structure Ion 
  = Ion'
      (structure Control     = Control
       structure Name        = Name
       structure NameSet     = NameSet
       structure NameBijectionConstraints = NameBijectionConstraints
       structure NameSetPP   = NameSetPP)

structure Wiring 
  = Wiring'
      (structure IntSet       = IntSet
       structure Link         = Link
       structure LinkSet      = LinkSet
       structure Name         = Name
       structure NameMap      = NameMap
       structure NameSet      = NameSet
       structure NameSetPP    = NameSetPP
       structure NameBijectionConstraints = NameBijectionConstraints
       structure ErrorHandler = ErrorHandler)

structure Permutation
  = Permutation'
      (structure Name         = Name
       structure NameSet      = NameSet
       structure NameBijectionConstraints = NameBijectionConstraints
       structure IntSet       = IntSet
       structure Interface    = Interface
       structure NameSetPP    = NameSetPP
       structure ErrorHandler = ErrorHandler)
  
structure BgTerm
  = BgTerm'
      (structure Info        = Info
       structure Link         = Link
       structure LinkSet      = LinkSet
       structure Ion         = Ion
       structure Control     = Control
       structure Wiring      = Wiring
       structure Permutation = Permutation
       structure Name        = Name
       structure NameSet     = NameSet
       structure NameSetPP   = NameSetPP
       structure ErrorHandler = ErrorHandler)

structure BgVal 
  = BgVal'
      (structure Info             = Info
       structure Name             = Name
       structure NameSet          = NameSet
       structure NameBijectionConstraints = NameBijectionConstraints
       structure Link             = Link
       structure LinkSet          = LinkSet
       structure Interface        = Interface
       structure Control          = Control
       structure Ion              = Ion
       structure Wiring           = Wiring
       structure Permutation      = Permutation
       structure BgTerm           = BgTerm
       structure NameSetPP        = NameSetPP
       structure ErrorHandler     = ErrorHandler)
      
structure BgBDNF 
  = BgBDNF'
      (structure Info             = Info
       structure Link             = Link
       structure Name             = Name
       structure NameSet          = NameSet
       structure NameBijectionConstraints = NameBijectionConstraints
       structure LinkSet          = LinkSet
       structure Interface        = Interface
       structure Ion              = Ion
       structure Wiring           = Wiring
       structure Permutation      = Permutation
       structure BgVal            = BgVal
       structure ErrorHandler     = ErrorHandler
       structure NameSetPP        = NameSetPP
       structure ListPP           = ListPP)

structure BPLTerm = BPLTerm (structure Control = Control)

type bgterm = BgTerm.bgterm

type bgval = BgVal.bgval

type control = Control.control

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

structure Instantiation
  = Instantiation
      (structure Info             = Info
       structure Name             = Name
       structure NameMap          = NameMap
       structure NameSet          = NameSet
       structure Interface        = Interface
       structure Wiring           = Wiring
       structure BgVal            = BgVal
       structure BgBDNF           = BgBDNF
       structure ErrorHandler     = ErrorHandler
       structure NameSetPP        = NameSetPP
       structure ListPP           = ListPP)

structure Rule = Rule
  (structure Info          = Info
   structure Interface     = Interface
   structure NameSet       = NameSet
   structure BgVal         = BgVal
   structure BgBDNF        = BgBDNF
   structure Instantiation = Instantiation
   structure ErrorHandler  = ErrorHandler)

type rule = Rule.rule

structure BPL2BgVal = BPL2BgVal (
  structure Info          = Info
  structure Origin        = Origin
  structure Name          = Name
  structure NameSet       = NameSet
  structure Interface     = Interface
  structure Link          = Link
  structure LinkSet       = LinkSet
  structure Wiring        = Wiring
  structure Permutation   = Permutation
  structure Control       = Control
  structure Ion           = Ion
  structure BgTerm        = BgTerm
  structure BgVal         = BgVal
  structure BgBDNF        = BgBDNF
  structure Instantiation = Instantiation
  structure Rule          = Rule
  structure BPLTerm       = BPLTerm
)

structure Match = Match'
  (structure Info         = Info
   structure Name         = Name
   structure NameMap      = NameMap
   structure Link         = Link
   structure NameSet      = NameSet
   structure LinkSet      = LinkSet
   structure Control      = Control
   structure Permutation  = Permutation
   structure Interface    = Interface
   structure Ion          = Ion
   structure Wiring       = Wiring
   structure BgVal        = BgVal
   structure BgBDNF       = BgBDNF
   structure Rule         = Rule
   structure LazyList     = LazyList
   structure ErrorHandler = ErrorHandler)

structure RuleNameMap = OrderFinMap
  (type T = string
   fun lt s1 s2 = (s1 : T) < s2)

structure Reaction = Reaction
  (structure RuleNameMap   = RuleNameMap
   structure Info          = Info
   structure Interface     = Interface
   structure Wiring        = Wiring
   structure BgVal         = BgVal
   structure BgBDNF        = BgBDNF
   structure Match         = Match
   structure Instantiation = Instantiation
   structure Rule          = Rule
   structure Origin        = Origin
   structure ErrorHandler  = ErrorHandler)

val pageWidth = ref 70
val indent = ref 1

structure Sugar 
  = Sugar'
      (structure Info          = Info
       structure Control       = Control
       structure Name          = Name
       structure NameSet       = NameSet
       structure NameSetPP     = NameSetPP
       structure Interface     = Interface
       structure Link          = Link
       structure LinkSet       = LinkSet
       structure Ion           = Ion
       structure Wiring        = Wiring
       structure Permutation   = Permutation
       structure BgVal         = BgVal
       structure BgBDNF        = BgBDNF
       structure Rule          = Rule
       structure Instantiation = Instantiation
       structure ErrorHandler = ErrorHandler)

fun getRev s = getOpt (Int.fromString s, 0)
val revisions
  = [hd (String.tokens (not o Char.isDigit) "$LastChangedRevision$"),
     BgVal.revision, BgBDNF.revision, Match.revision, Sugar.revision]
val revision
  = Int.toString (foldr Int.max 0 (map getRev revisions))
end

functor BGADT (structure ErrorHandler : ERRORHANDLER
                 where type ppstream    = PrettyPrint.ppstream
                   and type break_style = PrettyPrint.break_style
                   and type origin      = Origin.origin) 
  :> BG_ADT
      where type ErrorHandler.ppstream    = PrettyPrint.ppstream
        and type ErrorHandler.break_style = PrettyPrint.break_style
        and type ErrorHandler.origin      = Origin.origin =
struct
  structure BGADT = BGADT'(structure ErrorHandler = ErrorHandler)
  open BGADT
end
