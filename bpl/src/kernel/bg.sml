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

(** Main BG module.
 * @version $LastChangedRevision$
 *)
functor BG' (structure ErrorHandler : ERRORHANDLER
               where type ppstream    = PrettyPrint.ppstream
                 and type break_style = PrettyPrint.break_style
                 and type origin      = Origin.origin)
  : BG where type 'a Match.lazylist = 'a LazyList.lazylist =
struct

structure BGADT = BGADT' (structure ErrorHandler = ErrorHandler)
open BGADT
		   
structure Token = LrParser.Token
	   
structure BgTermLrVals
  = BgTermLrVals
      (structure Info        = Info
       structure Token       = Token
       structure Control     = Control
       structure Name        = Name
       structure NameSet     = NameSet
       structure Link        = Link
       structure LinkSet     = LinkSet
       structure Wiring      = Wiring
       structure Ion         = Ion
       structure Permutation = Permutation
       structure BgTerm      = BgTerm)

structure BgTermLex 
  = BgTermLex (structure Tokens = BgTermLrVals.Tokens)

structure BgTermParser
  = Join
      (structure ParserData  = BgTermLrVals.ParserData
       structure Lex         = BgTermLex
       structure LrParser    = LrParser)

fun pp bdnf = BgBDNF.pp (!indent) bdnf

fun toString bdnf 
  = PrettyPrint.pp_to_string (!pageWidth) pp bdnf

fun bgvalToString v
  = PrettyPrint.pp_to_string (!pageWidth) (BgVal.pp (!indent)) v

fun bgvalUsefile'' filename =
    let 
      val _ = (ErrorMsg.reset(); ErrorMsg.fileName := filename)
      val file = TextIO.openIn filename
      fun get _ = TextIO.input file
      fun parseerror (s, p1, p2) = ErrorMsg.error p1 p2 s
      val lexer = BgTermLex.makeLexer get
      val (bgterm, _)
	= BgTermParser.parse
	    (30, 
	     BgTermParser.Stream.streamify (lexer),
	     parseerror, 
	     ())
      val bgval = BgVal.make BgTerm.info bgterm
    in
      TextIO.closeIn file;
      bgval
    end

fun bgvalUsefile' filename =
    bgvalUsefile'' filename
      handle BgTermParser.ParseError => raise ErrorMsg.Error
	   | error => (ErrorHandler.explain error; raise ErrorMsg.Error)

fun usefile'' filename = 
    let
      val bgval = bgvalUsefile'' filename
      val bgbdnf = BgBDNF.make bgval
    in
      bgbdnf
    end

fun usefile' filename = 
    let
      val bgval = bgvalUsefile' filename
      val bgbdnf = BgBDNF.make bgval
    in
      bgbdnf
    end
      handle error => (ErrorHandler.explain error; raise ErrorMsg.Error)

fun usefile filename =
    let
      val bgbdnf = usefile' filename
    in
      print (toString bgbdnf); print "\n";
      bgbdnf
    end

end

functor BG (structure ErrorHandler : ERRORHANDLER
              where type ppstream    = PrettyPrint.ppstream
                and type break_style = PrettyPrint.break_style
                and type origin      = Origin.origin)
        :> BG where type 'a Match.lazylist = 'a LazyList.lazylist =
struct
  structure BG = BG'(structure ErrorHandler = ErrorHandler)
  open BG
end
