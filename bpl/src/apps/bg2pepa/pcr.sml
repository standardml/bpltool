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

(** Main PCR module.
 * @version $LastChangedRevision: 315 $
 *)

functor PCR (
  structure BG : BG
    where type ErrorHandler.ppstream    = PrettyPrint.ppstream
      and type ErrorHandler.break_style = PrettyPrint.break_style
      and type ErrorHandler.origin      = Origin.origin
) : PCR =
struct

open Debug
open BG.ErrorHandler
val file_origin = Origin.mk_file_origin
                      "$BPL/src/apps/pcr/pcr.sml"
                      Origin.NOPOS
fun mk_explainer errtitle (explainer : exn -> explanation list) e =
      Exp (LVL_USER, Origin.unknown_origin, mk_string_pp errtitle,
           explainer e)

exception ParsingError
  of (string * ((int * int) * (int * int)) * (int * int) * string)
      list
fun explain_ParsingError (ParsingError errors) =
      map (fn (file, fromto, (fromcharpos, tocharpos), errtxt)
            => Exp (LVL_USER, Origin.mk_file_origin file (Origin.POS fromto),
                              mk_string_pp errtxt, []))
          errors
    | explain_ParsingError _ = raise Match
  val _ = add_explainer
            (mk_explainer "Parsing error" explain_ParsingError)

structure PCRADT = PCRADT(structure BGADT = BG)
open PCRADT

structure Token = LrParser.Token
structure StringMap = Util.StringMap

structure PepaBgLrValsArgStruct =
struct
  structure Info      = Info
  structure Token     = Token
	structure Name      = BG.Name
	structure Control   = BG.Control
  structure Node      = Node
  structure BgAspects = BgAspects
  datatype result
    = StateResult of int * BgAspects.value BgAspects.AspectMap.map
    | StateListResult
      of (int * BgAspects.value BgAspects.AspectMap.map) list

  fun get_ctrl' signt (id, p1, p2) =
    case StringMap.lookup signt id of
      SOME ctrl => (ctrl : BG.Control.control)
    | NONE      => ( ErrorMsg.error p1 p2 ("unknown control: " ^ id)
                   ; raise Fail ("FIXME unknown control: " ^ id))
  val get_ctrl = ref (get_ctrl' StringMap.empty)
  fun set_signature signt = get_ctrl := get_ctrl' signt
end
structure PepaBgLrVals = PepaBgLrVals (PepaBgLrValsArgStruct)

structure PepaBgLex 
  = PepaBgLex (structure Tokens = PepaBgLrVals.Tokens)

structure PepaBgParser
  = Join (
      structure ParserData  = PepaBgLrVals.ParserData
      structure Lex         = PepaBgLex
      structure LrParser    = LrParser)

exception ThisCannotHappen

local
  open PepaBgLrVals
  open PepaBgLrValsArgStruct
  fun mkstate (StateResult state) = state
    | mkstate _ = raise ThisCannotHappen
  fun mkstatelist (StateListResult statelist) = statelist
    | mkstatelist _ = raise ThisCannotHappen
in
  type 'a kind
    = ((int * int) -> (Tokens.svalue, int) Tokens.token)
    * (result -> 'a)
  val STATE     = (Tokens.STATE, mkstate)
  val STATELIST = (Tokens.STATELIST, mkstatelist)
end

local
  fun parseerror (s, p1, p2) = ErrorMsg.error p1 p2 s

  fun parse (kind, mkkind) signt filename get close =
    let
      val _ = (ErrorMsg.reset(); ErrorMsg.fileName := filename)
      val _ = PepaBgLrValsArgStruct.set_signature signt
      val lexer = PepaBgLex.makeLexer get
      val lexer_stream = PepaBgParser.Stream.streamify lexer
      val lexer_stream'
        = PepaBgParser.Stream.cons (kind (~1, ~1), lexer_stream)

      val (result, _)
			  = (PepaBgParser.parse (30, lexer_stream', parseerror, ()))
			  handle e => ( close ()
                    ; raise ParsingError (ErrorMsg.getErrors ()))
    in
      ( close ()
      ; if !ErrorMsg.anyErrors then
          raise ParsingError (ErrorMsg.getErrors ())
        else
          mkkind result)
    end
in
  fun parseStr kindXmk signt filename str =
    let
      (* FIXME is this where we want it? And do we want it? *)
      val oldval = Flags.getBoolFlag "/kernel/bg/name/strip"
                   before Flags.setBoolFlag "/kernel/bg/name/strip" true;

      val get' = ref (fn _ => "")
      val _ = get' := (fn _ => (str before get' := (fn _ => "")))
      fun get x = (!get') x
      fun close _ = Flags.setBoolFlag "/kernel/bg/name/strip" oldval
    in
      parse kindXmk signt filename get (fn _ => ())
    end

  fun usefile kindXmk signt filename =
    let 
      (* FIXME is this where we want it? And do we want it? *)
      val oldval = Flags.getBoolFlag "/kernel/bg/name/strip"
                   before Flags.setBoolFlag "/kernel/bg/name/strip" true;

      val file = TextIO.openIn filename
      fun get _ = TextIO.input file
      fun close () = ( TextIO.closeIn file
                     ; Flags.setBoolFlag "/kernel/bg/name/strip" oldval)
    in
      parse kindXmk signt filename get close
    end
end

val parsePepaBgStateStr     = parseStr STATE
val parsePepaBgStateListStr = parseStr STATELIST

end
