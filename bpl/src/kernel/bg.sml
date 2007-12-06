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
  : BG =
struct
open Debug
open ErrorHandler
val file_origin = Origin.mk_file_origin
                      "$BPL/src/kernel/bg.sml"
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

structure RulesLrVals
  = RulesLrVals
      (structure Info          = Info
       structure Token         = Token
       structure Interface     = Interface
       structure Control       = Control
       structure Name          = Name
       structure NameSet       = NameSet
       structure Link          = Link
       structure LinkSet       = LinkSet
       structure Wiring        = Wiring
       structure Ion           = Ion
       structure Permutation   = Permutation
       structure BgTerm        = BgTerm
       structure BgVal         = BgVal
       structure BgBDNF        = BgBDNF
       structure Instantiation = Instantiation
       structure Rule          = Rule)

structure RulesLex 
  = RulesLex (structure Tokens = RulesLrVals.Tokens)

structure RulesParser
  = Join
      (structure ParserData  = RulesLrVals.ParserData
       structure Lex         = RulesLex
       structure LrParser    = LrParser)

structure BigraphData = BigraphData (
  structure Info    = Info
  structure Name    = Name
  structure Control = Control
  structure BgTerm  = BgTerm)

structure BigraphXMLHooks
  = BigraphXMLHooks
      (structure Info        = Info
       structure Control     = Control
       structure Name        = Name
       structure NameSet     = NameSet
       structure BgTerm      = BgTerm
       structure BigraphData = BigraphData) 

structure BigraphXMLParser =
  BPLXMLParser (structure BPLXMLHooks = BigraphXMLHooks)

structure SignatureXMLHooks =
  SignatureXMLHooks (structure Control = Control)

structure SignatureXMLParser =
  BPLXMLParser (structure BPLXMLHooks = SignatureXMLHooks)

structure PPSVG =
  PPSVG (
    structure Name        = Name
    structure NameSet     = NameSet
    structure Interface   = Interface
    structure Control     = Control
    structure Ion         = Ion
    structure Permutation = Permutation
    structure Link        = Link
    structure LinkSet     = LinkSet
    structure Wiring      = Wiring
    structure BgVal       = BgVal
    structure BgBDNF      = BgBDNF)  

val toSVGString = PPSVG.ppsvgdoc
fun bgvalToSVGString c v = toSVGString c (BgBDNF.make v)

fun pp bdnf = BgBDNF.pp (!indent) bdnf

fun toString bdnf 
  = PrettyPrint.pp_to_string (!pageWidth) pp bdnf

fun bgvalToString v
  = PrettyPrint.pp_to_string (!pageWidth) (BgVal.pp (!indent)) v

fun parseBgTermStr filename str =
    let 
      val _ = (ErrorMsg.reset(); ErrorMsg.fileName := filename)
      val get' = ref (fn _ => "")
      val _ = get' := (fn _ => (str before get' := (fn _ => "")))
      fun get x = (!get') x
      fun parseerror (s, p1, p2) = ErrorMsg.error p1 p2 s
      val lexer = BgTermLex.makeLexer get
      val (rules, _)
				= BgTermParser.parse
	    			(30, 
				     BgTermParser.Stream.streamify (lexer),
				     parseerror, 
				     ())
				  handle ParseError
				   => raise ParsingError (ErrorMsg.getErrors ())
    in
      rules
    end

exception ThisCannotHappen

  type ruledata = {
    name : string,
    redex : bgterm,
    react : bgterm,
    maps : ((int * Name.name list) * (int * Name.name list)) list,
    info : Info.info}

local
  open RulesLrVals
  open ParserData.Header
  fun mkbgterm (BGTERMRESULT bgterm) = bgterm
    | mkbgterm _ = raise ThisCannotHappen
  fun mkrules (RULESRESULT rules) = rules
    | mkrules _ = raise ThisCannotHappen
  fun mksignature (SIGRESULT ctrllist) = ctrllist
    | mksignature _ = raise ThisCannotHappen
in
  type 'a kind
    = ((int * int) -> (Tokens.svalue, int) Tokens.token)
    * (parserresult -> 'a)
  val SIGNATURE = (Tokens.SIGNATURE, mksignature)
  val BGTERM = (Tokens.BGTERM, mkbgterm)
  val RULES = (Tokens.RULELIST, mkrules)
end


fun parseStr (kind, mkkind) filename str =
    let 
      val _ = (ErrorMsg.reset(); ErrorMsg.fileName := filename)
      val get' = ref (fn _ => "")
      val _ = get' := (fn _ => (str before get' := (fn _ => "")))
      fun get x = (!get') x
      fun parseerror (s, p1, p2) = ErrorMsg.error p1 p2 s
      val lexer
        = RulesParser.Stream.cons
            (kind (~1, ~1),
             RulesParser.Stream.streamify (RulesLex.makeLexer get))
      val (result, _)
				= RulesParser.parse
	    			(300, 
				     lexer,
				     parseerror, 
				     ())
				  handle RulesParser.ParseError
				   => raise ParsingError (ErrorMsg.getErrors ())
    in
      mkkind result
    end


fun bgvalUsefile'' filename =
    let 
      val _ = (ErrorMsg.reset(); ErrorMsg.fileName := filename)
      val file = TextIO.openIn filename
      fun get _ = TextIO.input file
      fun parseerror (s, p1, p2) = ErrorMsg.error p1 p2 s
      val lexer = BgTermLex.makeLexer get
      val (bgterm, _)
	= (BgTermParser.parse
	     (30, 
	      BgTermParser.Stream.streamify (lexer),
	      parseerror, 
	      ()))
          handle e => (TextIO.closeIn file; raise e)
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

fun brsUseXMLfiles sigfilename brsfilename =
  let
    exception ExpectedBRS
    val cs = SignatureXMLParser.parseFile () sigfilename
  in
    case BigraphXMLParser.parseFile cs brsfilename of
      BigraphData.BRS rs => (cs, rs)
    | BigraphData.BIGRAPH _ => (cs,[]) (* TODO! *) 
  end

end

functor BG (structure ErrorHandler : ERRORHANDLER
              where type ppstream    = PrettyPrint.ppstream
                and type break_style = PrettyPrint.break_style
                and type origin      = Origin.origin)
        :> BG where type ErrorHandler.break_style = ErrorHandler.break_style
                and type ErrorHandler.ppstream = ErrorHandler.ppstream
                and type ErrorHandler.origin = Origin.origin =
struct
  structure BG = BG'(structure ErrorHandler = ErrorHandler)
  open BG
end
