(* Copyright (c) 2007  The BPL Group at the IT University of Copenhagen
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

(** Glue to combine BPL lexer and parser.
 *  Lexes and parses a .bpl file, outputs (pp) a bplterm ast.
 * @version $LastChangedRevision$
 * Modified: $Date: 2006/09/04 21:48:46 $ by: $Author: hniss $
 * Modified by Ebbe Elsborg on December 12 2007.
 *)

functor BPLParser (structure BPLTerm : BPLTERM) =
struct

    exception ParseError

    type pos = int * int

    (* Create the Lexer and Parser by ML-Yacc magic *)
    structure BplLrVals = 
	BplLrValsFun(structure Token = LrParser.Token
	             structure BPLTerm = BPLTerm)
    structure BplL = 
	BplLexFun(structure Tokens = BplLrVals.Tokens)
    structure BplP = 
	JoinWithArg(structure ParserData = BplLrVals.ParserData
		    structure Lex = BplL
		    structure LrParser = LrParser)

    (* prettyprint a tree to stdErr *)
    fun ppToStdErr pptree =
	Pretty.ppPrint
	    pptree
	    (Pretty.plainOutput ("(*","*)"))
	    TextIO.stdErr

    (* print an error message *)
    fun printError (s, {pos=p1,src={file=file}}, {pos=p2,src=s2}) =
	let val func = SourceLocation.ppSourceLocation
	    val res = func file (p1,p2) [Pretty.ppString s]
	in ppToStdErr res end

    (* parse a stream and return an ast or a ParseError *)
    fun parseStream src stream =
	let val lexarg = {src=src}
	    val lexer = 
		BplP.makeLexer
		    (fn i => TextIO.inputN (stream, i)) 
		    lexarg
	    val (ast, stream) = 
		BplP.parse(15, lexer, printError, ())
	        handle BplP.ParseError => raise ParseError
	in ast end

    (* general function for parsing input streams
     * - used for all the main functions below.
     *)
    fun parseFile file =
	let val stream = TextIO.openIn file
	    val result = parseStream {file=file} stream
	in result before TextIO.closeIn stream end

end (* structure Parse *)

(* panic@itu.dk: Kernel files should be libraries, so the following
 * is commented out:
 
exception MissingInFile

(* prettyprint the constructed ast parsed from file *)
val _ =
    let val ast = BPLParser.parseFile (hd(CommandLine.arguments()))
	    handle Empty => raise MissingInFile
    in Pretty.ppPrint (BPLTerm.pp ast)
		      (Pretty.plainOutput ("(*","*)")) TextIO.stdOut
    end

*)