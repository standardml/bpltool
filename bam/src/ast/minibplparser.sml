(* Copyright (c) 2007  Henning Niss, IT University of Copenhagen
 *
 * BAM is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 *
 * BAM is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with BAM; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
 * USA
 *)

structure MiniBPLParser :> sig 
    val parseFile : string -> unit Rule.t list * unit Term.t
end = struct

    exception ParseError

    type pos = int * int

    (* Create the Lexer and Parser *)
    structure MiniBPLLrVals = 
	MiniBPLLrValsFun(structure Token = LrParser.Token)
    structure MiniBPLL = 
	MiniBPLLexFun(structure Tokens = MiniBPLLrVals.Tokens)
    structure MiniBPLP = 
	JoinWithArg(structure ParserData = MiniBPLLrVals.ParserData
		    structure Lex = MiniBPLL
		    structure LrParser = LrParser)

    fun printError (s, p1, p2) = TextIO.output(TextIO.stdOut, s)

    fun parseStream stream =
	let val lexer = 
		MiniBPLP.makeLexer (fn i => TextIO.inputN (stream, i)) 
		                  ()

	    val (ast, stream) = 
		MiniBPLP.parse(15, lexer, printError, ())
	             handle MiniBPLP.ParseError => raise ParseError
	in
	    ast
	end

    (* General function for parsing input streams
     * - used for all the main functions below.
     *)

    fun parseFile file =
	let val stream = TextIO.openIn file
	    val result = parseStream stream
	in  result before TextIO.closeIn stream
	end
    
end (* structure Parse *)
