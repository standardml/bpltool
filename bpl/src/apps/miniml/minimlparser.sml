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

(** Glue to combine lexer and parser.
 * @version $Revision: 1.9 $
 * Modified: $Date: 2006/09/04 21:48:46 $ by: $Author: hniss $
 *)

structure MiniMLParser: MINIML_PARSER =
struct

    type pos = int * int
    type prog = (pos,Pattern.pat) MiniML.prog

    (* Create the Lexer and Parser *)
    structure MiniMLLrVals = 
	MiniMLLrValsFun(structure Token = LrParser.Token)
    structure MiniMLL = 
	MiniMLLexFun(structure Tokens = MiniMLLrVals.Tokens)
    structure MiniMLP = 
	JoinWithArg(structure ParserData = MiniMLLrVals.ParserData
		    structure Lex = MiniMLL
		    structure LrParser = LrParser)

    fun fileDevice filename =
	let val in_stream = BinIO.openIn filename
	in  ( (), fn _ => BinIO.closeIn in_stream )
        end
(*
	let val in_stream = Nonstdio.open_in_bin filename
	in  ( { name = "File \"" ^ filename ^ "\""
	      , input = fn () => Nonstdio.input_char in_stream
	      , seek = Nonstdio.seek_in in_stream
              }
	    , fn _ => BasicIO.close_in in_stream
            )
	end
*)
    fun ppToStdErr pptree =
	Pretty.ppPrint pptree (Pretty.plainOutput ("(*","*)")) TextIO.stdErr

    (* General function for parsing input streams
     * - used for all the main functions below.
     *)
    fun parseStream device stream =
	let val lexarg = {comlevel=ref 0} (* argument for each lex rule *)

	    fun printError (s, p1, p2) =
		TextIO.output (TextIO.stdErr,"  Error (" ^ 
			       Int.toString p1 ^"-"^ Int.toString p2 ^"): "^ 
			       s ^ "\n")
(*
	    fun printError (s, p1, p2) = 
		ppToStdErr(ErrorLoc.ppErrorLocation' device (p1,p2)
						     [Pretty.ppString s])
*)
	    val lexer = 
		MiniMLP.makeLexer (fn i => TextIO.inputN (stream, i)) 
		                  lexarg

	    val (ast, stream) = 
		MiniMLP.parse(15, lexer, printError, ())
	             handle Fail(s) => (printError(s, 0, 0);
					raise Fail s)
			  | MiniMLP.ParseError => raise Fail("ParseError")
	in
	    ast
	end

    fun parseFile file =
	let val (device,close) = fileDevice file
	    val stream = TextIO.openIn file
	    val result = parseStream device stream
	in  result before ( TextIO.closeIn stream ; close() )
	end
(*
    fun parse string =
	let val stream = TextIO.openString string
	    val result = parseStream stream
	    val _ = TextIO.closeIn stream
	in  result
	end

    fun testFile file =
	let val ast = parseFile file
	in  (Bane.Pretty.begin_pretty ();
	     AST.pp_prog ast;
	     Bane.Pretty.add_newline ();
	     Eval.pp_values (Eval.eval_prog ast);
	     Bane.Pretty.end_pretty ())
	end
*)
    
end (* structure Parse *)
