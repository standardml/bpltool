structure Parser : sig val parseFile : string -> unit Rule.t list * unit Term.t end =
struct

    exception ParseError

    type pos = int * int

    (* Create the Lexer and Parser *)
    structure ParserLrVals = 
	ParserLrValsFun(structure Token = LrParser.Token)
    structure ParserL = 
	LexerLexFun(structure Tokens = ParserLrVals.Tokens)
    structure ParserP = 
	JoinWithArg(structure ParserData = ParserLrVals.ParserData
		    structure Lex = ParserL
		    structure LrParser = LrParser)

    fun printError (s, p1, p2) = TextIO.output(TextIO.stdOut, s)

    fun parseStream stream =
	let val lexer = 
		ParserP.makeLexer (fn i => TextIO.inputN (stream, i)) 
		                  ()

	    val (ast, stream) = 
		ParserP.parse(15, lexer, printError, ())
	             handle ParserP.ParseError => raise ParseError
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
