(* Position info added by the parser
 * (c) 2004  The BPL Group, IT University of Copenhagen
 *)

(* This structure defines the format of position info added by the parser.
 *
 * So in the parser (defined in bplgrammar.grm) the type-variable 'info in 
 * the abstract syntax is instantiated to Pos.posinfo.
 * 
 * Pos.posinfo data should be read: 
 * (filename, start - (line, column), end - (line,column)) 
*)
structure Pos : POS =
struct
    type pos = int

    datatype posinfo = Pos of string * (pos * pos) * (pos * pos)
      | NoInfo

    fun mkPos filename (l1,c1) (l2,c2) = Pos(filename, (l1,c1), (l2,c2))

    val noInfo = NoInfo

    fun getPos(Pos(filename, (l1,c1), (l2,c2))) = SOME (filename, (l1,c1), (l2,c2))
      | getPos(NoInfo) = NONE
	
end
