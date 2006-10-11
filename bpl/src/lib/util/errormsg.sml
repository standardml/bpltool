(* Error-message func's used in lexer and parser. 
 * From Appel's code for "Modern Compiler Implementation in ML"
 * 
 * Main values of interest are 
 * filename - string ref : name of file being parsed
 * lineNum - int ref : current linenumber
 * linePos - int list ref : list of positions of line-endings
 * 
 *)
signature ERRORMSG =
sig
    val anyErrors : bool ref
    val fileName : string ref
    val lineNum : int ref
    val linePos : int list ref
    val sourceStream : TextIO.instream ref
    val error : int -> int -> string -> unit
    exception Error
    val impossible : string -> 'a   (* raises Error *)
    val reset : unit -> unit
end
structure ErrorMsg : ERRORMSG =
struct
  val anyErrors = ref false
  val fileName = ref ""
  val lineNum = ref 1
  val linePos = ref [1]
  val sourceStream = ref TextIO.stdIn
  fun reset() = (anyErrors:=false;
		 fileName:="";
		 lineNum:=1;
		 linePos:=[1];
		 sourceStream:=TextIO.stdIn)
  exception Error
  fun error leftpos rightpos (msg:string) =
      let fun print s = TextIO.output(TextIO.stdErr, s) (* Henning *)
	  fun lookleft (rightline, rightcol) =
	      let
		fun lookleft' (a :: rest, n) =
		    if a < leftpos then
		      (app print [":",
				  Int.toString n,
				  ".",
				  Int.toString (leftpos - a),
				  "-"];
		       app print [Int.toString rightline, "."];
		       print (Int.toString rightcol))
		    else 
		      lookleft' (rest, n - 1)
		  | lookleft' _
		    = app print [Int.toString rightline, ".",
				 Int.toString rightcol]
	      in
		lookleft'
	      end
	  fun look (a :: rest, n) =
		if a < rightpos then
		  lookleft (n, rightpos - a) (a :: rest, n)
		else 
		  look (rest, n - 1)
	    | look _ = print ":0.0"
       in anyErrors := true;
	  print (!fileName);
	  look(!linePos,!lineNum);
	  print " Error: ";
	  print msg;
	  print "\n"
      end
  fun impossible msg =
      (app print ["Error: Compiler bug: ",msg,"\n"];
       TextIO.flushOut TextIO.stdOut;
       raise Error)
end  (* structure ErrorMsg *)
  
