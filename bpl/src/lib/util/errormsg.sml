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
    val toCoords : int -> int -> ((int * int) * (int * int))
    val error : int -> int -> string -> unit
    val getErrors
      : unit
        -> (string * ((int * int) * (int * int)) * (int * int) * string)
           list
    exception Error
(*    val impossible : string -> 'a   (* raises Error *)*)
    val reset : unit -> unit
end
structure ErrorMsg : ERRORMSG =
struct
  val anyErrors = ref false
  val fileName = ref ""
  val lineNum = ref 1
  val linePos = ref [1]
  val sourceStream = ref TextIO.stdIn
  val errors
    : (string * ((int * int) * (int * int)) * (int * int) * string)
      list ref
    = ref []
  fun reset() =
    (anyErrors:=false;
     errors := [];
		 fileName:="";
		 lineNum:=1;
		 linePos:=[1];
		 sourceStream:=TextIO.stdIn)
  exception Error
  (* Translates 1D character positions to 2D ditto. *)
  fun toCoords leftpos rightpos =
      let fun lookleft (rightline, rightcol) =
	      let
		fun lookleft' (a :: rest, n) =
		    if a < leftpos then
		      ((n, leftpos - a), (rightline, rightcol))
		    else 
		      lookleft' (rest, n - 1)
		  | lookleft' _
		    = ((~1,~1), (rightline, rightcol))
	      in
		lookleft'
	      end
	  fun look (a :: rest, n) =
		if a < rightpos then
		  lookleft (n, rightpos - a) (a :: rest, n)
		else 
		  look (rest, n - 1)
	    | look _ = ((~1,~1), (~1,~1))
      in
	look(!linePos,!lineNum)
      end

  (* Accumulates reported errors. *)
  fun error leftpos rightpos (msg:string) =
    (errors
      := (!fileName, toCoords leftpos rightpos, (leftpos, rightpos), msg)
          :: !errors;
     anyErrors := true)

  fun impossible msg =
    (errors
      := (!fileName, ((~1, ~1), (~1, ~1)), (~1, ~1),
          "Compiler bug: " ^ msg)
          :: !errors;
     anyErrors := true)

  fun getErrors () = rev (!errors)

(*  fun error leftpos rightpos (msg:string) =
      let fun print s = TextIO.output(TextIO.stdErr, s) (* Henning *)
	  fun printPos () =
              case toCoords leftpos rightpos of
                ((~1, ~1), _) => print ":0.0"
              | ((leftline, leftcol), (rightline, rightcol)) =>
                  app print [":",
                             Int.toString leftline,
                             ".",
                             Int.toString leftcol,
                             "-",
                             Int.toString rightline,
                             ".",
                             Int.toString rightcol]
       in anyErrors := true;
	  print (!fileName);
          printPos();
	  print " Error: ";
	  print msg;
	  print "\n"
      end
  fun impossible msg =
      (app print ["Error: Compiler bug: ",msg,"\n"];
       TextIO.flushOut TextIO.stdOut;
       raise Error)*)
end  (* structure ErrorMsg *)
  
