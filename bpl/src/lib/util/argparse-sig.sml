(**
 *
 * Parsing of command line arguments.
 *
 * This module provides a general mechanism for extracting options and
 * arguments from the command line to the program. 
 *
 * Syntax of command lines.
 *  
 * A keyword is a character string starting with a [-].  An option is
 * a keyword alone or followed by an argument.  There are 4 types of
 * keywords: Unit, String, Int, and Float.  Unit keywords do not take
 * an argument.  String, Int, and Float keywords take the following
 * word on the command line as an argument.  Arguments not preceded by
 * a keyword are called anonymous arguments.
 *
 * Examples ([foo] is assumed to be the command name):
 * <pre>
 *  -  [foo -flag           ] (a unit option)
 *  -  [foo -int 1          ] (an int option with argument [1])
 *  -  [foo -string foobar  ] (a string option with argument ["foobar"])
 *  -  [foo -real 12.34     ] (a real option with argument [12.34])
 *  -  [foo 1 2 3           ] (three anonymous arguments: ["1"], ["2"], 
 *                             and ["3"])
 *  -  [foo 1 2 -flag 3 -string bar 4]
 *                            (four anonymous arguments, a unit option, 
 *                             and a string option with argument ["bar"])
 * </pre>
 *
 * For keywords that consists of [-] and a single letter, the behavior
 * of standard Unix getopt() is emulated to some degree (i.e. if the
 * argument takes an option the space before it is optional, and
 * otherwise the space+dash arfter it is optional if more arguments
 * follow.
 *		 
 * If one of the command line arguments is "--" the rest of the
 * command line is treated as anonymous arguments even if they
 * start with [-].
 *
 * @author From the Moscow ML compiler.
 * @author Henning Niss (minor modifications) <hniss@itu.dk>.
*)

signature ArgParse = (* From the MosML compiler. *)
sig

    datatype spec =
	String  of (string -> unit)
      | Int     of (int -> unit)
      | Unit    of (unit -> unit)
      | Real    of (real -> unit)

   (** [parse args speclist anonfun] parses the command line if args is NONE,
    * otherwise the list of arguments supplied in args, calling the
    * functions in [speclist] whenever appropriate, and [anonfun] on
    * anonymous arguments.  The functions are called in the same order as
    * they appear on the command line.  The strings in the [(string *
    * spec) list] are keywords and must start with a [-], else they are
    * ignored. Functions in [speclist] or [anonfun] can raise [Bad
    * message] to reject invalid arguments.
    *)
    val parse : string list option
	      -> (string * spec) list -> (string -> unit) -> unit;

    exception Bad of string

end
