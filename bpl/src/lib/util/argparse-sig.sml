(*
 *   Copyright 2001 Henning Makholm
 *   Copyright 2001 Henning Niss
 * 
 * Permission is hereby granted, to anyone and free of charge, to deal
 * in this software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the software, provided that
 * all copies or substantial portions of the software are accompanied
 * by the above copyright notice and this permission notice.
 * 
 * The software is provided "as is", without warranty of any kind, express
 * or implied, including but not limited to the warranties of
 * merchantability, fitness for a particular purpose and noninfringement.
 * In no event shall the above listed copyright holder(s) be liable for any
 * claim, damages or other liability, whether in an action of contract,
 * tort or otherwise, arising from, out of or in connection with the
 * software or the use or other dealings in the software.
 *)

(** Parsing of command line arguments.
 * <p>
 * This module provides a general mechanism for extracting options and
 * arguments from the command line to the program. 
 * <p>
 * <b>Syntax of command lines.</b>
 * <p>
 * A keyword is a character string starting with a <code>-</code>.  An
 * option is a keyword alone or followed by an argument.  There are 4
 * types of keywords: <code>Unit</code>, <code>String</code>,
 * <code>Int</code>, and <code>Float</code>.  <code>Unit</code>
 * keywords do not take an argument.  <code>String</code>,
 * <code>Int</code>, and <code>Float</code> keywords take the
 * following word on the command line as an argument.  Arguments not
 * preceded by a keyword are called anonymous arguments.
 * <p>
 * Examples (<code>foo</code> is assumed to be the command name):
 * <table>
 * <tr><td><code>foo -flag           </td><td>(a unit option)</td></tr>
 * <tr><td><code>foo -int 1          </td><td>(an int option with argument <code>1</code>)</td></tr>
 * <tr><td><code>foo -string foobar  </td><td>(a string option with argument <code>"foobar"</code>)</td></tr>
 * <tr><td><code>foo -real 12.34     </td><td>(a real option with argument <code>12.34</code>)</td></tr>
 * <tr><td><code>foo 1 2 3           </td><td>(three anonymous arguments: <code>"1"</code>, <code>"2"</code>, 
 *                             and <code>"3"</code>)</td></tr>
 * <tr><td><code>foo 1 2 -flag 3 -string bar 4</td><td>
 *                            (four anonymous arguments, a unit option, 
 *                             and a string option with argument <code>"bar"</code>)</td></tr>
 * </table>
 * <p>
 * For keywords that consists of <code>-</code> and a single letter,
 * the behavior of standard Unix <code>getopt()</code> is emulated to
 * some degree (i.e. if the argument takes an option the space before
 * it is optional, and otherwise the space+dash arfter it is optional
 * if more arguments follow.
 * <p>
 * If one of the command line arguments is <code>--</code> the rest of the
 * command line is treated as anonymous arguments even if they
 * start with <code>-</code>.
 *
 * @author Peter Sestoft (taken from the Moscow ML compiler).
 * @contributor Henning Niss (minor modifications) <hniss@itu.dk>.<br/>
 * @contributor Henning Makholm (minor modifications) <henning@makholm.net>.
*)

signature ArgParse = (* From the MosML compiler. *)
sig

    datatype spec =
        (** A string keyword. *)
	String  of (string -> unit)
        (** An int keyword. *)
      | Int     of (int -> unit)
        (** A unit keyword; no argument. *)
      | Unit    of (unit -> unit)
        (** A real keyword. *)
      | Real    of (real -> unit)

   (** Parses the command line or a list of arguments.
    * When called,  parses the command line
    * if args is NONE, otherwise the list of arguments supplied in
    * args, calling the functions in speclist whenever appropriate,
    * and anonfun on anonymous arguments.  The functions are called
    * in the same order as they appear on the command line.  The
    * strings in the speclist are keywords and must
    * start with a <code>-</code>, else they are ignored. Functions in
    * speclist or anonfun can raise Bad to reject invalid
    * arguments.
    * @params args speclist anonfun
    * @param args NONE to parse the command line, otherwise SOME(argument list)
    * @param speclist list of argument specifications
    * @param anonfun function invoked on anonymous arguments
    * @exception Bad raised if the command line or the list of arguments cannot be parsed.
    *)
    val parse : string list option
	      -> (string * spec) list -> (string -> unit) -> unit;

    (** Signals that the list of arguments cannot be parsed.
      *)
    exception Bad of string

end
