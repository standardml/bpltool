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

(** Runtime flags also settable via command line switches.
 * @version $LastChangedRevision$
 *)
signature FLAGS = sig

  (** Flag type.
   * @param name    Flag name, e.g. "/kernel/ast/bgterm/hideids" or "/dump/bgvals".
   * @param desc    Human readable description of the flag.
   * @param short   One-letter command line switch, e.g. "o".
   * @param long    Long version of the command line switch, e.g., "output-file".
   * @param arg     Optional argument name, e.g. N or FILENAME.
   * @param default Default flag value.
   *)
    type 'a flag_info 
      = {name:string,desc:string,
	 short:string,long:string,arg:string,
	 default:'a}

  (** Signal that a given flag name does not refer to an existing
   * flag in the current scope.
   *)
    exception FlagNotFound

    val makeIntFlag : int flag_info -> int ref
    val makeRealFlag : real flag_info -> real ref
    val makeBoolFlag : bool flag_info -> bool ref
    val makeStringFlag : string flag_info -> string ref

    val setIntFlag : string -> int -> unit
    val getIntFlag : string -> int
    val setRealFlag : string -> real -> unit
    val getRealFlag : string -> real
    val setBoolFlag : string -> bool -> unit
    val getBoolFlag : string -> bool
    val setStringFlag : string -> string -> unit
    val getStringFlag : string -> string

    (** List the default value for each flag. *)
    val listDefaults : TextIO.outstream -> unit
    (** List the current value for each flag if not default. *)
    val listChanged : TextIO.outstream -> unit
    (** Return a description of command line switches. *)
    val usage : unit -> string list
    (** Create an ArgParse specification from the current set of flags. *)
    val toSpec : unit -> (string * ArgParse.spec) list

end (* signature FLAGS *)
