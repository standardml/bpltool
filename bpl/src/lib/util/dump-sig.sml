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

(** Utilities for dumping prettyprinter output to disk.
 * @author Henning Niss, IT University of Copenhagen <hniss@itu.dk>
 * @version $LastChangedRevision: 129 $
 *)
signature DUMP = sig

    (** A flag controlling the path prefix to be used by the
      * dump functions. The flag can also be controlled through the
      * flag <code>/dump/prefix</code> (see 
      * <a href="SigFLAGS.html">FLAGS</a>).
      *)
    val prefix : string ref
    (** Sets the dump prefix to the supplied filename with the extension
      * removed.
      * @params filename
      * @param filename the filename
      *)
    val setPrefix : string (* filename *) -> unit
    (** Dumps pretty printer output. The output is sent to the file
      * <code>! prefix ^ "/" ^ ext</code>.
      * @params pp ext
      * @params pp pretty printer tree
      * @params ext filename extension
      *)
    val pretty : 'a Pretty.pp -> string (* extension *) -> 'a -> unit
    (** Dumps pretty printer output. The output is sent to the file
      * <code>! prefix ^ "/" ^ ext</code>.
      * @params pp ext
      * @params pp function producing pretty printer output
      * @params ext filename extension
      *)
    val pp : (int -> PrettyPrint.ppstream -> 'a -> unit)
                     -> string (* extension *) -> 'a -> unit

end (* signature DUMP *)
