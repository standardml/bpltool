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

(** Error handler which prints to std out.
 * @version $LastChangedRevision$
 *)
 
structure PrintErrorHandler
  :> ERRORHANDLER
     where type origin      = Origin.origin
       and type ppstream    = PrettyPrint.ppstream
       and type break_style = PrettyPrint.break_style
  =
struct
  open BaseErrorHandler
  open Debug

  fun explain e =
      let
        fun print_exn_hist () =
            case Exception.history e of
              [] => print "\n\nException history unavailable.\n"
            | es => (print "\n\nException history:\n";
                     app (fn e => (print "  ";
                                   print (Exception.event2string e);
                                   print "\n")) es;
                     print "\n")
      in
        (print "Error: ";
         print (explain' e);
         debug LVL_LOW print_exn_hist ())
      end handle e => raise e
end
