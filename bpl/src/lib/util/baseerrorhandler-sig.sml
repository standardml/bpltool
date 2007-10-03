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

(** Error handling.
 * @version $LastChangedRevision$
 *)
 
signature BASEERRORHANDLER =
sig
  type ppstream
  eqtype break_style
  type origin

  (** An error explanation. *)
  datatype explanation
    = Exp of Debug.debug_level * origin * (int -> ppstream -> unit)
             * explanation list

  (** Prettyprint an explanation regardless of the current debug level. *)
  val pp' : int -> ppstream -> explanation -> unit

  (** Prettyprint an explanation subject to the current debug level. *)
  val pp : int -> ppstream -> explanation -> unit

  (** Utility prettyprinter which doesn't print anything. *)
  val pp_nothing : int -> ppstream -> unit

  (** Utility function for combining a sequence of prettyprinters.
    * The result is wrapped in a new block using the given break style
    * and a break is added between each prettyprinter.
    * @params bs pps
    * @param bs   break style
    * @param pps  the prettyprinters to combine
    *)
  val concat_pps
      : break_style -> (int -> ppstream -> unit) list -> (int -> ppstream -> unit)

  (** Utility function for creating a prettyprinter for a list.
    * @params ldelim rdelim sep pp list
    * @param  ldelim  the left delimiter of the list
    * @param  rdelim  the right delimiter of the list
    * @param  sep     the separator to place between elements
    * @param  pp      the prettyprinter for elements
    * @param  list    the list to prettyprint
    *)
  val mk_list_pp'
      : string -> string -> string
        -> (int -> ppstream -> 'data -> unit)
        -> (int -> ppstream -> 'data list -> unit)

  (** Utility function for creating a prettyprinter for a list.
    * @params ldelim rdelim sep pp list
    * @param  ldelim  the left delimiter of the list
    * @param  rdelim  the right delimiter of the list
    * @param  sep     the separator to place between elements
    * @param  pp      the prettyprinter for elements
    * @param  list    the list to prettyprint
    *)
  val mk_list_pp
      : string -> string -> string
        -> (int -> ppstream -> 'data -> unit) -> 'data list
        -> (int -> ppstream -> unit)

  (** Utility function for creating a prettyprinter from a string. *)
  val mk_string_pp : string -> (int -> ppstream -> unit)

  (** Utility function for creating a prettyprinter from an integer. *)
  val mk_int_pp : int -> (int -> ppstream -> unit)

  (** Utility function for wrapping a pretty printer
      with the data to be printed. *)
  val pack_pp_with_data
      : (int -> ppstream -> 'data -> unit) -> 'data -> (int -> ppstream -> unit)
      
  (** Add an error explainer. *)
  val add_explainer : (exn -> explanation) -> unit

  (** Explain an exception.
      Raises the given exception if there is no explanation. *)
  val explain'' : exn -> explanation

  (** Explain an exception.
      Raises the given exception if there is no explanation. *)
  val explain' : exn -> string

end
