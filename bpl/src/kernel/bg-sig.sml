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

(** Main BG module, including file I/O and parsing.  For just the
 * BPL data types, see the BG_ADT module.
 * @version $LastChangedRevision$
 *)
signature BG =
sig

  include BG_ADT

  type ruledata = {
    name : string,
    redex : bgterm,
    react : bgterm,
    maps : ((int * Name.name list) * (int * Name.name list)) list,
    info : Info.info}
  (** The kind of contents to parse. *)
  type 'a kind
  (** Signature contents. *)
  val SIGNATURE : control list kind
  (** BgTerm contents. *)
  val BGTERM : bgterm kind
  (** List of rules contents. *)
  val RULES : ruledata list kind

  (** Parse a string as a bigraph term.
   * @params filename s
   * @param filename  File name to use when reporting errors.
   * @param s         The string to be parsed.
   *)
  val parseBgTermStr : string -> string -> bgterm
  (** Parse a string as a kind of contents.
   * @params kind filename s
   * @param kind      The kind of contents to expect when parsing.
   * @param filename  File name to use when reporting errors.
   * @param s         The string to be parsed.
   *)
  val parseStr : 'a kind -> string -> string -> 'a
  (** Read a BG expression from a file print it, and return it as BDNF. *)
  val usefile : string -> B bgbdnf
  (** Read a BG expression from a file, return it as BDNF, explain
   * any errors on stdOut.
   *)
  val usefile' : string -> B bgbdnf
  (** Read a BG expression from a file, return it as BDNF. *)
  val usefile'' : string -> B bgbdnf
  (** Read a BG expression from a file, return it as a bgval, explain
   * any errors on stdOut.
   *)
  val bgvalUsefile' : string -> bgval
  (** Read a BG expression from a file, return it as a bgval. *)
  val bgvalUsefile'' : string -> bgval
  (** Read a bigraphical signature from one XML file and a BRS from
   * another XML file. *)
  val brsUseXMLfiles : string -> string -> control list * ruledata list
  (** Prettyprinter for bigraphs. *)
  val pp : PrettyPrint.ppstream -> 'class bgbdnf -> unit
  (** Return string representation of a normal form bigraph. *)
  val toString : 'class bgbdnf -> string
  (** Return string representation of a bigraph value. *)
  val bgvalToString : bgval -> string

  structure BgTermParser : PARSER
  
  structure RulesParser : PARSER

end
