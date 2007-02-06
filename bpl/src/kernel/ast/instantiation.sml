(* Copyright (c) 2007  The BPL Group at the IT University of Copenhagen
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

(** Datatype for representing instantiations.
 * @version $LastChangedRevision: 397 $
 *)

functor Instantiation (structure BgVal : BGVAL
                       structure BgBDNF : BGBDNF) =
struct
  type bgval = BgVal.bgval
  type 'a bgbdnf = 'a BgBDNF.bgbdnf
  type DR = BgBDNF.DR
  type name = unit
  (** FIXME: Instantiation type. *)
  type inst = unit
  (** FIXME: Construct an instantiation. *)
  fun make () = ()
  fun make' _ = ()
  (** FIXME: Deconstruct an instantiation. *)
  fun unmk () = ()
  (** FIXME: *)
  val id : inst = ()
  (** FIXME: Use an instantiation to instantiate a bgval. *)
  fun instantiate inst d = d
  (** FIXME: Prettyprint an instantiation.
   * @params indent pps inst
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param inst    The instantiation to print.
   *)
  fun pp indent pps inst = ()

  fun toString i
    = PrettyPrint.pp_to_string
        (Flags.getIntFlag "/misc/linewidth") 
        (pp (Flags.getIntFlag "/misc/indent")) i
end