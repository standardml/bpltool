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

(** Abstract data type for modelling instantiations.
 * @version $LastChangedRevision: 397 $
 *)

signature INSTANTIATION =
sig
  type interface
  type name
  type nameset
  type bgval
  type 'a bgbdnf
  type DR
  
  (** Instantiation type. *)
  type inst

  (** A map specifying which root of the original to copy to
   * a given root of the instance and how to rename the local
   * names of that copy.
   * For instance, the map
   *   1&[y1,y2] |--> 0&[x1,x2]
   * written ((1,[x1,x2]),(0,[y1,y2])), will let root 1 of the instance
   * be a copy of root 0 of the original, where name y1 is renamed to x1,
   * name y2 renamed to x2.
   *)
  type map = (int * name list) * (int * name list)

  (** Signal a logical error in the implementation.
   * 
   * @params errtxt
   * @param errtxt  a description of the problem.
   *)
  exception LogicalError of string

  (** Signal that a local renaming from the local names of  I_i to
   * the local names of J_j cannot be inferred.
   * 
   * @params map I J
   * @param map  the map specifying i and j.
   * @param I    the instantiation innerface.
   * @param J    the instantiation outerface.
   *)
  exception CannotInferLocalRenaming of map * interface * interface

  (** Signal that n occurs more than once in the inner- or outerface of map.
   *
   * @params n map
   * @param n    the name.
   * @param map  the map.
   *)
  exception DuplicateNames of name * map

  (** Signal that n mentioned in map doesn't match the interface i.
   *
   * @params n map i
   * @param n    the name.
   * @param map  the map.
   * @param i    the interface
   *)
  exception NameNotInInterface of name * map * interface

  (** Signal that the namelists of the map has different lengths.
   *
   * @params map
   * @param map the map.
   *)
  exception UnequalNameListLengths of map

  (** Signal that the sites of I and J specified by map, has different
   * numbers of local names.
   *
   * @params map I J
   * @param map  the map.
   * @param I    the instantiation innerface.
   * @param J    the instantiation outerface.
   *)
  exception IncompatibleSites of map * interface * interface

  (** Signal that map has an incomplete renaming wrt. the interfaces
   * I and J: the names in X and Y are not mentioned in the renaming.
   *
   * @params X Y map I J
   * @param X    the set of local names from the site of I that are
   *             missing.
   * @param Y    the set of local names from the site of J that are
   *             missing.
   * @param map  the map.
   * @param I    the innerface of the instantiation.
   * @param J    the outerface of the instantiation.
   *)
  exception IncompleteRenaming
  of nameset * nameset * map * interface * interface

  (** Signal that the site number s in map is invalid wrt. interface i.
   * 
   * @params map s i
   * @param map  the map.
   * @param s    the site number.
   * @param i    the interface.
   *)
  exception InvalidSiteNumber of map * int * interface

  (** Signal that a reactum site s is mentioned more than once in a map
   * list maps.
   * 
   * @params s maps
   * @param s     reactum site number.
   * @param maps  the map list.
   *)
  exception DuplicateEntries of int * map list

  (** Signal that interface i has global names.
   *
   * @params i
   * @param i  the interface.
   *)
  exception NonLocalInterface of interface

  (** Construct an instantitation.   For instance,
   * [1&[y1,y2] |--> 0&[x1,x2], ...]
   * make I J [((1,[y1,y2]),(0,[x1,x2]))] will let root 1 of the instance
   * be a copy of root 0 of the original, where name x1 is renamed to y1,
   * name x2 renamed to y2, and all other variables and roots will be
   * copies of the corresponding entities of the original.
   * 
   * @params I J maps
   * @param I     the innerface of the instantiation.
   * @param J     the outerface of the instantiation.
   * @param maps  the non-trivial maps from sites of J to sites of I.
   *              Mising information is attempted inferred in the
   *              following way:
   *              <ol>
   *                <li> if a site of J is not mentioned, it is assumed to
   *                     map to the same site at I, inferring the name map
   *                     as in (2).
   *                <li> if the name lists of a map are empty, the local
   *                     renaming will be inferred as follows:
   *                     <ol>
   *                       <li> if the relevant sites of I and J has the same
   *                            local names, an identity renaming will be used.
   *                       <li> otherwise, if there is only one local name at
   *                            both sites, say x at I_i and y at J_j, the
   *                            local renaming (y)/(x) will be used.
   *                     </ol>
   *              </ol>
   * @exception CannotInferLocalRenaming  if a local renaming cannot
   *                                      be inferred.
   * @exception DuplicateNames            if a name occurs more than once in
   *                                      a map.
   * @exception NameNotInInterface        if a name in a map doesn't match
   *                                      the relevant interface.
   * @exception UnequalNameListLengths    if the name lists of a map has
   *                                      different lengths.
   * @exception IncompatibleSites         if a map is specified between two
   *                                      incompatible sites.
   * @exception IncompleteRenaming        if a map only specifies a partial
   *                                      renaming wrt. I and J.
   * @exception InvalidSiteNumber         if a map contains an invalid site
   *                                      number.
   * @exception DuplicateEntries          if a reactum site number occurs more
   *                                      than once in maps.
   * @exception NonLocalInterface         if I or J are not local.
   *)
  val make : {I : interface, J : interface, maps : map list} -> inst
  (** Construct an instantitation.
   * Must be equivalent to make i1 i2 []
   * @see make.
   *)
  val make' : {I : interface, J : interface} -> inst
  (** Deconstruct an instantiation.
   * @see make.
   *)
  val unmk : inst -> {I : interface, J : interface, maps : map list}

  (** Signal that the parameter d does not match the innerface of
   * the instantiation inst.
   * 
   * @params inst d
   * @param inst  the instantiation.
   * @param d     the parameter.
   *)
  exception IncompatibleParameter of inst * DR bgbdnf

  (** Instantiate a parameter d using instantiation inst.
   *
   * @params inst d
   * @param inst  the instantiation.
   * @param d     the parameter.
   * @exception IncompatibleParameter  if the outerface of the parameter
   *                                   does not match the innerface of the
   *                                   instantiation.
   *)
  val instantiate : inst -> DR bgbdnf -> bgval
  (** Prettyprint a map.
   * @params indent pps map
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param map     The map to print.
   *)
  val pp_map : int -> PrettyPrint.ppstream -> map -> unit
  (** Prettyprint an instantiation.
   * @params indent pps inst
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param inst    The instantiation to print.
   *)
  val pp : int -> PrettyPrint.ppstream -> inst -> unit
  (** Return a prettyprinted string representation of a instantiation. *)
  val toString : inst -> string

end