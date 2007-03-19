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

(** Partioning of lists into m groups where the order
 * of the groups is significant. Groups might be empty
 * but the order of empty groups is not significant.*)
signature ORDERED_PARTITION =
sig
  (** A partition generator type. *)
  type 'a opartitiongen

  (** Signal that there are no more partitions. *)
  exception NoPartitions

  (** Create a partition generator which generates all partitions
   * of list into m groups where the order of the groups is
   * significant. Groups might be empty but the order of empty
   * groups is not significant.
   *
   * @params list m
   * @param list  the list to partition.
   * @param m     the number of groups in a partition.
   * @return a partition generator
   * @exception NoPartitions if it is impossible to partition
   *                         list into m groups.
   *)
  val make : 'a list -> int -> 'a opartitiongen

  (** Get the next partition from a partition generator.
   * @params part_gen
   * @param part_gen  the partition generator.
   * @return a partition of the list given to make
   * @exception NoPartitions if no more partitions are available.
   *)
  val next : 'a opartitiongen -> 'a list list

  (** Get the next partition where each sublist has at least one element.
   * @params part_gen
   * @param part_gen  the partition generator.
   * @return a partition of the list given to make
   * @exception NoPartitions if no more partitions are available.
   *)
  val next' : 'a opartitiongen -> 'a list list
end
