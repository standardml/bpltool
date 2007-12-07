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

(** Two dimensional geometry utilities.
 * The x axis points right, and the y axis points upwards.
 * @version $LastChangedRevision: 1353 $
 *)

signature GEOMETRY2D =
sig
  (** Number type used for vector vecinates etc. *)
  type num
  (** Vector type. *)
  type vec = num * num
  (** Vector addition. *)
  val +++ : vec * vec -> vec
  (** Vector subtraction. *)
  val --- : vec * vec -> vec
  (** Inner product. *)
  val *** : vec * vec -> num
  (** Perpendicular vector. *)
  val -|- : vec -> vec
  (** Vector length. *)
  val length : vec -> real
  (** Normalised vector. *)
  val norm : vec -> real * real
  (** Compute the convex hull.
   * @return  The vecinates that define the convex hull of
   *          the given coordinates.
   *)
  val convexhull : vec list -> vec list
  (** Compute the minimal enclosing circle.
   * @return (c, r) where c is the centre, and r the radius.
   *)
  val minenclosingcircle : vec list -> vec * num
end