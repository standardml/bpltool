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
 * @version $LastChangedRevision: 1353 $
 *)

functor Geometry2D (
  type num
  val zero : num
  val real : num -> real
  val fromReal : real -> num
  val compare : num * num -> order
  val -- : num * num -> num
  val ++ : num * num -> num
  val ** : num * num -> num
  val == : num * num -> bool)
  :> GEOMETRY2D where type num = num =
struct
  
  type num = num
  
  infix 4 << <<= >> >>= == ===
  
  fun n1 << n2  = compare (n1, n2) =  LESS
  fun n1 <<= n2 = compare (n1, n2) <> LESS
  fun n1 >> n2  = compare (n1, n2) =  GREATER
  fun n1 >>= n2 = compare (n1, n2) <> GREATER
  
  type vec = num * num
  
  val origo = (zero, zero)

  fun lt (x1, y1) (x2, y2) =
    case compare (x1, x2) of
      EQUAL => (case compare (y1, y2) of LESS => true | _ => false)
    | LESS => true
    | GREATER => false
  
  fun fromRealVec (x, y) = (fromReal x, fromReal y)
  
  structure VecSet = OrderSet (
    type T = num * num
    val lt = lt
  )
  
  infix 7 *** ** mul div
  infix 6 +++ ++
  infix 6 --- --
  
  fun (x1, y1) +++ (x2, y2) = (x1 ++ x2, y1 ++ y2)
  
  fun ((x1, y1):vec) --- (x2, y2) = (x1 -- x2, y1 -- y2)
  
  fun (x1, y1) *** (x2, y2) = x1 ** x2 ++ y1 ** y2
  
  fun (x1, y1) === (x2, y2) = x1 == x2 andalso y1 == y2
  
  fun (x, y) mul r = (real x * r, real y * r)

  fun -|- (x, y) = (zero -- y, x)
  
  val sqrt = Math.sqrt
  
  fun length (x, y) =
    let
      val (x, y) = (real x, real y)
    in
      sqrt (x * x + y * y)
    end
  
  fun norm (x, y) =
    if (x, y) === origo then
      (0.0, 0.0)
    else
      let val l = length (x, y) in (real x / l, real y / l) end

  (** Compute the length of the projection of the normalised vectors. *)
  fun projlen v1 v2 =
    let
      val (x1, y1) = norm v1
      val (x2, y2) = norm v2
    in
      x1 * x2 + y1 * y2
    end

  (** Compute the convex hull using a simple algorithm. *)
  (* 1 Sort vectors lexicographically, x most significant, removing duplicates.
   * 2 If |vectors| < 3, return this list.
   * 3 For the least vector v0, let
   *   uppers := [v0]
   *   lowers := [v0].
   * 4 For each remaining vector v = (x, y),
   *   4a while |lowers| > 1 and the angle of (v - head lowers) is <= than
   *        the last lowerangle, lowers := tail lowers.
   *   4b While |uppers| > 1 and the angle of (v - head uppers) is >= than
   *      the last upperangle, uppers := tail uppers.
   *   4c Cons v onto uppers and lowers.
   * 5 Return reverse (init uppers) @ tail lowers.
   * Note: reverse is not necessary, but provides tail recursion.
   *)
  fun convexhull vs =
    case (VecSet.list o VecSet.fromList) vs of
      v0 :: (vs as (_ :: _ :: _)) =>
      let
        fun addpt v _ [] = [(v : vec, origo)]
          | addpt (v:vec) _ (vas as [(v':vec, _)]) = (v, v --- v') :: vas
          | addpt v le (vas as ((v', a') :: vas')) =
          let
            val a = v --- v'
            val aT = -|- a
          in
            if le (aT *** a', zero) then
              addpt v le vas'
            else
              (v, a) :: vas
          end
        fun addpoint (v, (uppers, lowers)) =
          (addpt v (op >>=) uppers, addpt v (op <<=) lowers)
        val (uppers, lowers) =
          foldl addpoint ([(v0, origo)], [(v0, origo)]) vs 
        fun revinitapp [] ys = ys
          | revinitapp [_] ys = ys
          | revinitapp (x :: xs) ys = revinitapp xs (x :: ys)
      in
        map #1 (revinitapp uppers (tl lowers))
      end
    | vs => vs
  (** Compute the circle touching the three points.  If the points
   * lie on a straight line, return the circle touching the outermost
   * points.
   *)
  (* Solve the linear equation defining the point of intersection
   * between the bisectors of a-b and a-c.  If the determinant is
   * zero, the points lie on a straight line.
   *)
  fun touchingcircle (a as (ax, ay)) (b as (bx, by)) (c as (cx, cy)) =
    let
      val bay = by -- ay
      val cax = cx -- ax
      val abx = ax -- bx
      val acy = ay -- cy
      val d = bay ** cax -- abx ** acy (* determinant *)
    in
      if d == zero then (* a, b and c on line *)
        let
          val ab = sqrt (real (abx ** abx ++ bay ** bay))
          val ac = sqrt (real (cax ** cax ++ acy ** acy))
          val bcx = bx -- cx
          val bcy = by -- cy
          val bc = sqrt (real (bcx ** bcx ++ bcy ** bcy))
          val (v1, v2, r) =
            if ab < ac then
              if bc < ac then (a, c, ac) (* ac longest *)
              else (b, c, bc) (* bc longest *)
            else
              if bc < ab then (a, b, ab) (* ab longest *)
              else (b, c, bc) (* bc longest *)
          val (x, y) = (v1 +++ v2) mul 0.5
        in
          ((fromReal x, fromReal y), fromReal (r + 0.5))
        end
      else
        let
          val d    = real d
          val bay  = real bay
          val cax  = real cax
          val acy  = real acy
          val abx  = real abx
          val cbxh = real (cx -- bx) / 2.0
          val cbyh = real (cy -- by) / 2.0
          val t = (cbxh * cax - cbyh * acy) / d
          val (x, y) = (real (ax ++ bx) * 0.5 + t * bay,
                        real (ay ++ by) * 0.5 + t * abx)
          val dx = x - real ax
          val dy = y - real ay
        in
          ((fromReal x, fromReal y),
           fromReal (sqrt (dx * dx + dy * dy) + 0.5))
        end 
    end
  (** Compute the minimal enclosing circle using a simple algorithm
   * of cubic complexity (a linear algorithm exist).
   *)
  (* 1 Compute the convex hull vs of the points.
   * 2 If vs = [v1,v2] return ((v1+v2)/2, |v1-v2|/2).
   * 3 Pick a pair (v1,v2) from vs.
   * 4 Find v in rest of vs with least angle a at vertex v in the
   *   triangle v-v1-v2.
   * 5 If a >= 90° return ((v1+v2)/2, |v1-v2|/2).
   * 6 If v-v1-v2 is obtuse, set (v1, v2) to the two nonobtuse
   *   corners, and repeat from step 4.
   * 7 If v-v1-v2 is nonobtuse, return touching circle of [v,v1,v2].
   *)
  fun minenclosingcircle vs =
    case convexhull vs of
      [v] => (v, zero)
    | [v1, v2] =>
      (fromRealVec ((v1 +++ v2) mul 0.5), fromReal (length (v1 --- v2) * 0.5))
    | (v1 :: v2 :: vs) => 
      let (* Least angle ~ greatest normalised projection length *)
        fun minangle' _ _ lvs [] max = (max, lvs)
          | minangle' v1 v2 lvs (v :: rvs) (vmax, lmax) =
          let
            val l = projlen (v2 --- v) (v1 --- v)
          in
            if l > lmax then
              minangle' v1 v2 (vmax :: lvs) rvs (v, l)
            else
              minangle' v1 v2 (v :: lvs) rvs (vmax, lmax)
          end
        fun minangle v1 v2 (v3 :: vs) =
          minangle' v1 v2 [] vs (v3, projlen (v2 --- v3) (v1 --- v3))
          
        fun doside v1 v2 vs =
          let
            val ((v, l), vs') = minangle v1 v2 vs
          in
            if l <= 0.0 then (* a >= 90° *)
              (fromRealVec ((v1 +++ v2) mul 0.5),
               fromReal (length (v1 --- v2) * 0.5))
            else if projlen (v2 --- v1) (v --- v1) < 0.0 then
              doside v v2 (v1 :: vs') (* v1 obtuse *)
            else if projlen (v1 --- v2) (v --- v2) < 0.0 then
              doside v v1 (v2 :: vs') (* v2 obtuse *)
            else
              touchingcircle v v1 v2
          end
      in
        doside v1 v2 vs
      end
end