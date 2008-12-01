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

(** Abstract data type for bigraph terms.
 * The terms are not necessarily well-formed:
 * scope rule may be violated in abstractions, names may clash in
 * interfaces of tensor product operands, bigraph widths may be
 * incompatible in compositions, etc.
 * <p>
 * Each constructor takes an info argument that can contain contextual
 * information (e.g., source file location for the term).
 * @version $LastChangedRevision$
 *)
functor BgTerm'(structure Info : INFO
                 structure Link : LINK
                 structure LinkSet : MONO_SET
                 structure Ion : ION
                 structure Control : CONTROL
                 structure Wiring : WIRING
                 structure Permutation : PERMUTATION
                 structure Name : NAME
                 structure NameSet : MONO_SET
                 structure NameSetPP : COLLECTIONPRETTYPRINT
                   where type ppstream    = PrettyPrint.ppstream
                 structure ErrorHandler : ERRORHANDLER
                   where type ppstream    = PrettyPrint.ppstream
                     and type break_style = PrettyPrint.break_style
                     and type origin      = Origin.origin
                 sharing type Name.name =
                              NameSet.elt =
                              Ion.name =
                              Link.name =
                              Wiring.name
                 sharing type Link.link = LinkSet.elt
                 sharing type LinkSet.Set = Wiring.linkset
                 sharing type NameSet.Set =
                              Name.NameSet.Set =
                              Ion.nameset =
                              Link.nameset =
                              Permutation.nameset =
                              Wiring.nameset =
                              NameSetPP.collection
                 sharing type Ion.control = Control.control)
      : BGTERM
          where type control = Control.control =
struct
  open Debug
  open ErrorHandler
  val file_origin = Origin.mk_file_origin
                      "$BPL/src/kernel/ast/bgterm.sml"
                      Origin.NOPOS
  fun mk_explainer errtitle (explainer : exn -> explanation list) e =
      Exp (LVL_USER, Origin.unknown_origin, mk_string_pp errtitle,
           explainer e)

  val _ = Flags.makeBoolFlag {
    name = "/kernel/ast/bgterm/ppids",
    desc = "Explicitly display identities in tensor and parallel products",
    short = "",
    long = "--ppids",
    arg = "",
    default = true}
  val _ = Flags.makeBoolFlag {
    name = "/kernel/ast/bgterm/ppabs",
    desc = "Explicitly display abstractions \
           \(abstractions on roots are always displayed)",
    short = "",
    long = "--ppabs",
    arg = "",
    default = true}
  val _ = Flags.makeBoolFlag {
    name = "/kernel/ast/bgterm/pp0abs",
    desc = "Explicitly display empty-set abstractions \
           \(ignored if ppabs is false)",
    short = "",
    long = "--pp0abs",
    arg = "",
    default = true}
  val _ = Flags.makeBoolFlag {
    name = "/kernel/ast/bgterm/pptenaspar",
    desc = "Replace tensor product with parallel product",
    short = "",
    long = "--pptenaspar",
    arg = "",
    default = false}
  val _ = Flags.makeBoolFlag {
    name = "/kernel/ast/bgterm/ppmeraspri",
    desc = "Replace merge with prime product (best effort)",
    short = "",
    long = "--ppmeraspri",
    arg = "",
    default = false}
  type info = Info.info
  type nameset = NameSet.Set
  type control = Control.control
  type ion = Ion.ion
  type Immutable = Permutation.Immutable
  type 'kind permutation = 'kind Permutation.permutation
  type wiring = Wiring.wiring

  datatype bgterm = 
	   Mer of int * info
	 | Con of nameset * info
	 | Wir of wiring * info
	 | Ion of ion * info
	 | Hop of bgterm * info
	 | Per of Immutable permutation * info
	 | Abs of nameset * bgterm * info
	 | Ten of bgterm list * info
	 | Pri of bgterm list * info
	 | Par of bgterm list * info
	 | Com of bgterm * bgterm * info

  fun innernames (Mer (_, _))    = NameSet.empty
	  | innernames (Con (X, _))    = X
	  | innernames (Wir (w, _))    = Wiring.innernames w
	  | innernames (Ion (i, _))    = Ion.innernames i
	  | innernames (Hop (b, _))    = innernames b
	  | innernames (Per (p, _))    = Permutation.innernames p
	  | innernames (Abs (_, b, _)) = innernames b
	  | innernames (Ten (bs, _)) =
      foldr (fn (b, Y) => NameSet.union' Y (innernames b)) NameSet.empty bs
	  | innernames (Pri (bs, _)) =
      foldr (fn (b, Y) => NameSet.union' Y (innernames b)) NameSet.empty bs
	  | innernames (Par (bs, _)) =
      foldr (fn (b, Y) => NameSet.union' Y (innernames b)) NameSet.empty bs
	  | innernames (Com (_, b2, _)) = innernames b2

  fun outernames (Mer (_, _))    = NameSet.empty
	  | outernames (Con (X, _))    = X
	  | outernames (Wir (w, _))    = Wiring.outernames w
	  | outernames (Ion (i, _))    = Ion.outernames i
	  | outernames (Hop (b, _))    = outernames b
	  | outernames (Per (p, _))    = Permutation.outernames p
	  | outernames (Abs (_, b, _)) = outernames b
	  | outernames (Ten (bs, _)) =
      foldr (fn (b, Y) => NameSet.union' Y (outernames b)) NameSet.empty bs
	  | outernames (Pri (bs, _)) =
      foldr (fn (b, Y) => NameSet.union' Y (outernames b)) NameSet.empty bs
	  | outernames (Par (bs, _)) =
      foldr (fn (b, Y) => NameSet.union' Y (outernames b)) NameSet.empty bs
	  | outernames (Com (b1, _, _)) = outernames b1

  fun WLS i ws = 
      Ten (map
	   (fn w =>
	       Abs (Wiring.outernames w, 
		    Com (Ten ([Wir (w, i),
			      Per (Permutation.id_n 1, i)], 
			      i),
			 Con (Wiring.innernames w, i),
			 i),
		    i))
	   ws,
	   i)

  fun info (Mer (_, i))    = i
    | info (Con (_, i))    = i
    | info (Wir (_, i))    = i
    | info (Ion (_, i))    = i
    | info (Hop (_, i))    = i
    | info (Per (_, i))    = i
    | info (Abs (_, _, i)) = i
    | info (Ten (_, i))    = i
    | info (Pri (_, i))    = i
    | info (Par (_, i))    = i
    | info (Com (_, _, i)) = i

  fun eq (Mer (i1, _))       (Mer (i2, _))                = i1 = i2
    | eq (Con (ns1, _))      (Con (ns2, _))               = NameSet.eq ns1 ns2
    | eq (Wir (w1, _))       (Wir (w2, _))                = Wiring.eq w1 w2
    | eq (Ion (i1, _))       (Ion (i2, _))                = Ion.eq i1 i2
    | eq (Hop (b1, _))       (Hop (b2, _))                = eq b1 b2
    | eq (Per (p1, _))       (Per (p2, _))                = Permutation.eq p1 p2
    | eq (Abs (ns1, b1, _))  (Abs (ns2, b2, _))           = 
        NameSet.eq ns1 ns2 andalso eq b1 b2
    | eq (Ten (bs1, _))      (Ten (bs2, _))               = 
        ListPair.all (fn (b1, b2) => eq b1 b2) (bs1, bs2)
    | eq (Pri (bs1, _))      (Pri (bs2, _))               = 
        ListPair.all (fn (b1, b2) => eq b1 b2) (bs1, bs2)
    | eq (Par (bs1, _))      (Par (bs2, _))               =
        ListPair.all (fn (b1, b2) => eq b1 b2) (bs1, bs2)
    | eq (Com (b11, b12, _)) (Com (b21, b22, _))          =
        eq b11 b21 andalso eq b12 b22
    | eq _ _                                              = false

  exception UnknownControl of bgterm
  exception WrongArity of bgterm

  fun replacectrls ctrllist =
    let
      fun replace (t as Ion (ion, i)) = (
          Ion (Ion.replacectrl ctrllist ion, i)
          handle Ion.WrongArity _ => raise WrongArity t
               | Ion.UnknownControl _ => raise UnknownControl t) 
        | replace (Hop (t, i)) = Hop (replace t, i)
        | replace (Abs (X, t, i)) = Abs (X, replace t, i)
        | replace (Ten (ts, i)) = Ten (map replace ts, i)
        | replace (Pri (ts, i)) = Pri (map replace ts, i)
        | replace (Par (ts, i)) = Par (map replace ts, i)
        | replace (Com (t1, t2, i))
        = Com (replace t1, replace t2, i)
        | replace t = t
    in
      replace
    end

  fun add1s (t as Ion (ion, i))
    = if Control.kind (#ctrl (Ion.unmk ion)) = Control.Atomic then
        Com (t, Mer (0, i), i)
      else
        t 
    | add1s (Hop (t, i))      = t
    | add1s (Abs (X, t, i))   = Abs (X, add1s t, i)
    | add1s (Ten (ts, i))     = Ten (map add1s ts, i)
    | add1s (Pri (ts, i))     = Pri (map add1s ts, i)
    | add1s (Par (ts, i))     = Par (map add1s ts, i)
    | add1s (Com (t1, t2, i)) = Com (add1s t1, add1s t2, i)
    | add1s t = t

  exception NotImplemented of bgterm * string

  (* Determine whether t = 1 * ... * 1 *)
  fun is_n (Mer (0, _))    = true
    | is_n (Mer _)         = false
    | is_n (Con _)         = false
    | is_n (Wir (w, _))    = Wiring.is_id0 w
    | is_n (Ion _)         = false
    | is_n (Hop _)         = false
    | is_n (Per (pi, _))   = Permutation.is_id0 pi
    | is_n (Abs (X, t, _)) = NameSet.isEmpty X andalso is_n t
    | is_n (Ten (ts, _))   = List.all is_n ts
    | is_n (Par (ts, _))   = List.all is_n ts
    | is_n (Pri (ts, _))   = List.all is_n ts
    | is_n (t as (Com _))          
      = raise NotImplemented (t, "is_n for composition")
  (* Determine whether (X)t is the identity. *)
  fun is_concretion_of X (Mer (1, _))     = NameSet.isEmpty X
    | is_concretion_of X (Mer _)          = false
    | is_concretion_of X (Con (Y, _))     = NameSet.eq X Y
    | is_concretion_of X (Wir _)          = false
    | is_concretion_of X (Ion _)          = false
    | is_concretion_of X (Hop _)          = false
    | is_concretion_of X (Per (pi, _))    = NameSet.isEmpty X andalso
		  			   Permutation.is_id pi
    | is_concretion_of X (Abs (Y, t, _))  
      = is_concretion_of (NameSet.union X Y) t
    | is_concretion_of X (Ten ([t], _))   = is_concretion_of X t
    | is_concretion_of _ (Ten _)          = false
    | is_concretion_of X (Par ([t], _))   = is_concretion_of X t
    | is_concretion_of _ (Par _)          = false
    | is_concretion_of X (Pri (ts, _))    = is_n_x_concretion_of X ts
    | is_concretion_of X (t as (Com _))          
      = raise NotImplemented (t, "is_concretion_of for composition")
  and is_n_x_concretion_of X [] = false
    | is_n_x_concretion_of X (t :: ts)
    = if is_n t then
        is_n_x_concretion_of X ts
      else if is_concretion_of X t then
        List.all is_n ts
      else
        false

  fun is_id0 (Mer _)        = false
    | is_id0 (Con _)        = false
    | is_id0 (Wir (w, _))   = Wiring.is_id0 w
    | is_id0 (Ion _)        = false
    | is_id0 (Hop _)        = false
    | is_id0 (Per (pi, _))  = Permutation.width pi = 0
    | is_id0 (Abs _)        = false
    | is_id0 (Ten (ts, _))  = List.all is_id0 ts
    | is_id0 (Par (ts, _))  = List.all is_id0 ts
    | is_id0 (Pri (ts, _))  = false
    | is_id0 (t as (Com _))
    = raise NotImplemented (t, "is_idw for composition")

  fun is_id0' t = is_id0 t handle NotImplemented _ => false

  fun is_idw (Mer _)        = false
    | is_idw (Con _)        = false
    | is_idw (Wir (w, _))   = Wiring.is_id w
    | is_idw (Ion _)        = false
    | is_idw (Hop _)        = false
    | is_idw (Per (pi, _))  = Permutation.width pi = 0
    | is_idw (Abs _)        = false
    | is_idw (Ten (ts, _))  = List.all is_idw ts
    | is_idw (Par (ts, _))  = List.all is_idw ts
    | is_idw (Pri (ts, _))  = List.all is_idw ts
    | is_idw (t as (Com _))
    = raise NotImplemented (t, "is_idw for composition")

  fun is_id1_x_idw (Mer (1, _))    = true
    | is_id1_x_idw (Mer _)         = false 
    | is_id1_x_idw (Con (X, _))    = NameSet.isEmpty X
    | is_id1_x_idw (Wir (w, _))    = Wiring.is_id w
    | is_id1_x_idw (Ion _)         = false
    | is_id1_x_idw (Hop _)         = false
    | is_id1_x_idw (Per (pi, _))   = Permutation.width pi = 1
    | is_id1_x_idw (Abs (X, t, _)) = is_concretion_of X t
    | is_id1_x_idw (Ten (ts, _))   = is_id1_x_idw_list ts 
    | is_id1_x_idw (Par (ts, _))   = is_id1_x_idw_list ts 
    | is_id1_x_idw (Pri (ts, _))   = is_id1_x_n_x_idw_list ts
    | is_id1_x_idw (t as (Com _)) = raise NotImplemented
	  			 (t, "is_id1_x_idw for composition")
	and is_id1_x_idw_list [] = false
	  | is_id1_x_idw_list (t :: ts)
	  = if is_idw t then
	      is_id1_x_idw_list ts
	    else if is_id1_x_idw t then
	      List.all is_idw ts
	    else
	      false
	and is_id1_x_n_x_idw (Mer (1, _))    = true
    | is_id1_x_n_x_idw (Mer _)         = false 
    | is_id1_x_n_x_idw (Con (X, _))    = NameSet.isEmpty X
    | is_id1_x_n_x_idw (Wir (w, _))    = Wiring.is_id w
    | is_id1_x_n_x_idw (Ion _)         = false
    | is_id1_x_n_x_idw (Hop _)         = false
    | is_id1_x_n_x_idw (Per (pi, _))   = Permutation.width pi = 1
    | is_id1_x_n_x_idw (Abs (X, t, _)) = is_concretion_of X t
    | is_id1_x_n_x_idw (Ten (ts, _))   = is_id1_x_n_x_idw_list ts 
    | is_id1_x_n_x_idw (Par (ts, _))   = is_id1_x_n_x_idw_list ts 
    | is_id1_x_n_x_idw (Pri (ts, _))   = is_id1_x_n_x_idw_list ts
    | is_id1_x_n_x_idw (t as (Com _)) = raise NotImplemented
	  			 (t, "is_id1_x_n_x_idw for composition")
	and is_id1_x_n_x_idw_list [] = false
	  | is_id1_x_n_x_idw_list (t ::  ts)
	  = if is_idw t orelse is_n t then
	      is_id1_x_n_x_idw_list ts
	    else if is_id1_x_n_x_idw t then
	      List.all (fn t => is_idw t orelse is_n t) ts
	    else
	      false

  fun is_id (Mer (1, _))    = true
    | is_id (Mer _)         = false
    | is_id (Con (X, _))    = NameSet.isEmpty X
    | is_id (Wir (w, _))    = Wiring.is_id w
    | is_id (Ion _)         = false
    | is_id (Hop _)         = false
    | is_id (Per (pi, _))   = Permutation.is_id pi
    | is_id (Abs (X, t, _)) = is_concretion_of X t
    | is_id (Ten (ts, _))   = List.all is_id ts
    | is_id (Pri (ts, _))   = is_id1_x_idw_list ts
    | is_id (Par (ts, _))   = List.all is_id ts
    | is_id (t as (Com _))  = raise NotImplemented
	  			 (t, "is_id for composition")

  fun is_id' t = is_id t handle NotImplemented _ => false

  fun is_barren_root (Mer (0, _))      = true
    | is_barren_root (Mer _)           = false 
    | is_barren_root (Con _)           = false
    | is_barren_root (Wir _)           = false
    | is_barren_root (Ion _)           = false
    | is_barren_root (Hop _)           = false
    | is_barren_root (Per (pi, _))     = false
    | is_barren_root (Abs (X, t, _))   = NameSet.isEmpty X andalso is_barren_root t
    | is_barren_root (Ten (ts, _))     = is_barren_root_list ts 
    | is_barren_root (Par (ts, _))     = is_barren_root_list ts 
    | is_barren_root (Pri (ts, _))     = List.all is_barren_root ts
    | is_barren_root (Com (t1, t2, _)) = is_id' t1 andalso is_barren_root t2
                                         orelse is_barren_root t1
  and is_barren_root_list [] = false
    | is_barren_root_list (t :: ts)
    = if is_barren_root t then
        List.all is_id0' ts
      else if is_id0' t then
        is_barren_root_list ts
      else
        false

  fun width (Mer _)         = 1
    | width (Con _)         = 1
    | width (Wir _)         = 0
    | width (Ion _)         = 1
    | width (Hop _)         = 1
    | width (Per (pi, _))   = Permutation.width pi
    | width (Abs _)         = 1
    | width (Ten (ts, _))   = foldr (fn (t, n) => width t + n) 0 ts
    | width (Par (ts, _))   = foldr (fn (t, n) => width t + n) 0 ts
    | width (Pri _)         = 1
    | width (Com (t, _, _)) = width t

  exception ThisCannotHappen

  fun widest [] = raise ThisCannotHappen
    | widest (t :: ts)
    = let
        fun w (widestt, n) [] = widestt
          | w (widestt, n) (t :: ts)
          = let
              val n_t = width t
            in
              w (if n_t > n then (t, n_t) else (widestt, n)) ts
            end
      in
        w (t, width (t : bgterm)) ts
      end

  (* Determine whether the inside of t is an atomic ion with
   * an empty inner face.
   *)
  fun is_atomic_ion (Mer _)         = false
    | is_atomic_ion (Con _)         = false
    | is_atomic_ion (Wir _)         = false
    | is_atomic_ion (Ion (ion, _))
    = Control.kind (#ctrl (Ion.unmk ion)) = Control.Atomic
    | is_atomic_ion (Hop (t, _))    = false
    | is_atomic_ion (Per (pi, _))   = false
    | is_atomic_ion (Abs (X, t, _)) = is_atomic_ion t
    | is_atomic_ion (Ten (ts, _))
    = foldr
        (fn (t, n) => if is_atomic_ion t then 1 + n else n + 2 * width t)
        0
        ts
      = 1
    | is_atomic_ion (Par (ts, _))
    = foldr
        (fn (t, n) => if is_atomic_ion t then 1 + n else n + 2 * width t)
        0
        ts
      = 1
    | is_atomic_ion (Pri (ts, _))
    = foldr
        (fn (t, n) => if is_atomic_ion t then 1 + n else n + 2 * width t)
        0
        ts
      = 1
    | is_atomic_ion (Com (_, t, _)) = is_atomic_ion t


  local
    (* Push a global name substitution without introductions and
     * closed links down onto a prime.
     * We try to remove created identities.
     * NB! It's probably possible to push the wiring further down. *)
    fun apply_sigma s (b as (Mer _)) = (b, false)
      | apply_sigma s (b as (Con (X, i))) =
        let
          val s' = Wiring.restrict s X
        in
          if Wiring.is_id s' then
            (b, NameSet.isEmpty X)
          else
            (Com (Ten ([Wir (s', i),
                        Per (Permutation.id_n 1, i)], i), b, i), false)
        end
      | apply_sigma s (Wir (w, i)) =
        let
          val w' = Wiring.o (Wiring.restrict s (Wiring.outernames w), w)
        in
          (Wir (w', i), Wiring.is_id w')
        end
      | apply_sigma s (Ion (KyX, i)) =
        let
          val {ctrl, free, bound} = Ion.unmk KyX
          (* NB! we are allowing the same name to occur more than once:
           *     E.g. x//[x,y]  and  K[x,y]  =>  K[x,x]   *)
          val free' = (List.map (fn y => getOpt (Wiring.app_x s y, y)) free)
        in
          (Ion (Ion.make {ctrl = ctrl, free = free', bound = bound}, i), false)
        end
      | apply_sigma s (Hop (b, i)) = (Hop (#1 (apply_sigma s b), i), false)
      | apply_sigma s (b as (Per (pi, _))) = (b, Permutation.is_id pi)
      | apply_sigma s (Abs (X, b, i)) =
        let
          val (b', id') = apply_sigma (Wiring.* (s, Wiring.id_X X)) b
        in
          (* NB! The substitution is only on global names. *)
          (Abs (X, b', i), id' andalso NameSet.isEmpty X)
        end
      | apply_sigma s (Ten (bs, i)) =
        let
          val (bs', id) = foldr
                            (fn (b, (bs', id)) =>
                                let
                                  val (b', id') = apply_sigma s b
                                in
                                  (b'::bs', id andalso id')
                                end)
                            ([], true)
                            bs
        in
          (Par (bs', i), id)
        end
      | apply_sigma s (Par p) = apply_sigma s (Ten p)
      | apply_sigma s (Pri (bs, i)) =
        let
          val bs' = map (#1 o (apply_sigma s)) bs
        in
          (Pri (bs', i), is_id1_x_idw_list bs')
        end
      | apply_sigma s (Com (b1, b2, i)) =
        let
          val (b1', id) = apply_sigma s b1
        in
          if id then
            (b2, is_id' b2)
          else
            (Com (b1', b2, i), false)
        end

    (* Check whether any name in X is in the innerface of b *)
    fun not_in_innerface X (Mer _)         = true
      | not_in_innerface X (Con (Y, _))    = NameSet.disjoint X Y
      | not_in_innerface X (Wir (w, _))    = NameSet.disjoint X (Wiring.innernames w)
      | not_in_innerface X (Ion (i, _))    = NameSet.disjoint X (Ion.innernames i)
      | not_in_innerface X (Hop (b, _))    = not_in_innerface X b
      | not_in_innerface X (Per (p, _))    = NameSet.disjoint X (Permutation.innernames p)
      | not_in_innerface X (Abs (_, b, _)) = not_in_innerface X b
      | not_in_innerface X (Ten (bs, _))   = List.all (not_in_innerface X) bs
      | not_in_innerface X (Pri (bs, _))   = List.all (not_in_innerface X) bs
      | not_in_innerface X (Par (bs, _))   = List.all (not_in_innerface X) bs
      | not_in_innerface X (Com (_, b, _)) = not_in_innerface X b
  in
    (* Try heuristically to replace a merge with prime product(s),
     * eliminating as much wiring as possible.
     *
     * The function can only return SOME b' when called with some b if
     * size(b') < size(b) for some appropriate size-measure. This is
     * necessary to prevent divergence, as the function may be called
     * on its result.
     *)
        (*     (X * (/Y_1 * ... * /Y_k) * (z_1/V_1 * ... * z_l/V_l) * merge(m))
         *     o (P_1 * ... * P_n)
         * ->  (X * (/y_1 * ... * /y_k) * id_{z_1, ...,z_l} * idp(1))
         *     o (sigma(P_1) `|` ... `|` sigma(P_n))
         *
         * where  sigma = y_1/Y_1 * ... * y_k/Y_k * z_1/V_1 * ... * z_l/V_l
         *   and  y_i \in Y_i
         *   and  width(P_i) <= 1
         *   and  sum_{1 <= i <= n}(width(P_i)) = m
         *)
    fun to_prime_product (Com (Ten ([Wir (w, i), Mer (m, i')], i''),
                               Ten (bs, i'''), i'''')) =
        let
          val n'
            = foldr (fn (_, ~1) => ~1
                      | (b, n') => let val n'' = width b
                                   in if n'' <= 1 then n' + n'' else ~1 end)
                    0 bs
        in
          if n' = m then
            let
              val {intro, closures, function} = Wiring.partition w
              val (ys, yYs) = LinkSet.fold
                                (fn l => fn (ys, yYs) =>
                                   let
                                     val Y_i = Link.innernames l
                                     val y_i = NameSet.someElement Y_i
                                   in
                                     (NameSet.insert y_i ys,
                                      LinkSet.insert
                                        (Link.make {outer = SOME y_i, inner = Y_i})
                                        yYs)
                                   end)
                                (NameSet.empty, LinkSet.empty)
                                (Wiring.unmk closures)
              val sigma = Wiring.* (Wiring.make yYs, function)
            in
              (* If (X * (/y_1 * ... * /y_k) * id_{z_1, ..., z_l} * idp(1)) is
               * an identity we can leave it out. *)
              if Wiring.is_id0 intro andalso Wiring.is_id0 closures then
                SOME (Pri (map (#1 o (apply_sigma sigma)) bs, i'''))
              else
                SOME
                  (Com (Ten ([Wir (Wiring.**
                                     [intro, Wiring.close ys,
                                      Wiring.id_X (Wiring.outernames function)], i),
                              Per (Permutation.id_n 1, i')], i''),
                        Pri (map (#1 o (apply_sigma sigma)) bs, i'''), i''''))
            end
          else
            NONE
        end
        (* Catch the symmetric case of the above *)
      | to_prime_product (Com (Ten ([m as (Mer _), w as (Wir _)], i),
                               t as (Ten _), i')) =
        to_prime_product (Com (Ten ([w, m], i), t, i'))
        (*     merge(m) o (P_1 * ... * P_n)
         * ->  P_1 `|` ... `|` P_n
         * where  width(P_i) <= 1 for 1 <= i <= n
         *   and  sum_{1 <= i <= n}(width(P_i)) = m  *)
      | to_prime_product (Com (Mer (m, _), Ten (bs, _), i)) =
        let
          val n' 
            = foldr (fn (_, ~1) => ~1
                      | (b, n') => let val n'' = width b
                                   in if n'' <= 1 then n' + n'' else ~1 end)
                    0 bs
        in
          if n' = m then
            SOME (Pri (bs, i))
          else
            NONE
        end
        (* All transformations valid for tensor product are valid for
         * parallel product. *)
      | to_prime_product (Com (Par p, b, i)) =
        to_prime_product (Com (Ten p, b, i))
      | to_prime_product (Com (b, Par p, i)) =
        to_prime_product (Com (b, Ten p, i))
      | to_prime_product _ = NONE

    (* Try heuristically to replace a tensor product with a parallel
     * product, eliminating as much wiring as possible, and push
     * substitutions downwards.
     *
     * The function can only return SOME b' when called with some b if
     * size(b') < size(b) for some appropriate size-measure. This is
     * necessary to prevent divergence, as the function may be called
     * on its result.
     *)
    fun to_parallel_product (Com (Ten (Wir (w, i) :: bs, i'), b, i'')) =
        (case b of
           Con _ => NONE (* We currently can't push w through a concretion,
                          * thus apply_sigma would not decrease the size-measure
                          * in this case *)
         | _     =>
           (* FIXME should be refactored *)
           let
             val (bs_is_idp, bs_is_ion_with_multiple_names_for_binding_port)
               = case bs of
                   [Per (pi, _)]  => (Permutation.is_id pi, false)
(*                 | [Ion (KyX, _)] => (false, List.exists
                                               (fn X => NameSet.size X > 1)
                                               (#bound (Ion.unmk KyX)))
*)                 | _              => (false, false)
             val {intro, closures, function} = Wiring.partition w
             val Z = Wiring.outernames function
             val (ys, yYs, yYs_is_not_id)
               = LinkSet.fold
                   (fn l => fn (ys, yYs, yYs_is_not_id) =>
                      let
                        val Y_i = Link.innernames l
                        val y_i = NameSet.someElement Y_i
                      in
                        (NameSet.insert y_i ys,
                         LinkSet.insert
                           (Link.make {outer = SOME y_i, inner = Y_i})
                           yYs,
                         yYs_is_not_id orelse NameSet.size Y_i > 1)
                      end)
                   (NameSet.empty, LinkSet.empty, false)
                   (Wiring.unmk closures)
           in
             if bs_is_idp
                andalso Wiring.is_id0 intro andalso Wiring.is_id0 closures then
               (*    (sigma * idp(m)) o b
                * -> sigma(b)
                *
                * where sigma = z_1/V_1 * ... * z_l/V_l
                *   and V_i =/= Ø
                *)
               SOME (#1 (apply_sigma w b))
(* FIXME this case requires that apply_sigma also works on local names.
             else if bs_is_ion_with_multiple_names_for_binding_port then
               (*    (w * K[y_1, ..., y_m][[X_1, ..., X_n]]) o b
                * -> (w * K[y_1, ..., y_m][[{x_1}, ..., {x_n}]]) o sigma(b)
                *
                * FIXME when X_i is empty we don't change that port.
                *
                * where sigma = id_Z * x_1/X_1 * ... * x_n/X_n
                *   and sigma =/= id
                *   and w : Z ->
                *)
               let
                 val Z = Wiring.innernames w
                 val id_Z = Wiring.id_X Z
                 val (KyX, i''') = case bs of
                                     [Ion i] => i
                                   | _ => raise ThisCannotHappen
                 val {ctrl, free, bound} = Ion.unmk KyX
                 val (bound', xXs)
                   = foldr (fn (X_i, (bound', xXs)) =>
                               if NameSet.isEmpty X_i then
                                 (X_i :: bound', xXs)
                               else
                                 let
                                   val x_i = NameSet.someElement X_i
                                 in
                                   (NameSet.singleton x_i :: bound',
                                    LinkSet.insert
                                      (Link.make {outer = SOME x_i, inner = X_i})
                                      xXs)
                                 end)
                           ([], LinkSet.empty)
                           bound
                 val KyX'  = Ion.make {ctrl = ctrl, free = free, bound = bound'}
                 val sigma = Wiring.* (id_Z, Wiring.make xXs)
               in
                 SOME (Com (Ten ([Wir (w, i), Ion (KyX', i''')], i'),
                            #1 (apply_sigma sigma b), i''))
               end
*)
             else if (not (Wiring.is_id function)
                      andalso not_in_innerface Z (Ten (bs, i')))
                     orelse yYs_is_not_id then
               (*    (X * (/Y_1 * ... * /Y_k) * (z_1/V_1 * ... * z_l/V_l)
                *     * b_1 * ... * b_m)
                *    o b
                * -> (X * (/y_1 * ... * /y_k) * id_Z
                *     * b_1 * ... * b_m)
                *    o sigma(b)
                *
                * where sigma = y_1/Y_1 * ... * y_k/Y_k * z_1/V_1 * ... * z_l/V_l
                *   and sigma =/= id
                *   and y_i \in Y_i
                *   and V_i =/= Ø
                *   and Z = {z_1, ..., z_n}               
                *)
               let
                 val sigma = Wiring.* (Wiring.make yYs, function)
                 val id_Z  = Wiring.id_X Z
               in
                 SOME
                   (Com (Par (Wir (Wiring.** [intro, Wiring.close ys, id_Z], i)
                              :: bs, i'),
                         #1 (apply_sigma sigma b), i''))
               end
             else
               NONE
           end)
        (* All transformations valid for tensor product are valid for
         * parallel product. *)
      | to_parallel_product (Com (Par p, b, i)) =
        to_parallel_product (Com (Ten p, b, i))
      | to_parallel_product _ = NONE
  end

  fun pp'' pp_unchanged indent pps t =
    let
      val ppids      = Flags.getBoolFlag "/kernel/ast/bgterm/ppids"
      val ppabs      = Flags.getBoolFlag "/kernel/ast/bgterm/ppabs"
      val pp0abs     = Flags.getBoolFlag "/kernel/ast/bgterm/pp0abs"
      val pptenaspar = Flags.getBoolFlag "/kernel/ast/bgterm/pptenaspar"
      val ppmeraspri = Flags.getBoolFlag "/kernel/ast/bgterm/ppmeraspri"
      val (pp_unchanged_add, pp_unchanged_remove)
        = if pp_unchanged then
            (fn _ => (), fn _ => ())
          else
            (Name.pp_unchanged_add, Name.pp_unchanged_remove)
      open PrettyPrint
      val PrMax = 9
      val PrCom = 7
      val PrPri = 6
      val PrTen = 5
      val PrPar = 5
      val PrAbs = 4
      val PrMin = 0
      val show = add_string pps
      fun << () = begin_block pps INCONSISTENT 0
      fun >> () = end_block pps
      fun brk () = add_break pps (1, 0)
      fun brk0 () = add_break pps (0, 0)
      fun brkindent () = add_break pps (1, indent)
      (* Pretty print using precedences (binary operators are right associative).
       * Operator precedences (highest binds tightest):
       *  8 (X) abstraction (only affects expressions to the right)
       *  7  o  composition
       *  6  |  prime product
       *  5  *  tensor product
       *  5  || parallel product
       * Abstraction reaches as far right as possible.
       * @params pps prf prr t
       * @param pps  PrettyPrint stream
       * @param pal  Precedence attraction of surrounding left expression
       * @param par  Precedence attraction of surrounding right expression
       * @param prr  Precedence resistance of surrounding right expression
       * @param aih  Atomic ions should be printed in "hole form"
       * @param t    Term to print
       *)
      fun ppp pal par prr aih =
        let 
          fun checkprec prec =
            if pal > prec orelse par >= prec then
              (fn () => show "(", 
               PrMin, PrMin, PrMax, 
               fn () => show ")")
      	    else
      	      (fn () => (), pal, par, prr, fn () => ())
          (* Pretty print the product of a list of terms.
           *
           * @param outermost  Is this list the outermost term?
           * @param innermost  Is this list the innermost term?
           * @param sep        Separator between list elements.
           * @param empty      Symbol to print if the list is empty.
           * @param pr         Precedence of sep.
           * @param ppids      Whether to print identities.
           * @param bs         The terms to print.
           * @return  The set of inner names and the set innernames \ outernames.
           *)
          fun pplist outermost innermost sep empty pr ppids bs =
              (let
                 (* Remove ids if the option is set - but keep track of the
                  * inner names that we don't print. *)
                 fun remove_id is_id (b, (bs', inner_ns)) =
                     if is_id b then
                       (bs', NameSet.union' inner_ns (innernames b))
                     else
                       (b :: bs', inner_ns)
                 val (bs', inner_ns) =
                     if ppids then
                       (bs, NameSet.empty)
                     (* It is unsound to remove ids with width > 0 even if
                      * they're not innermost.
                      * Example:
                      *   (`[]` || `[x]`) o (M0 || M1[x])
                      * becomes
                      *   `[x]` o (M0 || M1[x])
                      *)
                     else (* if innermost then *)
                       foldr (remove_id is_id0') ([], NameSet.empty) bs
                     (* else
                       foldr (remove_id is_id') ([], NameSet.empty) bs *)
              in
                case bs' of
                 []
               => (case bs of
                     [] => (show empty;
                            (NameSet.empty, NameSet.empty))
                   | bs => (pp''' outermost innermost (widest bs);
                            (inner_ns, NameSet.empty)))
               | [b] =>
                 let
                   val (inner_ns', new_ns') = pp''' outermost innermost b
                 in
                   (NameSet.union' inner_ns inner_ns', new_ns')
                 end
               | (b :: bs) => 
                 let
                   val (showlpar, pal', par', prr', showrpar) 
                     = checkprec pr
                   fun mappp [] ns = ns
                     | mappp [b] (inner_ns, new_ns) =
                       let
                         val (inner_ns', new_ns')
                           = (show sep; brk();
                              ppp pr par' prr' aih outermost innermost b)
                       in
                         (NameSet.union' inner_ns inner_ns',
                          NameSet.union' new_ns new_ns')
                       end
                     | mappp (b :: b' :: bs) (inner_ns, new_ns) =
                       let
                         val (inner_ns', new_ns')
                           = (show sep; brk();
                              ppp pr pr pr aih outermost innermost b)
                       in
                         mappp
                           (b' :: bs)
                           (NameSet.union' inner_ns inner_ns',
                            NameSet.union' new_ns new_ns')
                       end
                   val (inner_ns', new_ns')
                     = (showlpar();
                        <<();
                        ppp pal' pr pr aih outermost innermost b)
                 in
                   mappp bs (NameSet.union' inner_ns inner_ns',
                             new_ns')
                   before
                  (showrpar();
                   >>())
                 end
               end handle e => raise e)
          (* Pretty print a term.
           *
           * @param outermost  Is this list the outermost term?
           * @param innermost  Is this list the innermost term?
           * @param t          The term to print.
           * @return  The set of inner names and the set innernames \ outernames.
           *)
          and pp''' _ _ (Mer (0, _)) = (  show "<->"
                                      ; (NameSet.empty, NameSet.empty))
            | pp''' _ _ (Mer (n, _)) = (  show ("merge(" ^ Int.toString n ^ ")")
                                      ; (NameSet.empty, NameSet.empty))
            | pp''' _ _ (Con (X, _))
            = (  (show "`"; NameSetPP.ppbr indent "[" "]" pps X; show "`"
               ; (X, NameSet.empty))
               handle e => raise e)
            | pp''' _ _ (Wir (w, _)) = 
              (let
                 val w'       = if ppids then w else Wiring.removeids w
                 val inner_ns = Wiring.innernames w
                 val new_ns   = NameSet.difference
                                  inner_ns (Wiring.outernames w)
               in 
                 (  pp_unchanged_add new_ns
                  ; Wiring.pp indent pps w'
                  ; (inner_ns, new_ns))
               end handle e => raise e)
            | pp''' _ _ (Ion (KyX, _)) =
              (let
                 val ctrl     = #ctrl (Ion.unmk KyX)
                 val inner_ns = Ion.innernames KyX
                 val new_ns   = NameSet.difference
                                  inner_ns (Ion.outernames KyX)
               in
                 if aih andalso Control.kind ctrl = Control.Atomic then
                   (  pp_unchanged_add new_ns
                    ; show "<<("
                    ; Ion.pp indent pps KyX handle e => raise e
                    ; show ")>>"
                    ; (inner_ns, new_ns))
                 else
                   (  pp_unchanged_add new_ns
                    ; Ion.pp indent pps KyX handle e => raise e
                    ; (inner_ns, new_ns))
               end handle e => raise e)
            | pp''' outermost innermost (Hop (t, _)) =
              (  show "<<("
               ; pp''' outermost innermost t
                 before
                 show ")>>")
            | pp''' _ _ (Per (pi, _)) =
              (  Permutation.pp indent pps pi handle e => raise e
               ; (Permutation.innernames pi, NameSet.empty))
            | pp''' outermost innermost (Abs (X, b, _))
            = ((if (outermost andalso not (NameSet.isEmpty X)) orelse
                   ppabs andalso
                   (pp0abs orelse not (NameSet.isEmpty X)) then
                  let
                    val (showlpar, pal', par', prr', showrpar) = 
                      if prr < PrAbs orelse
                        pal >= PrAbs orelse
                        par > PrAbs then 
                        (fn () => show "(", 
                         PrMin, PrMin, PrMax,
                         fn () => show ")")
                      else
                        (fn () => (), PrMin, par, prr, fn () => ())
                  in
                    <<(); 
                    showlpar();
                    show "<"; NameSetPP.ppbr indent "[" "]" pps X; show ">";
                    brkindent();
                    ppp pal' par' prr' aih outermost innermost b
                    before
                   (showrpar();
                    >>())
                  end
                else
                  ppp pal par prr aih outermost innermost b)
               handle e => raise e)
            | pp''' outermost innermost (Ten (bs, i)) =
              if pptenaspar then
                pp''' outermost innermost (Par (bs, i))
              else
                pplist outermost innermost " *" "idx0" PrTen ppids bs
            | pp''' outermost innermost (Par (bs, _)) =
              pplist outermost innermost " ||" "idx0" PrPar ppids bs
            | pp''' outermost innermost (Pri (bs, _)) =
              pplist outermost innermost " `|`" "<->" PrPri false bs
            | pp''' outermost innermost (b as (Com (b1, b2, _))) =
              (let
                 val (showlpar, pal', par', prr', showrpar)
                   = checkprec PrCom
               in
                 if is_atomic_ion b1 then
                   if is_barren_root b2 then
                     ppp pal par prr false outermost innermost b1
                   else
                     (* Print the atomic ion b1 in a special way which
                      * indicates that it has a hole *)
                     let
                       val (inner_ns1, new_ns1)
                         = (showlpar();
                            <<();
                            ppp pal' PrCom PrCom true outermost false b1)
                       val (inner_ns2, new_ns2)
                         = (show " o";
                            brk();
                            ppp PrCom par' prr' false false innermost b2)
                     in
                       showrpar();
                       >>();
                       pp_unchanged_remove
                         (NameSet.difference new_ns1 inner_ns2);
                       (inner_ns2,
                        NameSet.union'
                          new_ns2
                          (NameSet.intersect new_ns1 inner_ns2))
                     end
                 else
                   let
                     val b'  = if ppmeraspri then
                                 to_prime_product b
                               else
                                 NONE
                     val b'' = case b' of
                                 SOME _ => b'
                               | NONE => if pptenaspar then
                                           to_parallel_product b
                                         else
                                           NONE
                   in
                     case b'' of
                       SOME b => pp''' outermost innermost b
                     | NONE =>
                       let
                         (* Is this too expensive? *)
                         val (b1isid, b2isid) = if not ppids then
                                                  (is_id' b1, is_id' b2)
                                                else
                                                  (false, false)
                       in
                         if b1isid then
                           pp''' outermost innermost b2
                         else if b2isid then
                           pp''' outermost innermost b1
                         else
                           let
                             val (inner_ns1, new_ns1)
                               = (showlpar();
                                  <<();
                                  ppp pal' PrCom PrCom false outermost false b1)
                             val (ns2 as (inner_ns2, new_ns2))
                               = (show " o";
                                  brk();
                                  ppp PrCom par' prr' aih false innermost b2)
                           in
                             showrpar();
                             >>();
                             pp_unchanged_remove
                               (NameSet.difference new_ns1 inner_ns2);
                             (inner_ns2, NameSet.union'
                                           new_ns2
                                           (NameSet.intersect new_ns1 inner_ns2))
                           end
                       end
                   end
               end handle e => raise e)
        in
          pp'''
        end
    in
      (ppp PrMin PrMin PrMax true true true t; ())
    end handle e => raise e
                          
  fun pp indent pps t =
      let
        val ns = NameSet.union' (outernames t) (innernames t)
      in
        (  Name.pp_unchanged ns
           handle Name.PPUnchangedNameClash _ =>
                  Name.pp_unchanged_add ns
         ; pp'' false indent pps t)
      end

  fun pp' indent pps t =
      let
        val ns = NameSet.union' (outernames t) (innernames t)
      in
        (  Name.pp_unchanged_add ns
         ; pp'' false indent pps t)
      end

  fun pp_unchanged indent pps t =
      (  Name.pp_unchanged NameSet.empty
       ; pp'' true indent pps t)

      fun oldpp indent pps =
        let
          open PrettyPrint
          val PrMax = 9
          val PrAbs = 8
          val PrCom = 7
          val PrTen = 6
          val PrPri = 6
          val PrPar = 6
          val PrMin = 0
          val show = add_string pps
          fun << () = begin_block pps INCONSISTENT indent
          fun >> () = end_block pps
          fun brk () = add_break pps (1, 0)
          fun brk0 () = add_break pps (0, 0)
          (* Pretty print using precedences.  Operator precedences
           * (highest binds tightest):
           *  8 (X) abstraction (only affects expressions to the right)
           *  7  o  composition
           *  6  x  tensor product
           * Abstraction reaches as far right as possible.
           * @params pps prf prr t
           * @param pps  PrettyPrint stream
           * @param pal  Precedence attraction of surrounding left expression
           * @param par  Precedence attraction of surrounding right expression
           * @param prr  Precedence resistance of surrounding right expression
           * @param t    Term to print
           *)
          fun ppp pal par prr =
            let 
              fun checkprec prec =
                if pal >= prec orelse par > prec then
                  (fn () => show "(", 
                   PrMin, PrMin, PrMax, 
                   fn () => show ")")
                else
                  (fn () => (), pal, par, prr, fn () => ())
              fun pp' (Mer (0, _)) = show "1"
                | pp' (Mer (n, _)) = show ("merge_" ^ Int.toString n)
                | pp' (Con (X, _)) 
                = (show "'"; NameSetPP.pp indent pps X; show "'")
                | pp' (Wir (w, _)) = Wiring.oldpp indent pps w
                | pp' (Ion (KyX, _)) = Ion.oldpp indent pps KyX
                | pp' (Hop (t, _)) = (show "<<("; pp' t; show ")>>")
                | pp' (Per (pi, _)) = Permutation.oldpp indent pps pi
                | pp' (Abs (X, b, _)) =
                  let
                    val (showlpar, pal', par', prr', showrpar)
                      = if prr < PrAbs then 
                          (fn () => show "(", 
                           PrMin, PrMin, PrMax,
                           fn () => show ")")
                        else
                          (fn () => (), PrMin, par, prr, fn () => ())
                  in
                    <<(); 
                    showlpar();
                    show "("; NameSetPP.pp indent pps X; show ")";
                    brk0();
                    ppp pal' par' prr' b;
                    showrpar();
                    >>()
                  end
                | pp' (Ten (bs, _)) =
                  (case bs of
                     [] => show "idx_0"
                   | [b] => pp' b
                   | (b :: bs) => 
                     let
                       val (showlpar, pal', par', prr', showrpar) 
                         = checkprec PrTen
                       fun mappp [] = ()
                         | mappp [b]
                         = (brk(); show "* "; ppp PrTen par' prr' b)
                         | mappp (b :: b' :: bs) 
                         = (brk();
                            show "* ";
                            ppp PrTen PrTen PrTen b;
                            mappp (b' :: bs))
                     in
                       <<();
                       showlpar();
                       ppp pal' PrTen PrTen b;
                       mappp bs;
                       showrpar();
                       >>()
                     end)
                | pp' (Pri (bs, _)) =
                  (case bs of
                     [] => show "1"
                   | [b] => pp' b
                   | (b :: bs) => 
                     let
                       val (showlpar, pal', par', prr', showrpar) 
                         = checkprec PrPri
                       fun mappp [] = ()
                         | mappp [b]
                         = (brk(); show "| "; ppp PrPri par' prr' b)
                         | mappp (b :: b' :: bs) 
                         = (brk();
                            show "| ";
                            ppp PrPri PrPri PrPri b;
                            mappp (b' :: bs))
                     in
                       <<();
                       showlpar();
                       ppp pal' PrPri PrPri b;
                       mappp bs;
                       showrpar();
                       >>()
                     end)
                | pp' (Par (bs, _)) =
                  (case bs of
                     [] => show "id||_0"
                   | [b] => pp' b
                   | (b :: bs) => 
                     let
                       val (showlpar, pal', par', prr', showrpar) 
                         = checkprec PrPar
                       fun mappp [] = ()
                         | mappp [b]
                         = (brk(); show "|| "; ppp PrPar par' prr' b)
                         | mappp (b :: b' :: bs) 
                         = (brk();
                            show "|| ";
                            ppp PrPar PrPar PrPar b;
                            mappp (b' :: bs))
                     in
                       <<();
                       showlpar();
                       ppp pal' PrPar PrPar b;
                       mappp bs;
                       showrpar();
                       >>()
                     end)
                | pp' (Com (b1, b2, _)) = 
                  let
                    val (showlpar, pal', par', prr', showrpar)
                      = checkprec PrCom
                  in
                    <<();
                    showlpar();
                    ppp pal' PrCom PrCom b1;
                    brk();
                    show "o ";
                    ppp PrCom par' prr' b2;
                    showrpar();
                    >>()
                  end
            in
              pp'
            end
        in
          ppp PrMin PrMin PrMax
        end handle e => raise e

  fun explain_NotImplemented (NotImplemented (v, errtxt)) =
      [Exp (LVL_USER, Info.origin (info v), pack_pp_with_data pp v, []),
       Exp (LVL_LOW, file_origin, mk_string_pp errtxt, [])]
    | explain_NotImplemented _ = raise Match
  val _ = add_explainer
            (mk_explainer "feature not implemented"
                          explain_NotImplemented)
  fun explain_UnknownControl (UnknownControl t) =
      [Exp (LVL_USER, Info.origin (info t), pack_pp_with_data pp t, []),
       Exp (LVL_LOW, file_origin, mk_string_pp "in BgTerm.replacectrls", [])]
    | explain_UnknownControl _ = raise Match
  val _ = add_explainer
            (mk_explainer "The ion control name has not been declared"
                          explain_UnknownControl)
  fun explain_WrongArity (WrongArity t) =
      [Exp (LVL_USER, Info.origin (info t), pack_pp_with_data pp t, []),
       Exp (LVL_LOW, file_origin, mk_string_pp "in BgTerm.replacectrls", [])]
    | explain_WrongArity _ = raise Match
  val _ = add_explainer
            (mk_explainer
              "Ion control arity does not match the number \
              \of free names or bound name sets"
                          explain_WrongArity)
  fun size bg =
      case bg of
	  Mer _ => 1
	| Con _ => 1
	| Wir _ => 1
	| Ion _ => 1
	| Hop(b,_) => 1+size b
	| Per _ => 1
	| Abs(_,b,_) => 1+size b
	| Ten(bs,_) => 1+sizes bs
	| Pri(bs,_) => 1+sizes bs
	| Par(bs,_) => 1+sizes bs
	| Com(b1,b2,_) => 1+size b1+size b2
  and sizes bs = List.foldl (fn (b,s) => size b + s) 0 bs

end

functor BgTerm (structure Info : INFO
                 structure Link : LINK
                 structure LinkSet : MONO_SET
                 structure Ion : ION
                 structure Control : CONTROL
                 structure Wiring : WIRING
                 structure Permutation : PERMUTATION
                 structure Name : NAME
                 structure NameSet : MONO_SET
                 structure NameSetPP : COLLECTIONPRETTYPRINT
                   where type ppstream    = PrettyPrint.ppstream
                 structure ErrorHandler : ERRORHANDLER
                   where type ppstream     = PrettyPrint.ppstream
                     and type break_style = PrettyPrint.break_style
                     and type origin       = Origin.origin
                 sharing type Name.name =
                              NameSet.elt =
                              Ion.name =
                              Link.name =
                              Wiring.name
                 sharing type Link.link = LinkSet.elt
                 sharing type LinkSet.Set = Wiring.linkset
                 sharing type NameSet.Set =
                              Name.NameSet.Set =
                              Ion.nameset =
                              Link.nameset =
                              Permutation.nameset =
                              Wiring.nameset =
                              NameSetPP.collection
    sharing type Ion.control = Control.control)
      :> BGTERM where type info = Info.info
                  and type wiring = Wiring.wiring
                  and type 'kind permutation = 'kind Permutation.permutation
                  and type Immutable = Permutation.Immutable
                  and type control = Control.control
                  and type ion = Ion.ion
                  and type nameset = NameSet.Set
 =
struct
  structure BgTerm = BgTerm'(structure Info = Info
                              structure Link = Link
                              structure LinkSet = LinkSet
                              structure Ion = Ion
                              structure Control = Control
                              structure Wiring = Wiring
                              structure Permutation = Permutation
                              structure Name = Name
                              structure NameSet = NameSet
                              structure NameSetPP = NameSetPP
                              structure ErrorHandler = ErrorHandler)
  open BgTerm
end
