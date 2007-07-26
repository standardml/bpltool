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
		structure Ion : ION
		structure Control : CONTROL
		structure Wiring : WIRING
		structure Permutation : PERMUTATION
		structure NameSet : MONO_SET
		structure NameSetPP : COLLECTIONPRETTYPRINT
                  where type ppstream    = PrettyPrint.ppstream
    structure ErrorHandler : ERRORHANDLER
        where type ppstream    = PrettyPrint.ppstream
          and type break_style = PrettyPrint.break_style
          and type origin      = Origin.origin
		sharing type NameSet.Set =
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
    desc = "Explicitly display identities in tensor products",
    short = "",
    long = "--ppids",
    arg = "",
    default = true}
  val _ = Flags.makeBoolFlag {
    name = "/kernel/ast/bgterm/pp0abs",
    desc = "Explicitly display empty-set abstractions",
    short = "",
    long = "--pp0abs",
    arg = "",
    default = true}
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
	 | Per of Immutable permutation * info
	 | Abs of nameset * bgterm * info
	 | Ten of bgterm list * info
	 | Pri of bgterm list * info
	 | Par of bgterm list * info
	 | Com of bgterm * bgterm * info

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

  fun is_idw (Mer _)        = false
    | is_idw (Con _)        = false
    | is_idw (Wir (w, _))   = Wiring.is_id w
    | is_idw (Ion _)        = false
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
    | is_id (Per (pi, _))   = Permutation.is_id pi
    | is_id (Abs (X, t, _)) = is_concretion_of X t
    | is_id (Ten (ts, _))   = List.all is_id ts
    | is_id (Pri (ts, _))   = is_id1_x_idw_list ts
    | is_id (Par (ts, _))   = List.all is_id ts
    | is_id (t as (Com _))  = raise NotImplemented
	  			 (t, "is_id for composition")

  fun is_id' t = is_id t handle NotImplemented _ => false

  fun width (Mer _)         = 1
    | width (Con _)         = 1
    | width (Wir _)         = 0
    | width (Ion _)         = 1
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

  fun pp indent pps =
    let
      val ppids = Flags.getBoolFlag "/kernel/ast/bgterm/ppids"
      val pp0abs = Flags.getBoolFlag "/kernel/ast/bgterm/pp0abs"
      open PrettyPrint
      val PrMax = 9
      val PrCom = 6
      val PrTen = 5
      val PrPri = 5
      val PrPar = 5
      val PrAbs = 4
      val PrMin = 0
      val show = add_string pps
      fun << () = begin_block pps INCONSISTENT 0
      fun >> () = end_block pps
      fun brk () = add_break pps (1, 0)
      fun brk0 () = add_break pps (0, 0)
      fun brkindent () = add_break pps (1, indent)
      (* Pretty print using precedences.  Operator precedences
       * (highest binds tightest):
       *  8 (X) abstraction (only affects expressions to the right)
       *  7  o  composition
       *  6  *  tensor product
       *  6  |  prime product
       *  6  || parallel product
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
          fun pp' (Mer (0, _)) = show "<->"
            | pp' (Mer (n, _)) = show ("merge(" ^ Int.toString n ^ ")")
            | pp' (Con (X, _)) 
            = ((show "`"; NameSetPP.ppbr indent "[" "]" pps X; show "`")
               handle e => raise e)
            | pp' (Wir (w, _)) = (Wiring.pp indent pps w handle e => raise e)
            | pp' (Ion (KyX, _)) = (Ion.pp indent pps KyX handle e => raise e)
            | pp' (Per (pi, _)) = (Permutation.pp indent pps pi handle e => raise e)
            | pp' (Abs (X, b, _))
            = ((if pp0abs orelse not (NameSet.isEmpty X) then
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
                    ppp pal' par' prr' b;
                    showrpar();
                    >>()
                  end
                else
                  ppp pal par prr b)
               handle e => raise e)
            | pp' (Ten (bs, i)) =
              (let
                val bs' =
                  if ppids then
                    bs
                  else
                    List.filter (not o is_id') bs
              in
                case bs' of
                 []
               => (case bs of
                     [] => show "idx0"
                   | bs => pp' (widest bs))
               | [b] => pp' b
               | (b :: bs) => 
                 let
                   val (showlpar, pal', par', prr', showrpar) 
                     = checkprec PrTen
                   fun mappp [] = ()
                     | mappp [b]
                     = (show " *"; brk(); ppp PrTen par' prr' b)
                     | mappp (b :: b' :: bs) 
                     = (show " *";
                        brk();
                        ppp PrTen PrTen PrTen b;
                        mappp (b' :: bs))
                 in
                     showlpar();
                     <<();
                     ppp pal' PrTen PrTen b;
                     mappp bs;
                     showrpar();
                     >>()
                 end
               end handle e => raise e)
            | pp' (Pri (bs, _)) =
              ((case bs of
                 [] => show "<->"
               | [b] => pp' b
               | (b :: bs) => 
                 let
                   val (showlpar, pal', par', prr', showrpar) 
                     = checkprec PrPri
                   fun mappp [] = ()
                     | mappp [b]
                     = (show " `|`"; brk(); ppp PrPri par' prr' b)
                     | mappp (b :: b' :: bs) 
                     = (show " `|`";
                        brk();
                        ppp PrPri PrPri PrPri b;
                        mappp (b' :: bs))
                 in
                   showlpar();
                   <<();
                   ppp pal' PrPri PrPri b;
                   mappp bs;
                   showrpar();
                   >>()
                 end) handle e => raise e)
            | pp' (Par (bs, _)) =
              ((case bs of
                 [] => show "id||0"
               | [b] => pp' b
               | (b :: bs) => 
                 let
                   val (showlpar, pal', par', prr', showrpar) 
                     = checkprec PrPar
                   fun mappp [] = ()
                     | mappp [b]
                     = (show " ||"; brk(); ppp PrPar par' prr' b)
                     | mappp (b :: b' :: bs) 
                     = (show " ||";
                        brk();
                        ppp PrPar PrPar PrPar b;
                        mappp (b' :: bs))
                 in
                   showlpar();
                   <<();
                   ppp pal' PrPar PrPar b;
                   mappp bs;
                   showrpar();
                   >>()
                 end) handle e => raise e)
            | pp' (Com (b1, b2, _)) = 
              let
                val (showlpar, pal', par', prr', showrpar)
                  = checkprec PrCom
              in
                if is_atomic_ion b1 then
                  ppp pal par prr b1
                else
                  (showlpar();
                   <<();
                   ppp pal' PrCom PrCom b1;
                   show " o";
                   brk();
                   ppp PrCom par' prr' b2;
                   showrpar();
                   >>())
              end handle e => raise e
          in
            pp'
          end
        in
          ppp PrMin PrMin PrMax
        end handle e => raise e

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
	| Per _ => 1
	| Abs(_,b,_) => 1+size b
	| Ten(bs,_) => 1+sizes bs
	| Pri(bs,_) => 1+sizes bs
	| Par(bs,_) => 1+sizes bs
	| Com(b1,b2,_) => 1+size b1+size b2
  and sizes bs = List.foldl (fn (b,s) => size b + s) 0 bs

end

functor BgTerm (structure Info : INFO
		structure Ion : ION
		structure Control : CONTROL
		structure Wiring : WIRING
		structure Permutation : PERMUTATION
		structure NameSet : MONO_SET
		structure NameSetPP : COLLECTIONPRETTYPRINT
                  where type ppstream    = PrettyPrint.ppstream
    structure ErrorHandler : ERRORHANDLER
        where type ppstream    = PrettyPrint.ppstream
          and type break_style = PrettyPrint.break_style
          and type origin      = Origin.origin
		sharing type NameSet.Set =
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
                             structure Ion = Ion
                             structure Control = Control
                             structure Wiring = Wiring
                             structure Permutation = Permutation
                             structure NameSet = NameSet
                             structure NameSetPP = NameSetPP
                             structure ErrorHandler = ErrorHandler)
  open BgTerm
end