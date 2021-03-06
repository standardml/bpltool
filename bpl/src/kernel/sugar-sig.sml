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

(** Syntactic sugar for creating bgvals in SML.  For example:
 * <pre>
 * (print o BG.bgvalToString)
 * let
 *   open BG.Sugar
 *   infix 7 /  infix 7 //
 *   infix 6 o
 *   infix 5 *  infix 5 ||  infix 5 `|`
 *   infix 4 >
 *   infix 3 &  infix 3 --> infix 3 --   infix 3 --|>  infix 3----|>
 *   infix 2 =: infix 2 -:  infix 2 |->  infix 2 |-->  infix 2 :::
 *   nonfix @
 *   nonfix <
 * 
 *   val K  = active ("K" -: 3)
 *   val L  = passive ("L" =: 2 --> 2)
 *   val M  = atomic ("M" =: 1 --> 0)
 *   val N1 = active0 ("N1")
 *   val N2 = passive0 ("N2")
 *   val N3 = atomic0 ("N3")
 * 
 *   val (x, y, z, u, w) = ("x", "y", "z", "u", "w")
 *
 *   val rule1 =
 *     "FirstRule" ::: L[x,y][[z],[]] ----|> `[z]` * x//[] * y//[]
 *   val rule2 = "SecondRule" :::
 *     K[x,y,z] * `[u,w]`
 *     --[1&[x,y] |--> 1&[u,w], 2 |-> 1]--|>
 *     idp(1) * `[x,y]` * `[u,w]` * z//[]
 * in
 *   (x/y  *  (idp(1) * z//[x,z]) o `[z,x]`)
 *    o (<[x,z]> (idw[x,y,z] * merge(6)) o 
 *               (K[x,y,z]  *  (-//[z,y] * idp(1)) o L[y,z][[z,y],[]]  
 *               *  M[][[x]]  *  N1 o merge(3)  *  N2  *  N3))
 *    o (@@[1&[y,z], 2&[], 0&[]]  *  @[1,2,0]  *  -/w)
 * end
 *   handle error => (ErrorHandler.explain error; raise error);
 * </pre>
 * @version $LastChangedRevision$
 *)
signature SUGAR =
  sig
    (** Well-formed bigraph type. *)
    type bgval
    (** Name type. *)
    type name = string
    type arities
    type namedports
    type portassign 
    type mapinfo
    type placeinfo
    type absinfo
    type ctrlkind
    type rule
    type redexinst

    (** Signal detection of an ion with duplicate names.
     * @params K ys Xs errtxt
     * @param K       The ion control.
     * @param ys      The ions free names. 
     * @param Xs      The ions bound name sets.
     * @param errtxt  Text detailing the error
     *)
    exception DuplicateName of 
	      string * name list * name list list * string
    (** Signal detection of arity mismatch in control application.
     * @params errtxt
     * @param errtxt  Text detailing the error
     *)
    exception WrongArity of string
    (** Signal detection of a non-atomic control when using the << >> operator.
     * @params b
     * @param b  The bgval on which the << >> operator was used.
     *)
    exception NonAtomicControl of bgval
    (** Signal detection of a non-molecule when using the << >> operator.
     * @params b
     * @param b  The bgval on which the << >> operator was used.
     *)
    exception NotMolecule of bgval

    (** Create an active control. *)
    val active : (ctrlkind -> 'a) -> 'a
    (** Create an active control of arity 0 -> 0. *)
    val active0 : string -> bgval
    (** Create an atomic control. *)
    val atomic : (ctrlkind -> 'a) -> 'a
    (** Create an atomic control of arity 0 -> 0. *)
    val atomic0 : string -> bgval
    (** Create an passive control. *)
    val passive : (ctrlkind -> 'a) -> 'a
    (** Create an active passive of arity 0 -> 0. *)
    val passive0 : string -> bgval
    (** Operator to put between control name and arity spec. *)
    val =: : string * arities -> ctrlkind 
	     -> name list -> name list list -> bgval
    (** Operator to put between control name and zero inner 
     * arity spec.
     *)
    val -: : string * int -> ctrlkind -> name list -> bgval
    (** Operator to put between inner and outer arity. *)
    val --> : int * int -> arities
    (** Operator to put between control name and port names spec. *)
    val ==: : string * namedports -> ctrlkind 
	     -> portassign list -> portassign list -> bgval
    (** Operator to put between control name and port names spec with
     * no binding ports. *)
    val --: : string * string list -> ctrlkind -> portassign list -> bgval
    (** Operator to put between inner and outer named ports descriptions *)
    val ---> : string list * string list -> namedports
    (** Operator to put between port name and link name. *)
    val == : string * name -> portassign
    (** Turn an atomic molecule into an atomic ion. *)
    val << : bgval -> unit -> bgval
    (** Sugar for the unit argument to the << function.
     * It allows one to write <<M>>. *)
    val >> : unit
    (** A barren root bigraph. *)
    val <-> : bgval 
    (** Construct a merge_n bigraph. *)
    val merge : int -> bgval
    (** Construct single name renaming. *)
    val / : name * name -> bgval
    (** Construct single outer name wiring. *)
    val // : name * name list -> bgval
    (** Construct a closure. *)
    val -/ : name -> bgval
    (** Construct a multiple closure. *)
    val -// : name list -> bgval
    (** Left bracket for abstraction. *)
    val < : name list -> absinfo
    (** Right bracket for abstraction. *)
    val > : absinfo * bgval -> bgval
    (** Construct a nameless permutation. *)
    val @ : int list -> bgval
    (** Construct a permutation. *)
    val @@ : placeinfo list -> bgval
    (** Operator to put between map index and name set in permuation. *)
    val & : int * name list -> placeinfo
    (** Construct a concretion. *)
    val ` : name list -> 'a -> bgval
    (** Construct a bigraph composition. *)
    val o : bgval * bgval -> bgval
    (** Construct a tensor product. *)
    val * : bgval * bgval -> bgval
    (** Construct an iterated tensor product. *)
    val ** : bgval list -> bgval
    (** Construct a parallel product. *)
    val || : bgval * bgval -> bgval
    (** Construct an iterated parallel product. *)
    val ||| : bgval list -> bgval
    (** Construct a prime product. *)
    val `|` : bgval * bgval -> bgval
    (** Construct an iterated prime product. *)
    val `|`` : bgval list -> bgval
    (** Construct an identity nameless permutation. *)
    val idp : int -> bgval
    (** Construct an empty wiring. *)
    val idw0 : bgval
    (** Construct an identity wiring. *)
    val idw : name list -> bgval
    (** Construct an empty tensor product. *)
    val idx0 : bgval
    (** Construct a nameless site mapping. *)
    val |-> : int * int -> mapinfo
    (** Construct a site mapping. *)
    val |--> : placeinfo * placeinfo -> mapinfo
    (** Partially construct a rule. *)
    val -- : bgval * mapinfo list -> redexinst
    (** Construct an anonymous rule with a specified instantiation. *)
    val --|> : redexinst * bgval -> rule
    (** Construct an anonymous rule with a trivial instantiation. *)
    val ----|> : bgval * bgval -> rule
    (** Give a name to a rule. *)
    val ::: : string * rule -> rule
    (** Prettyprint a mapinfo. *)
    val ppMapinfo : int -> PrettyPrint.ppstream -> mapinfo -> unit
    (** Revision number.*)
    val revision : string
  end
