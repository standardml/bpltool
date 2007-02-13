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

functor Instantiation (
  structure BgVal     : BGVAL
  structure BgBDNF    : BGBDNF
  structure Info      : INFO
  structure Interface : INTERFACE
  structure Name      : NAME
  structure NameMap   : MONO_FINMAP
  structure NameSet   : MONO_SET
  structure Wiring    : WIRING
  structure ErrorHandler : ERRORHANDLER
    where type ppstream    = PrettyPrint.ppstream
      and type break_style = PrettyPrint.break_style
      and type origin      = Origin.origin
  structure NameSetPP : COLLECTIONPRETTYPRINT
    where type ppstream    = PrettyPrint.ppstream
  sharing type Info.info =
               BgVal.info =
               BgBDNF.info
  sharing type Name.name =
               BgVal.name =
               Wiring.name =
               NameSet.elt = 
               NameMap.dom
  sharing type NameMap.map = Wiring.namemap
  sharing type NameSet.Set =
               NameSetPP.collection =
               Name.NameSet.Set =
               Wiring.nameset =
               Interface.nameset 
  sharing type Interface.interface =
               BgVal.interface
  sharing type Wiring.wiring = BgVal.wiring
  sharing type BgVal.bgval = BgBDNF.bgval
) : INSTANTIATION
  where type name = Name.name
    and type bgval = BgVal.bgval
    and type 'a bgbdnf = 'a BgBDNF.bgbdnf
    and type DR        = BgBDNF.DR
    and type interface = Interface.interface =
struct
  type interface = Interface.interface
  type name      = Name.name
  type wiring    = Wiring.wiring
  type bgval     = BgVal.bgval
  type 'a bgbdnf = 'a BgBDNF.bgbdnf
  type DR        = BgBDNF.DR

  val array  = Array.array
  val update = Array.update
  val sub    = Array.sub
  infix 8 sub

  val noinfo = Info.noinfo

  (* Instantiation type.
   * The n is the width of the instantiation, i.e. the width of
   * the bigraphs resulting from applying the instantiation to a given
   * parameter.
   * Each element L_i of the array, L, indicates where the i'th
   * root of the original
   *    \bigox_{i \in m}(P_i)
   * should be copied to and what local substitution to apply to each copy.
   *    \bigox(P'_i)
   * I.e. each L_i has elements
   *    L_ij = (root_ij, ren_ij)
   * which means that P'_{root_ij} = ren_ij o P_i.
   * To be meaningful, the n root_ij's must be different and  root_ij \in n.
   * Also, the outerfaces of the renamings ren_ij must not share names.
   *)
  type inst = {n : int, inst : ((int * bgval) list) array}
  (* Construct an instantiation.
   * n is taken to be |maps|.
   * map_i = (root_i, ren_i)  where root_i \in m  is the number of a site
   *                                              in the redex.
   *
   *                            and ren_i         is a renaming of the local
   *                                              names of the site given by
   *                                              root_i to the local names
   *                                              of site i in the reactum.
   *                                              The renaming is given a list
   *                                              of name pairs (x_j, y_j) to
   *                                              be interpreted as x_j |-> y_j.
   *)
  fun make m maps =
      let
        val inst = array (m, [])

        fun add_entry ((root_i, ren_i), (i, Ys)) =
            let
              val L_ijs = inst sub i
              val ls_i  = BgVal.LS
                            noinfo
                            (Wiring.make_ren (NameMap.fromList ren_i))
              val Ys_i = Interface.names (BgVal.outerface ls_i)
              val Ys   = (NameSet.union Ys Ys_i)
                         handle NameSet.DuplicatesRemoved => raise (Fail "FIXME")
            in
              (  update (inst, root_i, (i, ls_i)::L_ijs)
               ; (i + 1, Ys))
            end

        val (n, _) = foldl add_entry (0, NameSet.empty) maps
      in
        {n = n, inst = inst}
      end

  (* Unless otherwise specified in maps, root i of the instance will be
   * a copy of root i of the original, given that there is a unique
   * one-to-one correspondence between the local names of the two roots.
   * If the names used are the same, they are assumed to map to themselves.
   * 
   * The algorithm is as follows:
   * 1. sort maps by descending reactum root numbers
   * 2. insert trivial maps for the instance roots that are not mentioned
   *    (if possible)
   * 3. add name maps to the entries that has none
   * 4. verify the entries of maps. I.e. we check that the names mentioned
   *    in a map correspond to those of the interfaces and that each root
   *    of the instance is mentioned exactly once.
   * 5. convert maps to the format used by make
   *
   * steps 2, 3, 4, and 5 is done in step
   *)
  fun make' i1 i2 maps =
      let
        val m = Interface.width i1
        val n = Interface.width i2
        val Xs = Array.fromList (Interface.loc i1)
        val Ys = Array.fromList (Interface.loc i2)

        fun map_cmp (((rea1,_), _), ((rea2,_), _)) = Int.compare (rea2, rea1)

        val maps = ListSort.sort map_cmp maps

        (* infer (if possible) the name map from site red of redex to
         * site rea of reactum. *)
        fun infer_map_names rea red =
            let
              val X = Xs sub red
              val Y = Ys sub rea
            in
              if NameSet.eq X Y then
                (red, NameSet.fold (fn n => fn nmap => (n, n)::nmap) [] X)
              else if NameSet.size X = 1 andalso NameSet.size Y = 1 then
                (red, [(NameSet.someElement X, NameSet.someElement Y)])
              else
                raise (Fail "FIXME not a trivial map from names of red to rea")
            end

        (* verify that the names given in the map matches the interfaces.
         * If no names are given, try to infer them. *)
        fun verify_map_names ((rea, []), (red, [])) = infer_map_names rea red
          | verify_map_names ((rea, reans), (red, redns)) =
            let
              val X = Xs sub red
              val Y = Ys sub rea

              (* verify a single renaming and keep track of the used names *)
              fun verify_ren (x, y, (X', Y', nmap)) =
                  if NameSet.member x X andalso NameSet.member y Y then
                    (NameSet.insert x X', NameSet.insert y Y', (x, y)::nmap)
                    handle NameSet.DuplicatesRemoved => raise (Fail "FIXME multiple occurences of the same name")
                  else
                    raise (Fail "FIXME name doesn't match interface")

              val (X', Y', nmap)
                = (ListPair.foldrEq 
                     verify_ren
                     (NameSet.empty, NameSet.empty, [])
                     (redns, reans))
                  handle ListPair.UnequalLengths => raise (Fail "FIXME invalid map")
            in
              if NameSet.size X = NameSet.size X' andalso NameSet.size Y = NameSet.size Y' then
                (red, nmap)
              else
                raise Fail "FIXME not all renamings are specified in the map"
            end

        fun foo root [] acc =
            if root = ~1 then
              acc
            else
              raise (Fail "FIXME too few map entries")
          | foo root (maps as ((map as ((rea, reans), (red, redns)))::mapstl)) acc =
            if root >= 0 then
              (case Int.compare (rea, root) of
                 EQUAL   => foo (root - 1) mapstl ((verify_map_names map) :: acc)
               | LESS    => foo (root - 1) maps ((infer_map_names root root) :: acc)
               | GREATER => raise (Fail "FIXME invalid rea"))
            else
              raise (Fail "FIXME too many map entries")
      in
        make m (foo (n - 1) maps [])
      end

  fun make'' i1 i2 = make' i1 i2 []

  val id : inst = {n = 0, inst = Array.fromList []} (* FIXME! *)

  (* FIXME maybe we should keep the working array between instantiations? *)
  (* FIXME which exceptions should be raised when Ps doesn't match the
   * instantiation? *)
  fun instantiate {n, inst} d =
      let
        infix 5 **
        fun b1 ** b2 = BgVal.Ten noinfo [b1, b2]
        val oo = BgVal.Com noinfo
        infix 7 oo
        
        val {ren, Ps} = BgBDNF.unmkDR d

        (* build the result list in an array *)
        val id_0 = BgVal.Ten noinfo []
        val res = array (n, id_0)

        (* copy P_i and rename the local names *)
        fun copy_P P_i (root_ij, ren_ij) =
            let
              val W_i    = Interface.glob (BgVal.outerface P_i)
              val id_W_i = BgVal.Wir noinfo (Wiring.id_X W_i)
            in
              update (res, root_ij, (ren_ij ** id_W_i) oo P_i)
            end

        fun copies_P (P_i, i) =
            if i < n then
              (  app (copy_P (BgBDNF.unmk P_i)) (inst sub i)
               ; i + 1)
            else 
              raise (Fail "FIXME")

        val () = if List.foldl copies_P 0 Ps = n then
                   ()
                 else 
                   raise (Fail "FIXME")

        val P's  = BgVal.Par noinfo (Array.foldr (op ::) [] res)
        val W    = Interface.glob (BgVal.innerface P's)
        val id_W = BgVal.Wir noinfo (Wiring.id_X W)
      in
        (  if List.foldl copies_P 0 Ps = n then
             ()
           else 
             raise (Fail "FIXME")
         ; (ren ** id_W) oo (BgVal.Par noinfo (Array.foldr (op ::) [] res)))
      end

  (** Deconstruct an instantiation. *)
  fun unmk {n, inst} =
      let
        val res = array (n, (~1, []))

        (* Convert a single copy endtry to res.
         * I.e. deconstruct the local substitution and the underlying renaming
         * {x_j |-> y_j} and set  res_rea = (red, ren).
         *)
        local open BgVal in
          fun map2res red ((rea, subst), m) =
              case match (PAbs (PCom (PTen [PWir, PVar], PVar))) subst of
                MAbs (_, MCom (MTen [MWir wren, _], _)) =>
                let
                  val ren = (NameMap.Fold (op ::) [] (Wiring.unmk_ren wren))
                            handle Wiring.NotARenaming _ => raise (Fail "FIXME should not happen")
                in
                  (  update (res, rea, (red, ren))
                   ; m + 1)
                end
              | _ => raise (Fail "FIXME should not happen")
        end

        (* convert an entry of inst to res format *)
        fun convert (red, entry, m) = foldl (map2res red) m entry

        val m = Array.foldli convert 0 inst
      in
        (m, Array.foldr (op ::) [] res)
      end
    
  (* Prettyprint an instantiation.
   * @params indent pps inst
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param inst    The instantiation to print.
   *)
  fun pp indent pps inst =
      let
	open PrettyPrint
	val show = add_string pps
	fun << () = begin_block pps INCONSISTENT indent
	fun >> () = end_block pps
	fun brk () = add_break pps (1, 0)
        fun pp_nlist select [] = ()
          | pp_nlist select (nmap : (name * name) list) = 
            ( show "&["
             ; foldl (fn (xy, notfirst) =>
                         (  if notfirst then (show ","; brk()) else ()
                          ; Name.pp indent pps (select xy)
                          ; true))
                     false nmap
             ; show "]")
        fun pp_elt rea (red, nmap) =
            (  <<()
             ; show (Int.toString rea)
             ; pp_nlist #2 nmap
             ; show " |--> "
             ; show (Int.toString red)
             ; pp_nlist #1 nmap
             ; >>())
	fun pp_e (e, (rea, notfirst)) =
	    (  if notfirst then (show ","; brk()) else ()
	     ; pp_elt rea e
	     ; (rea + 1, true))
      in
	<<(); show "["; foldl pp_e (0, false) (#2(unmk inst)); show "]"; >>()
      end
      

  fun toString i
    = PrettyPrint.pp_to_string
        (Flags.getIntFlag "/misc/linewidth") 
        (pp (Flags.getIntFlag "/misc/indent")) i
end
