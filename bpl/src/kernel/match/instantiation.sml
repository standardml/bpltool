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
 * Closely follows Jensen and Milner's definition from p 75
 * "Bigraphs and Mobile Processes (revised)", February 2004.
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
  structure ListPP : POLYCOLLECTIONPRETTYPRINT
    where type ppstream      = PrettyPrint.ppstream
      and type 'a collection = 'a list
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
               BgVal.interface =
               BgBDNF.interface
  sharing type Wiring.wiring = BgVal.wiring
  sharing type BgVal.bgval = BgBDNF.bgval
) : INSTANTIATION
  where type name = Name.name
    and type bgval = BgVal.bgval
    and type 'a bgbdnf = 'a BgBDNF.bgbdnf
    and type DR        = BgBDNF.DR
    and type interface = Interface.interface =
struct

  open Debug
  open ErrorHandler

  val file_origin = Origin.mk_file_origin
                      "$BPL/src/kernel/match/instantiation.sml"
                      Origin.NOPOS
  fun mk_explainer errtitle (explainer : exn -> explanation list) e =
      Exp (LVL_USER, Origin.unknown_origin, mk_string_pp errtitle,
           explainer e)

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
   *
   * I    is the (local) innerface of the instantiation
   * J    is the (local) outerface
   * rho  is the function \bar{rho} : n -> m  and  the local substitutions
   *      (X_\bar{rho}(j)) -> (Y_j)
   *)
  type inst = {I : interface, J : interface, rho : (int * bgval) array}

  (* Unless otherwise specified in maps, root i of the instance will be
   * a copy of root i of the original, given that there is a unique
   * one-to-one correspondence between the local names of the two roots.
   * If the names used are the same, they are assumed to map to themselves.
   * 
   * The algorithm is as follows:
   * 1. sort maps by descending reactum site numbers
   * 2. insert trivial maps for the reactum sites that are not mentioned
   *    (if possible)
   * 3. add name maps to the entries that has none
   * 4. verify the entries of maps. I.e. we check that the names mentioned
   *    in a map correspond to those of the interfaces and that each root
   *    of the instance is mentioned exactly once.
   *
   * steps 2, 3, 4, and 5 are done in step
   *)
  (* FIXME should the interfaces be local? or should we only store
   *       the local parts? or neither? *)
  fun make I J maps =
      let
        val m  = Interface.width I
        val n  = Interface.width J
        val Xs = Array.fromList (Interface.loc I)
        val Ys = Array.fromList (Interface.loc J)

        (* Create a rho array on the form described above using a completely
         * specified map list, i.e. the list must have, in order, elements
         * [(redex_site_{0}, name_map_{0}), ...,
         *  (redex_site_{m - 1}, name_map_{m - 1})]
         *)
        fun maps2rho maps =
            let
              val id_0 = BgVal.Ten noinfo []
              val rho = array (m, (~1, id_0))
                
              fun add_entry ((redsite, nmap), reasite) =
                  let
                    val ls = BgVal.LS noinfo (Wiring.make_ren nmap)
                  in
                    (  update (rho, reasite, (redsite, ls))
                     ; reasite + 1)
                  end
            in
              (  foldl add_entry 0 maps
               ; rho)
            end
        (* infer or verify a name map from a redex site to a reactum site.
         *)
        fun infer_or_verify_namemap ((rea, []), (red, [])) =
            (* infer (if possible) the name map from site red of redex to
             * site rea of reactum. *)
            let
              val X = Xs sub red
              val Y = Ys sub rea
            in
              if NameSet.eq X Y then
                NameSet.fold
                  (fn n => fn nmap => NameMap.add (n, n, nmap))
                  NameMap.empty X
              else if NameSet.size X = 1 andalso NameSet.size Y = 1 then
                NameMap.fromList
                  [(NameSet.someElement X, NameSet.someElement Y)]
              else
                raise Fail "FIXME not a trivial map from names of red to rea"
            end
          | infer_or_verify_namemap ((rea, reans), (red, redns)) =
            (* verify the given name map *)
            let
              val X = Xs sub red
              val Y = Ys sub rea

              (* verify a single renaming and keep track of the used names *)
              fun verify_ren (x, y, (X', Y', nmap)) =
                  if NameSet.member x X andalso NameSet.member y Y then
                    (NameSet.insert x X',
                     NameSet.insert y Y',
                     NameMap.add (x, y, nmap))
                    handle NameSet.DuplicatesRemoved =>
                      raise Fail "FIXME multiple occurences of the same name"
                  else
                    raise Fail "FIXME name doesn't match interface"

              val (X', Y', nmap)
                = (ListPair.foldrEq 
                     verify_ren
                     (NameSet.empty, NameSet.empty, NameMap.empty)
                     (redns, reans))
                  handle ListPair.UnequalLengths => raise (Fail "FIXME invalid map")
            in
              if NameSet.size X = NameSet.size X' andalso NameSet.size Y = NameSet.size Y' then
                nmap
              else
                raise Fail "FIXME not all renamings are specified in the map"
            end

        (* infer missing maps and verify the map list.
         * reasite is the number of the next reactum site to be added to the
         * accumulator acc.
         *)
        fun infer_and_verify_maps reasite acc [] =
            if reasite = ~1 then
              acc
            else
              (* insert a trivial map for reasite *)
              infer_and_verify_maps
                (reasite - 1)
                ((reasite,
                  (infer_or_verify_namemap ((reasite, []), (reasite, []))))
                 :: acc)
                []
          | infer_and_verify_maps reasite acc (maps as ((map as ((rea, reans), (red, redns)))::mapstl)) =
            if reasite >= 0 then
              (case Int.compare (rea, reasite) of
                 EQUAL => (* verify map and add it to the accumulator *)
                   infer_and_verify_maps
                     (reasite - 1)
                     ((red, (infer_or_verify_namemap map)) :: acc)
                     mapstl
               | LESS => (* insert a trivial map for reasite *)
                   infer_and_verify_maps
                     (reasite - 1)
                     ((reasite,
                       (infer_or_verify_namemap ((reasite, []), (reasite, []))))
                      :: acc)
                     maps
               | GREATER => raise (Fail "FIXME invalid rea"))
            else
              raise (Fail "FIXME too many map entries")

        (* compare function for maps used to sort in decreasing order *)
        fun map_cmp (((rea1,_), _), ((rea2,_), _)) = Int.compare (rea2, rea1)
      in
        {I = I,
         J = J,
         rho = (  maps2rho 
                o (infer_and_verify_maps (n - 1) [])
                o (ListSort.sort map_cmp))
               maps}
      end

  fun make' I J = make I J []

  (* FIXME which exceptions should be raised when d doesn't match the
   *       instantiation? *)
  fun instantiate {I, J, rho} d =
      let
        infix 5 **
        fun b1 ** b2 = BgVal.Ten noinfo [b1, b2]
        val oo' = BgVal.Com' noinfo
        infix 7 oo'
        
        val {ren, Ps} = BgBDNF.unmkDR d
        val Ps = Array.fromList Ps

        fun copy_P ((i, ls_i), es) =
            (ls_i oo' (BgBDNF.unmk (Ps sub i))) :: es
      in
        (* FIXME only compare the local part? *)
        if Interface.eq (BgBDNF.outerface d, I) then
          ren oo' (BgVal.Par noinfo (Array.foldr copy_P [] rho))
        else
          raise Fail "FIXME parameter does not match the instantiation innerface"
      end

  (* Deconstruct an instantiation. *)
  fun unmk {I, J, rho} =
      let
        open BgVal
        fun rho2map (reasite, (redsite, ls), maps) = 
            case match (PAbs (PCom (PTen [PWir, PVar], PVar))) ls of
              MAbs (_, MCom (MTen [MWir wren, _], _)) =>
              let
                val (reans, redns)
                  = (NameMap.Fold
                       (fn ((x, y), (reans, redns)) => (y::reans, x::redns))
                       ([], [])
                       (Wiring.unmk_ren wren))
                    handle Wiring.NotARenaming _ =>
                      raise Fail "FIXME should not happen"
              in
                ((reasite, reans), (redsite, redns)) :: maps
              end
            | _ => raise Fail "FIXME should not happen"
      in
        (I, J, Array.foldri rho2map [] rho)
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
	fun pp_e indent pps ((reasite, reans), (redsite, redns)) =
	    (  <<()
             ; show (Int.toString reasite)
             ; ListPP.pp Name.pp indent pps reans
             ; brk()
             ; show "|-->"
             ; brk()
             ; show (Int.toString redsite)
             ; ListPP.pp Name.pp indent pps redns
             ; >>())
      in
	ListPP.pp pp_e indent pps (#3 (unmk inst))
      end

  fun toString i
    = PrettyPrint.pp_to_string
        (Flags.getIntFlag "/misc/linewidth") 
        (pp (Flags.getIntFlag "/misc/indent")) i

end
