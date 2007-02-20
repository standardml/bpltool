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
 * @version $LastChangedRevision$
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
  where type name      = Name.name
    and type nameset   = NameSet.Set
    and type bgval     = BgVal.bgval
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
  type nameset   = NameSet.Set
  type wiring    = Wiring.wiring
  type bgval     = BgVal.bgval
  type 'a bgbdnf = 'a BgBDNF.bgbdnf
  type DR        = BgBDNF.DR

  val array  = Array.array
  val update = Array.update
  val sub    = Array.sub
  infix 8 sub

  val noinfo = Info.noinfo

  exception LogicalError of string
  fun explain_LogicalError (LogicalError errtxt) =
      [Exp (LVL_LOW, file_origin, mk_string_pp errtxt, [])]
    | explain_LogicalError _ = raise Match
  val _ = add_explainer
            (mk_explainer
               "an internal error occurred"
               explain_LogicalError)

  type map = (int * name list) * (int * name list)

  (* Prettyprint a map. *)
  fun pp_map indent pps ((reasite, []), (redsite, [])) =
      let
	open PrettyPrint
	val show = add_string pps
	fun << () = begin_block pps INCONSISTENT indent
	fun >> () = end_block pps
	fun brk () = add_break pps (1, 0)
      in
        (  <<()
         ; show (Int.toString reasite)
         ; brk()
         ; show "|->"
         ; brk()
         ; show (Int.toString redsite)
         ; >>())
      end
    | pp_map indent pps ((reasite, reans), (redsite, redns)) =
      let
	open PrettyPrint
	val show = add_string pps
	fun << () = begin_block pps INCONSISTENT indent
	fun >> () = end_block pps
	fun brk () = add_break pps (1, 0)
      in
        (  <<()
         ; show (Int.toString reasite)
         ; show "&"
         ; ListPP.pp Name.pp indent pps reans
         ; brk()
         ; show "|-->"
         ; brk()
         ; show (Int.toString redsite)
         ; show "&"
         ; ListPP.pp Name.pp indent pps redns
         ; >>())
      end

  val pp_map_list = ListPP.pp pp_map

  (* Instantiation type.
   *
   * I    is the (local) innerface of the instantiation
   * J    is the (local) outerface
   * rho  is the function \bar{rho} : n -> m  and  the local substitutions
   *      (X_\bar{rho}(j)) -> (Y_j)
   *)
  type inst = {I : interface, J : interface, rho : (int * bgval) array}

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
                      raise LogicalError
                              "rho contains an invalid local substitution"
              in
                ((reasite, reans), (redsite, redns)) :: maps
              end
            | _ => raise LogicalError
                           "rho contains an invalid local substitution"
      in
        {I = I, J = J, maps = Array.foldri rho2map [] rho}
      end

  (* Prettyprint an instantiation. *)
  fun pp indent pps inst = pp_map_list indent pps (#maps (unmk inst))

  fun toString i
    = PrettyPrint.pp_to_string
        (Flags.getIntFlag "/misc/linewidth") 
        (pp (Flags.getIntFlag "/misc/indent")) i

  exception CannotInferLocalRenaming of map * interface * interface
  fun explain_CannotInferLocalRenaming (CannotInferLocalRenaming (map, I, J)) =
      [Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "map",
            [Exp (LVL_USER, Origin.unknown_origin,
                  pack_pp_with_data pp_map map, [])]),
       Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "interface I",
            [Exp (LVL_USER, Origin.unknown_origin,
                  pack_pp_with_data Interface.pp I, [])]),
       Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "interface J",
            [Exp (LVL_USER, Origin.unknown_origin,
                  pack_pp_with_data Interface.pp J, [])])]
    | explain_CannotInferLocalRenaming _ = raise Match
  val _ = add_explainer
            (mk_explainer
               "cannot infer local renaming for instantiation"
               explain_CannotInferLocalRenaming)

  exception DuplicateNames of name * map
  fun explain_DuplicateNames (DuplicateNames (n, map)) =
      [Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "map",
            [Exp (LVL_USER, Origin.unknown_origin,
                  pack_pp_with_data pp_map map, [])]),
       Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "name",
            [Exp (LVL_USER, Origin.unknown_origin,
                  pack_pp_with_data Name.pp n, [])])]
    | explain_DuplicateNames _ = raise Match
  val _ = add_explainer
            (mk_explainer
               "instantiation map has duplicate names"
               explain_DuplicateNames)

  exception NameNotInInterface of name * map * interface
  fun explain_NameNotInInterface (NameNotInInterface (n, map, i)) =
      [Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "map",
            [Exp (LVL_USER, Origin.unknown_origin,
                  pack_pp_with_data pp_map map, [])]),
       Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "name",
            [Exp (LVL_USER, Origin.unknown_origin,
                  pack_pp_with_data Name.pp n, [])]),
       Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "interface",
            [Exp (LVL_USER, Origin.unknown_origin,
                  pack_pp_with_data Interface.pp i, [])])]
    | explain_NameNotInInterface _ = raise Match
  val _ = add_explainer
            (mk_explainer
               "name in instantiation map doesn't match interface"
               explain_NameNotInInterface)
  
  exception UnequalNameListLengths of map
  fun explain_UnequalNameListLengths (UnequalNameListLengths map) =
      [Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "map",
            [Exp (LVL_USER, Origin.unknown_origin,
                  pack_pp_with_data pp_map map, [])])]
    | explain_UnequalNameListLengths _ = raise Match
  val _ = add_explainer
            (mk_explainer
               "instantiation map has name lists of different lengths"
               explain_UnequalNameListLengths)

  exception IncompatibleSites of map * interface * interface
  fun explain_IncompatibleSites (IncompatibleSites (map, I, J)) =
      [Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "map",
            [Exp (LVL_USER, Origin.unknown_origin,
                  pack_pp_with_data pp_map map, [])]),
       Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "interface I",
            [Exp (LVL_USER, Origin.unknown_origin,
                  pack_pp_with_data Interface.pp I, [])]),
       Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "interface J",
            [Exp (LVL_USER, Origin.unknown_origin,
                  pack_pp_with_data Interface.pp J, [])])]
    | explain_IncompatibleSites _ = raise Match
  val _ = add_explainer
            (mk_explainer
               "interface sites specified by instantiation map are incompatible"
               explain_IncompatibleSites)

  exception IncompleteRenaming
  of nameset * nameset * map * interface * interface
  fun explain_IncompleteRenaming (IncompleteRenaming (X, Y, map, I, J)) =
      [Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "missing names from I",
            [Exp (LVL_USER, Origin.unknown_origin,
                  pack_pp_with_data NameSetPP.pp X, [])]),
       Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "missing names from J",
            [Exp (LVL_USER, Origin.unknown_origin,
                  pack_pp_with_data NameSetPP.pp Y, [])]),
       Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "map",
            [Exp (LVL_USER, Origin.unknown_origin,
                  pack_pp_with_data pp_map map, [])]),
       Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "interface I",
            [Exp (LVL_USER, Origin.unknown_origin,
                  pack_pp_with_data Interface.pp I, [])]),
       Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "interface J",
            [Exp (LVL_USER, Origin.unknown_origin,
                  pack_pp_with_data Interface.pp J, [])])]
    | explain_IncompleteRenaming _ = raise Match
  val _ = add_explainer
            (mk_explainer
               "instantiation map specifies an incomplete renaming"
               explain_IncompleteRenaming)

  exception InvalidSiteNumber of map * int * interface
  fun explain_InvalidSiteNumber (InvalidSiteNumber (map, s, i)) =
      [Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "map",
            [Exp (LVL_USER, Origin.unknown_origin,
                  pack_pp_with_data pp_map map, [])]),
       Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "site",
            [Exp (LVL_USER, Origin.unknown_origin, mk_int_pp s, [])]),
       Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "interface",
            [Exp (LVL_USER, Origin.unknown_origin,
                  pack_pp_with_data Interface.pp i, [])])]
    | explain_InvalidSiteNumber _ = raise Match
  val _ = add_explainer
            (mk_explainer
               "instantiation map has an invalid site number wrt. interface"
               explain_InvalidSiteNumber)

  exception DuplicateEntries of int * map list
  fun explain_DuplicateEntries (DuplicateEntries (s, maps)) =
      [Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "map list",
            [Exp (LVL_USER, Origin.unknown_origin,
                  pack_pp_with_data pp_map_list maps, [])]),
       Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "site",
            [Exp (LVL_USER, Origin.unknown_origin, mk_int_pp s, [])])]
    | explain_DuplicateEntries _ = raise Match
  val _ = add_explainer
            (mk_explainer
               "instantiation map list has duplicate entries for site"
               explain_DuplicateEntries)

  exception NonLocalInterface of interface
  fun explain_NonLocalInterface (NonLocalInterface i) =
      [Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "interface",
            [Exp (LVL_USER, Origin.unknown_origin,
                  pack_pp_with_data Interface.pp i, [])])]
    | explain_NonLocalInterface _ = raise Match
  val _ = add_explainer
            (mk_explainer
               "instantiation interface is not local"
               explain_NonLocalInterface)


  (* Unless otherwise specified in maps, root i of the instance will be
   * a copy of root i of the original, given that there is a unique
   * one-to-one correspondence between the local names of the two roots.
   * If the names used are the same, they are assumed to map to themselves.
   *)
  fun make {I, J, maps} =
      let
        val m  = Interface.width I
        val n  = Interface.width J
        val Xs = Array.fromList (Interface.loc I)
        val Ys = Array.fromList (Interface.loc J)

        (* Create a rho array on the form described above (type inst) using
         * a completely specified map list, i.e. the list must have, in order,
         * elements
         * [(redex_site_{0}, name_map_{0}), ...,
         *  (redex_site_{n - 1}, name_map_{n - 1})]
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
        fun infer_or_verify_namemap (map as ((rea, []), (red, []))) =
            (* infer (if possible) the local renaming from site red of
             * redex to site rea of reactum, in the form of a name map. *)
            let
              val X = Xs sub red
              val Y = Ys sub rea
            in
              if NameSet.size X <> NameSet.size Y then
                raise IncompatibleSites (map, I, J)
              else if NameSet.eq X Y then
                NameSet.fold
                  (fn n => fn nmap => NameMap.add (n, n, nmap))
                  NameMap.empty X
              else if NameSet.size X = 1 andalso NameSet.size Y = 1 then
                NameMap.fromList
                  [(NameSet.someElement X, NameSet.someElement Y)]
              else
                raise CannotInferLocalRenaming (map, I, J)
            end
          | infer_or_verify_namemap (map as ((rea, reans), (red, redns))) =
            (* verify the given local renaming (reans)/(redns) and construct
             * a corresponding name map.  *)
            let
              val X = Xs sub red
              val Y = Ys sub rea

              val () = if NameSet.size X = NameSet.size Y then ()
                       else raise IncompatibleSites (map, I, J)

              (* Verify a single renaming, i.e. that the names in the given
               * renaming matches the interfaces I and J.
               * Keep track of the used names X' and Y', to make sure that
               * all the local names of the sites are mentioned exactly once
               * in the renaming. *)
              fun verify_ren (x, y, (X', Y', nmap)) =
                  if not (NameSet.member x X) then
                    raise NameNotInInterface (x, map, I)
                  else if not (NameSet.member y Y) then
                    raise NameNotInInterface (y, map, J)
                  else
                    (NameSet.insert x X'
                       handle NameSet.DuplicatesRemoved =>
                         raise DuplicateNames (x, map),
                     NameSet.insert y Y'
                       handle NameSet.DuplicatesRemoved =>
                         raise DuplicateNames (y, map),
                     NameMap.add (x, y, nmap))

              val (X', Y', nmap)
                = (ListPair.foldrEq 
                     verify_ren
                     (NameSet.empty, NameSet.empty, NameMap.empty)
                     (redns, reans))
                  handle ListPair.UnequalLengths =>
                    raise UnequalNameListLengths map
            in
              if NameSet.size X <> NameSet.size X' then
                raise IncompleteRenaming
                        (NameSet.difference X X',
                         NameSet.difference Y Y', 
                         map, I, J)
              else
                nmap
            end

        (* infer missing maps and verify the map list.
         * reasite is the number of the next reactum site to be added to the
         * accumulator acc.
         * The result is a list on the form described in the comment
         * for maps2rho.
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
          | infer_and_verify_maps reasite acc (maps' as ((map as ((rea, reans), (red, redns)))::mapstl)) =
            if rea < 0 orelse n <= rea then
              raise InvalidSiteNumber (map, rea, J)
            else if red < 0 orelse m <= red then
              raise InvalidSiteNumber (map, red, I)
            else if reasite >= 0 then
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
                     maps'
               | GREATER =>
                   raise DuplicateEntries (rea, maps))
            else
              raise DuplicateEntries (rea, maps)

        (* compare function for maps used to sort in decreasing order *)
        fun map_cmp (((rea1,_), _), ((rea2,_), _)) = Int.compare (rea2, rea1)
      in
        if not (Interface.is_local I) then
          raise NonLocalInterface I
        else if not (Interface.is_local J) then
          raise NonLocalInterface J
        else
          {I = I,
           J = J,
           rho = (  maps2rho 
                  o (infer_and_verify_maps (n - 1) [])
                  o (ListSort.sort map_cmp))
                 maps}
      end

  fun make' {I, J} = make {I = I, J = J, maps = []}

  exception IncompatibleParameter of inst * DR bgbdnf
  fun explain_IncompatibleParameter (IncompatibleParameter (inst, d)) =
      [Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "instantiation",
            [Exp (LVL_USER, Origin.unknown_origin,
                  pack_pp_with_data pp inst, [])]),
       Exp (LVL_USER, Origin.unknown_origin, mk_string_pp "parameter",
            [Exp (LVL_USER, Origin.unknown_origin,
                  pack_pp_with_data BgBDNF.pp d, [])])]
    | explain_IncompatibleParameter _ = raise Match
  val _ = add_explainer
            (mk_explainer
               "parameter is not compatible with instantiation"
               explain_IncompatibleParameter)
  

  fun instantiate (inst as {I, J, rho}) d =
      let
        infix 5 **
        fun b1 ** b2 = BgVal.Ten noinfo [b1, b2]
        val oo' = BgVal.Com' noinfo
        infix 7 oo'
        
        val {ren, Ps} = BgBDNF.unmkDR d
        val Ps = Array.fromList Ps

        val d_loc_outerface
          = Interface.make {loc = Interface.loc (BgBDNF.outerface d),
                            glob = NameSet.empty}

        fun copy_P ((i, ls_i), es) =
            (ls_i oo' (BgBDNF.unmk (Ps sub i))) :: es
      in
        if Interface.eq (d_loc_outerface, I) then
          ren oo' (BgVal.Par noinfo (Array.foldr copy_P [] rho))
        else
          raise IncompatibleParameter (inst, d)
      end
end
