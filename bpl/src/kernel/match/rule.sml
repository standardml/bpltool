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

(** Abstract data type for modelling rules.
 * @version $LastChangedRevision$
 *)
functor Rule (structure Info : INFO
              structure Interface : INTERFACE
              structure NameSet : MONO_SET
              structure BgVal : BGVAL
              structure BgBDNF : BGBDNF
              structure Instantiation : INSTANTIATION
              structure ErrorHandler : ERRORHANDLER
                  where type ppstream    = PrettyPrint.ppstream
                    and type break_style = PrettyPrint.break_style
                    and type origin      = Origin.origin
              sharing type NameSet.Set =
                           Interface.nameset
              sharing type Interface.interface =
                           BgVal.interface =
                           BgBDNF.interface =
                           Instantiation.interface
              sharing type BgVal.bgval =
                           BgBDNF.bgval
              sharing type Info.info =
                           BgBDNF.info =
                           BgVal.info
             ) : RULE
              where type bgval = BgVal.bgval
                and type 'a bgbdnf = 'a BgBDNF.bgbdnf
                and type BR = BgBDNF.BR
                and type inst = Instantiation.inst =
struct
  open Debug
  open ErrorHandler
  val _ = Flags.makeBoolFlag {
    name = "/kernel/match/rule/ppsimpleredex",
    desc = "Simplify redex when displaying rules",
    short = "",
    long = "--ppsimpleredex",
    arg = "",
    default = true}
  val _ = Flags.makeBoolFlag {
    name = "/kernel/match/rule/ppsimplereactum",
    desc = "Simplify reactum when displaying rules",
    short = "",
    long = "--ppsimplereactum",
    arg = "",
    default = true}
  type info = Info.info
  type bgval = BgVal.bgval
  type 'a bgbdnf = 'a BgBDNF.bgbdnf
  type BR = BgBDNF.BR
  type inst = Instantiation.inst
  datatype rule =
    Rule of {name : string, redex : BR bgbdnf, react : bgval, inst : inst, info : info}

  val file_origin = Origin.mk_file_origin
                      "$BPL/src/kernel/match/rule.sml"
                      Origin.NOPOS
  fun mk_explainer errtitle (explainer : exn -> explanation list) e =
      Exp (LVL_USER, Origin.unknown_origin, mk_string_pp errtitle,
           explainer e)

  exception OuterfaceMismatch of BR bgbdnf * bgval
  fun explain_OuterfaceMismatch (OuterfaceMismatch (redex, react)) =
      [Exp (LVL_USER, Origin.unknown_origin,
            mk_string_pp ("Redex and reactum outer face must be equal"),
            [Exp (LVL_USER, Info.origin (BgBDNF.info redex),
                  pack_pp_with_data BgBDNF.ppWithIface redex, []),
             Exp (LVL_USER, Info.origin (BgVal.info react),
                  pack_pp_with_data BgVal.ppWithIface react, [])])]
    | explain_OuterfaceMismatch _ = raise Match
  val _ = add_explainer
            (mk_explainer
               "Redex/reactum outer face mismatch"
               explain_OuterfaceMismatch)

  (** Construct a rule.  The instantiation must be compatible
   * with redex and reactum inner faces, i.e., instantiate the
   * inner face of reactum from the inner face of redex.
   * @params {redex, react, inst}
   * @param redex  Redex bigraph
   * @param react  Reactum bigraph
   * @param inst   Instantiation
   *)
  fun make (r as {redex, react, ...}) =
      if Interface.eq (BgBDNF.outerface redex, BgVal.outerface react)
      then
        Rule r
      else
        raise OuterfaceMismatch (redex, react)
  fun make' {name, redex, react, info} =
      let
        val I
          = Interface.make {loc = Interface.loc (BgBDNF.innerface redex),
                            glob = NameSet.empty}
        val J
          = Interface.make {loc = Interface.loc (BgVal.innerface react),
                            glob = NameSet.empty}
      in
	if Interface.eq (BgBDNF.outerface redex, BgVal.outerface react)
	then
          Rule
           {name = name, redex = redex, react = react,
            inst = Instantiation.make' {I = I, J = J},
            info = info}
	else
	  raise OuterfaceMismatch (redex, react)
      end
  (** Deconstruct a rule. @see make. *)
  fun unmk (Rule r) = r
  (** Prettyprint a rule.
   * @params indent pps r
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param r       The rule to output.
   *)
  fun pp indent pps (Rule {name, redex, react, inst, info}) =
    let
      open PrettyPrint
      val show = add_string pps
      fun << () = begin_block pps CONSISTENT 0
      fun <<< () = begin_block pps INCONSISTENT 0
      fun >> () = end_block pps
      fun >>> () = end_block pps
      fun brk () = add_break pps (1, 1)
      fun brk0 () = add_break pps (1, 0)
    in
      <<<();
      if name <> "" then (show name; brk(); show ":::"; brk()) else ();
      <<();
      if Flags.getBoolFlag "/kernel/match/rule/ppsimpleredex" then
        BgVal.pp indent pps (BgVal.simplify (BgBDNF.unmk redex))
      else
        BgBDNF.pp indent pps redex;
      brk0();
      show "--";
      if Instantiation.trivial inst then
        ()
      else
        Instantiation.pp indent pps inst;
      show "--|>"; brk0();
      if Flags.getBoolFlag "/kernel/match/rule/ppsimplereactum" then
        BgVal.pp indent pps (BgVal.simplify react)
      else
        BgVal.pp indent pps react;
      >>();
      >>>()
    end
    
  fun oldpp indent pps (Rule {name, redex, react, inst, info}) =
    let
      open PrettyPrint
      val show = add_string pps
      fun << () = begin_block pps CONSISTENT 0
      fun >> () = end_block pps
      fun brk () = add_break pps (1, 1)
    in
      <<();
      show "{";
      <<(); show "redex"; brk(); show "= ";
      BgBDNF.pp indent pps redex;
      show ","; >>(); brk();
      <<(); show "react"; brk(); show "= ";
      BgVal.pp indent pps react;
      show ","; >>(); brk();
      <<(); show "inst"; brk(); show "= ";
      Instantiation.pp indent pps inst;
      >>();
      show "}";
      >>()
    end
  fun toString rule
    = PrettyPrint.pp_to_string
        (Flags.getIntFlag "/misc/linewidth") 
        (pp (Flags.getIntFlag "/misc/indent"))
        rule
end
