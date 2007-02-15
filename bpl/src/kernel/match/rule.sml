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
 * @version $LastChangedRevision: 397 $
 *)
functor Rule (structure Interface : INTERFACE
              structure NameSet : MONO_SET
              structure BgVal : BGVAL
              structure BgBDNF : BGBDNF
              structure Instantiation : INSTANTIATION
              sharing type NameSet.Set =
                           Interface.nameset
              sharing type Interface.interface =
                           BgVal.interface =
                           BgBDNF.interface =
                           Instantiation.interface
             ) : RULE
              where type bgval = BgVal.bgval
                and type 'a bgbdnf = 'a BgBDNF.bgbdnf
                and type BR = BgBDNF.BR
                and type inst = Instantiation.inst =
struct
  type bgval = BgVal.bgval
  type 'a bgbdnf = 'a BgBDNF.bgbdnf
  type BR = BgBDNF.BR
  type inst = Instantiation.inst
  type rule = {name : string, redex : BR bgbdnf, react : bgval, inst : inst}
  (** Construct a rule.  The instantiation must be compatible
   * with redex and reactum inner faces, i.e., instantiate the
   * inner face of reactum from the inner face of redex.
   * @params {redex, react, inst}
   * @param redex  Redex bigraph
   * @param react  Reactum bigraph
   * @param inst   Instantiation
   *)
  fun make r = r
  fun make' {name, redex, react} =
      let
        val I
          = Interface.make {loc = Interface.loc (BgBDNF.innerface redex),
                            glob = NameSet.empty}
        val J
          = Interface.make {loc = Interface.loc (BgVal.innerface react),
                            glob = NameSet.empty}
      in
        {name = name, redex = redex, react = react,
         inst = Instantiation.make' {I = I, J = J}}
      end
  (** Deconstruct a rule. @see make. *)
  fun unmk r = r
  (** Prettyprint a rule.
   * @params indent pps r
   * @param indent  Indentation at each block level.
   * @param pps     Prettyprint stream on which to output.
   * @param r       The rule to output.
   *)
  fun pp indent pps {name, redex, react, inst} =
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