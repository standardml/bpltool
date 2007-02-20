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
 
(** Error handler for BPLweb.
 * @version $LastChangedRevision$
 *)

structure BPLwebErrorHandler : ERRORHANDLER =
struct
  open Debug

  type ppstream    = PrettyPrint.ppstream
  type break_style = PrettyPrint.break_style
  type origin      = Origin.origin

  datatype explanation
    = Exp of debug_level * origin * (int -> ppstream -> unit) * explanation list
  type explainer   = exn -> explanation

  fun pp_nothing indent pps = ()

  fun concat_pps bs pps indent ppstream =
      let
        open PrettyPrint
        fun << () = begin_block ppstream bs indent
        fun >> () = end_block ppstream
        fun brk () = add_break ppstream (1, 0)
        fun pp_pps [] = ()
          | pp_pps [pp] = pp indent ppstream
          | pp_pps (pp::pps) =
            (pp indent ppstream;
             brk();
             pp_pps pps)
      in
        <<();
        pp_pps pps;
        >>()
      end

  fun mk_string_pp s indent pps = PrettyPrint.add_string pps s

  fun mk_int_pp i indent pps = PrettyPrint.add_string pps (Int.toString i)

  fun mk_list_pp ldelim rdelim sep pp list indent pps =
      (PrettyPrint.add_string pps ldelim;
       PrettyPrint.begin_block pps PrettyPrint.INCONSISTENT 0;
       (case list of
          []    => ()
        | e::es =>
          (pp indent pps e;
           app (fn e => (PrettyPrint.add_string pps sep;
                         PrettyPrint.add_break pps (1, 0);
                         pp indent pps e))
              es));
       PrettyPrint.add_string pps rdelim;
       PrettyPrint.end_block pps)

  fun mk_list_pp' ldelim rdelim sep pp indent pps list =
      mk_list_pp  ldelim rdelim sep pp list indent pps

  fun pack_pp_with_data pp data indent pps =
      pp indent pps data

  (** Page width used by explain. *)
  val pageWidth = ref 80
  (** Block indentation used by explain. *)
  val indent = ref 2

  val explainers = ref [] : explainer list ref

  fun add_explainer (e : explainer) =
      explainers := e :: !explainers

  (* Prettyprinter which uses rpp for recursive calls. *)
  fun pp'' rpp indent pps (Exp (_, orig, pp_exp, exps)) =
      let
        open PrettyPrint
        val show = add_string pps
        fun << () = begin_block pps CONSISTENT 0
        fun >> () = end_block pps
        fun brk () = add_break pps (1, 0)
      in
        <<();
        show "{";
        (case Origin.unmk_origin orig of
          Origin.FILE_ORIGIN (field, pos) =>
          (show ("\"field\": \"" ^ field ^ "\"");
           (case pos of
              Origin.POS ((lin1, col1), (lin2, col2))
            =>(show ","; add_break pps (1, 1);
               show ("\"from\": {\"line\": " ^ Int.toString lin1
                     ^ ", \"col\": " ^ Int.toString col1 ^ "},");
               add_break pps (1, 1);
               show ("\"to\": {\"line\": " ^ Int.toString lin2
                     ^ ", \"col\": " ^ Int.toString col2 ^ "}"))
            | _ => ());
           show ","; brk ())
        | _ => ());
        begin_block pps INCONSISTENT 0;
        show "\"txt\": ";
        add_break pps (1, 2);
        show "\"";
        pp_exp indent pps;

        if null exps then
          show "\""
        else
         (show "\",";
          brk();
          <<();
          show "\"suberrors\": ["; add_break pps (0, 2);
          foldl (fn (e as Exp (dbg_lvl, _, _, _), first) =>
                    (if first then
                       ()
                     else
                       (debug dbg_lvl (fn () => (show ", "; add_break pps (1, 2))) ());
                     rpp indent pps e;
                     false)) true exps;
          >>();
          show "]"
          );
        
        show "}";
        >>();
        >>()
      end

  fun pp' indent = pp'' pp' indent

  fun pp indent pps (e as Exp (dbg_lvl, _, _, _)) =
      debug dbg_lvl (pp'' pp indent pps) e

  fun explain''' e []
    = Exp (LVL_USER, Origin.unknown_origin,
           (fn _ => fn pps => PrettyPrint.add_string pps (exnMessage e)), [])
    | explain''' e (explainer::exps) =
      (explainer e) handle Match => explain''' e exps

  fun explain'' e = explain''' e (!explainers)

  fun explain' e =
      PrettyPrint.pp_to_string (!pageWidth) (pp (!indent)) (explain'' e)

  (* Remove \n from within double quotes. *)
  fun jsonify s =
    let
      fun inquotes (#"\"" :: cs) = #"\"" :: notinquotes cs
        | inquotes (#"\\" :: c :: cs) = #"\\" :: c :: inquotes cs
        | inquotes (#"\n" :: cs) = #"\\" :: #"n" :: inquotes cs 
        | inquotes (c :: cs) = c :: inquotes cs
        | inquotes [] = []
      and notinquotes (#"\"" :: cs) = #"\"" :: inquotes cs
        | notinquotes (#"~" :: d :: cs)
        = if Char.isDigit d then
            #"-" :: d :: notinquotes cs
          else
            #"~" :: notinquotes (d :: cs)
        | notinquotes (c :: cs) = c :: notinquotes cs
        | notinquotes [] = []
    in
      implode (notinquotes (explode s))
    end
    
  fun explain e =
       (print "ERROR\n[";
        print (jsonify (explain' e));
        print "]\nEND\n";
        TextIO.flushOut TextIO.stdOut)
end 