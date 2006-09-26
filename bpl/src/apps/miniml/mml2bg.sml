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

(** Command-line wrapper for the translation from MiniML to bigraphs.
 * @version $LastChangedRevision$
 * Modified: $Date: 2006/09/04 20:54:23 $ by: $Author: hniss $
 *)

functor MiniMLToBG(structure BG : BG_ADT
		        where type BgBDNF.ppstream = PrettyPrint.ppstream
                    structure BGGen : BGGEN
		        where type ppstream = PrettyPrint.ppstream
		      sharing type BGGen.bpl = BG.BgVal.bgval = BG.BgBDNF.bgval)
=  struct

    val dump_desugar =
	Flags.makeBoolFlag{name="/dump/desugar",
			   short="",long="dump-desugar",arg="",
			   desc="Dump desugared MiniML program",default=false}
    val dump_bgval =
	Flags.makeBoolFlag{name="/dump/bgval",arg="",
			   short="",long="dump-bgval",default=false,
			   desc="Dump BG term prior to normalization"}

    exception CompileError of string

    fun pps os = PrettyPrint.mk_ppstream 
		     { consumer = fn s => TextIO.output(os, s)
		     , linewidth = 72
		     , flush = fn () => TextIO.flushOut(os)
		     }

    fun explain exn =
	case exn of 
	    Util.ShouldntHappen code => "Internal error; grep for " ^ Int.toString code ^ " in the source"
	  | TypeInference.TypeError (p, msg) => String.concat("type error\n"::msg)
	  | exn => BG.BGErrorHandler.explain' exn
	
    fun handler reason exn =
	( raise CompileError(reason ^ ": " ^ explain exn)
        )

    fun compile infile outfile =
	let 
	    val _ = Dump.setPrefix infile

            (* Frontend issues: parsing, type inference, match compilation,
                  alpha-renaming
            *)
	    val ast =
		let 
		    val ast = MiniMLParser.parseFile infile
		    val ast = TypeInference.inference 
				  (fn x => x) (fn pos => fn tau => pos) ast
		    val ast = MatchCompiler.compile (fn x=>x) (0,0) ast
		    val ast = MiniML.fresh MiniML.freshPat ast
		in  ast
		end
		handle exn => handler "Frontend failed" exn

	    (* possibly dump desugared miniml term *)
	    val _ = 
		if !dump_desugar then
		    if !MiniML.dump_fresh then ()
		    else Dump.pretty (MiniML.pp MiniML.ppPat) "desugar" ast
		else ()

            (* Code generation: convert to bgval, then normalize *)
	    val bpl = case BGGen.toBG ast of
			  BGGen.Some bpl => bpl
			| BGGen.None exn => handler "toBG failed" exn
            (* possibly dump bgval term *)
	    val _ = 
		if !dump_bgval then
		    Dump.pp BGGen.pp "bgval" bpl
		else ()
	    val bpl = BG.BgBDNF.make bpl
		        handle exn => handler "Normalization failed" exn

            (* Output result *)
	    val os = TextIO.openOut outfile
	    val ps = pps os
	in  BG.BgBDNF.pp 0 ps bpl
          ; PrettyPrint.flush_ppstream ps
	  ; TextIO.closeOut os
	end

end (* structure MiniMLToBG *)
