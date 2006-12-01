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
		      sharing type BGGen.bg = BG.BgVal.bgval = BG.BgBDNF.bgval)
	: MINIMLTOBG
=  struct

    val dump_frontend =
	Flags.makeBoolFlag{name="/dump/frontend",
			   short="",long="dump-frontend",arg="",
			   desc="Dump frontend compiled MiniML program",default=false}
    val dump_bgval =
	Flags.makeBoolFlag{name="/dump/bgval",arg="",
			   short="",long="dump-bgval",default=false,
			   desc="Dump BG term prior to normalization"}

    val statistics =
	Flags.makeBoolFlag{name="/compile/stats",arg="",
			   short="",long="stats",default=false,
			   desc="Collect statistics (besides timings) of the compilation"}

    fun pps os = PrettyPrint.mk_ppstream 
		     { consumer = fn s => TextIO.output(os, s)
		     , linewidth = 72
		     , flush = fn () => TextIO.flushOut(os)
		     }

    local open Pretty 
    in
    exception CompileError of string * style pptree

    fun explain file exn =
	case exn of 
	    Util.ShouldntHappen code => 
	       ppString("Internal error; grep for "^Int.toString code^" in the source")
	  | TypeInference.TypeError (p, msg) => 
	       SourceLocation.ppSourceLocation file p (List.map ppString ("Type error"::msg))
	  | MatchCompiler.NonExhaustiveMatch p =>
	       SourceLocation.ppSourceLocation file p [ppString "Non-exhaustive match"]
	  | exn => raise exn
    end

	
    fun handler file reason exn =
	( raise CompileError(reason ^ ": ", explain file exn)
        )

    val frontend_timer : (unit -> (MatchCompiler.pos, MiniML.pat) MiniML.prog) -> (MatchCompiler.pos, MiniML.pat) MiniML.prog =
	Timings.add {name="/bplc/1/frontend",desc="Frontend time"}
    val codegen_timer : (unit -> BGGen.bg) -> BGGen.bg =
	Timings.add {name="/bplc/2/codegen",desc="Code generation time"}
    val normalize_timer : (unit -> BG.BgBDNF.B BG.BgBDNF.bgbdnf) -> BG.BgBDNF.B BG.BgBDNF.bgbdnf =
	Timings.add {name="/bplc/3/normalize",desc="BgTerm normalization"}

    fun compile infile outfile =
	let 
	    val _ = Dump.setPrefix infile

            (* Frontend issues: parsing, type inference, match compilation,
                  alpha-renaming
            *)
	    val ast = frontend_timer (fn () =>
		let 
		    val ast = MiniMLParser.parseFile infile
		    val ast = Desugar.desugar ast
		    val ast = TypeInference.inference (0,0) (fn x => x) ast
		    val ast = MatchCompiler.compile (fn (p,t)=>p) 
				    ((0,0),TypeExp.unitty()) (fn (p,t)=>p) ast
		    val ast = MiniML.fresh MiniML.freshPat ast
		in  ast
		end)
		handle exn => handler infile "Frontend failed" exn

	    val size_ast = if !statistics then MiniML.size ast
			   else 0

	    (* possibly dump frontend compiled miniml term *)
	    val _ = 
		if !dump_frontend then
		    if !MiniML.dump_fresh then ()
		    else Dump.pretty (MiniML.pp' MiniML.ppPat) "frontend" ast
		else ()

            (* Code generation: convert to bgval, then normalize *)
	    val bpl = codegen_timer (fn () => BGGen.toBG ast)
			handle exn => handler infile "toBG failed" exn
	    val size_bpl = BG.BgVal.size bpl
            (* possibly dump bgval term *)
	    val _ = 
		if !dump_bgval then
		    Dump.pp BGGen.pp "bgval" bpl
		else ()
	    val bpl = normalize_timer (fn () => BG.BgBDNF.make bpl)
		        handle exn => handler infile "Normalization failed" exn
	    val size_bdnf = if !statistics then BG.BgBDNF.size bpl
			    else 0

            (* Output result *)
	    val _ = 
		let val os = TextIO.openOut outfile
		    val ps = pps os
		in  BG.BgBDNF.pp 0 ps bpl
		  ; PrettyPrint.flush_ppstream ps
		  ; TextIO.closeOut os
		end
	in  
	    if !statistics then
		List.app print 
		     [ "Sizes:\n"
		     , "MiniML AST : ", Int.toString size_ast, "\n"
		     , "Bg before normalization : ", Int.toString size_bpl, "\n"
		     , "Bg after normalization :  ", Int.toString size_bdnf,"\n"
                     ]
	    else ()
	end

end (* structure MiniMLToBG *)
