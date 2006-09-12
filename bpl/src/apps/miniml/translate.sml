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
 * @version $Revision: 1.19 $
 * Modified: $Date: 2006/09/04 09:31:14 $ by: $Author: hniss $
 *)


structure BG = BGADT(type info = int * int
                       val noinfo = (~1, ~1)
		       val bgvalinfo2pos = fn x => x
		       structure PrettyPrint = PrettyPrint)

structure BGGen = BGGen(structure PrettyPrint = PrettyPrint
			  structure BG = BG)

fun error s = TextIO.output(TextIO.stdErr, s)

(* Command line processing *)
fun stringOption () =
    let val r = ref NONE
	fun set s = r := SOME s
	fun get () = !r
    in  (set, get)
    end
fun boolOption initial = 
    let val r = ref initial
	fun set () = r := true
	fun clr () = r := false
	fun get () = !r
    in  (set,clr,get)
    end

val (setPrelude,getPrelude) = stringOption ()
val (setOutput,getOutput) = stringOption ()
val (setDebug,_,getDebug) = boolOption false
val specs = [ ("-p", ArgParse.String setPrelude)
            , ("-o", ArgParse.String setOutput)
            , ("-d", ArgParse.Unit   setDebug)
	    ]

(* PP streams *)
fun pps os = PrettyPrint.mk_ppstream 
		 { consumer = fn s => TextIO.output(os, s)
		 , linewidth = 72
		 , flush = fn () => TextIO.flushOut(os)
		 }


fun ppToStdErr pptree =
    Pretty.ppPrint pptree (Pretty.plainOutput ("(*","*)")) TextIO.stdErr

fun ppError file pos msg =
    ppToStdErr(ErrorLoc.ppErrorLocation file pos 
					(List.map Pretty.ppString msg))
    

(* Main translator *)
fun phase prompt pp f a =
    let val prog = f a
    in
	if getDebug()
	then ( print (prompt ^ "\n")
	     ; Pretty.ppPrint (pp prog) 
			      (Pretty.plainOutput("(*","*)")) TextIO.stdOut
             ; print "\n"
             )
	else ()
      ; prog
    end

fun translate file =
    let open MiniML
	fun bindsOf (Prog binds) = binds

	val prelude = 
	    case getPrelude() of
		NONE => []
	      | SOME prelude => bindsOf (MiniMLParser.parseFile prelude)

	val pp = MiniML.pp MiniML.ppPat
	val ppWithPats = MiniML.pp Pattern.ppPat

	val miniml = phase "Parsed MiniML program:" 
			ppWithPats MiniMLParser.parseFile file
	val miniml = Prog(prelude @ bindsOf(MiniMLParser.parseFile file))
	val miniml = TypeInference.inference (fn x => x) (fn pos => fn tau => pos) miniml
	val miniml = phase "Match-compiled MiniML program:" 
			pp (MatchCompiler.compile (fn x=>x) (0,0)) miniml
	val miniml = phase "Alpha-renamed MiniML program:"
                        pp (fresh freshPat) miniml

	val outstream = case getOutput() of NONE => TextIO.stdOut
					  | SOME f => TextIO.openOut f
	val ppsError = pps TextIO.stdErr
	val pps = pps outstream

        val bgs = BGGen.toBG miniml

	fun ppBpl pp (BGGen.Some bpl) = 
	    ( pp 0 pps bpl
	    ; PrettyPrint.flush_ppstream pps
	    )
	  | ppBpl pp (BGGen.None exn) =
	    ( print "! Error:" 
	    ; BG.BGErrorHandler.explain exn
            ; PrettyPrint.flush_ppstream ppsError
            ; print "\n"
            )
	fun get (BGGen.Some bpl) = SOME bpl
	  | get (BGGen.None _  ) = NONE

    in  ppBpl BGGen.pp bgs
      ; PrettyPrint.flush_ppstream pps
(*
      ; List.app (ppBpl BGGen.pp')
	     (List.mapPartial (Option.map BGGen.normalize o get o #2) bgs)
*)
      ; print "\n"
      ; case getOutput() of SOME f => TextIO.closeOut outstream
			  | NONE => ()
    end
      handle IO.Io({name,...}) => error("! Error opening "^name^"\n")
	   | OS.SysErr(msg,_) => error("! Error opening "^file^": "^msg^"\n")
	   | Fail("ParseError") => error("! Error during parse\n")
	   | Util.ShouldntHappen i =>
	        error("! Internal error; grep for the number `" 
		      ^ Int.toString i ^ "' in the source\n")
	   | TypeInference.TypeError(pos, m::msgs)
	     => ppError file pos ("Type error: "^m::msgs)
	   | MatchCompiler.NonExhaustiveMatch pos
	     => ppError file pos ["Non exhaustive match"]

fun main _ =
    let val (setFile,getFile) = stringOption ()
	val _ = ArgParse.parse NONE specs setFile
    in  case getFile () of
	    NONE => error("! Error: expected a file name\n")
	  | SOME f => translate f
    end

val _ = main ()
