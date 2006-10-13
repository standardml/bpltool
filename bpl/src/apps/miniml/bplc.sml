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

structure BG = BG (PrettyPrint);

structure BGGen
  = BGGen
	(structure PrettyPrint = PrettyPrint
         structure BG = BG)

structure MiniMLToBG
  = MiniMLToBG
	(structure PrettyPrint = PrettyPrint
         structure BG = BG
	 structure BGGen = BGGen)

fun say s = TextIO.output(TextIO.stdErr, s ^ "\n")

fun ppToStdErr pptree =
    Pretty.ppPrint pptree (Pretty.plainOutput ("(*","*)")) TextIO.stdErr

fun run () =
    let val infile : string option ref = ref NONE
	val _ = ArgParse.parse NONE (Flags.toSpec()) 
			       (fn file => infile := SOME file)
	val infile = valOf(!infile)
		     handle Option => raise ArgParse.Bad("Missing input file")
	val _ = MiniMLToBG.compile infile "tmp.bg"
    in  ()
    end handle ArgParse.Bad s => (say s; List.app say (Flags.usage()))
	     | MiniMLToBG.CompileError(reason,expl) =>
	          ( say("Compiler error: " ^ reason)
                  ; ppToStdErr expl )
val _ = run()

