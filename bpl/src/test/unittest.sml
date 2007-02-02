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

(** Code for testing BPL. 
 * @version $LastChangedRevision$
 *)

structure Test = struct

    structure Assert           = SMLUnit.Assert
    structure Test             = SMLUnit.Test
    structure TextUITestRunner = SMLUnit.TextUITestRunner

    structure BG = BG (structure ErrorHandler = PrintErrorHandler);

    structure BGTest 
      = BGTest
	    (structure Assert = Assert
             structure Test = Test)

    structure BDNFTest 
      = BDNFTest
	    (structure ErrorHandler = PrintErrorHandler
             structure Assert = Assert
             structure Test = Test
             val       test_data_dir = "kernel/ast/test")

    structure MatchTest 
      = MatchTest
	    (structure Assert = Assert
             structure Test = Test)

    structure BGGen
      = BGGen
	    (structure PrettyPrint = PrettyPrint
             structure BG = BG)
    structure MiniMLToBG
      = MiniMLToBG
	    (structure PrettyPrint = PrettyPrint
             structure BG = BG
	     structure BGGen = BGGen)
    structure MiniMLTest  
      = MiniMLTest
	    (structure Assert = Assert
             structure Test = Test
	     structure MiniMLToBG = MiniMLToBG
	     structure BG = BG)

    fun run () =
	let
	    fun run1 (name, dir, suite) =
		let val cwd = OS.FileSys.getDir ()
		in  OS.FileSys.chDir dir
                  ; print ("\nRunning " ^ name ^ " test suite: ")
		  ; TextUITestRunner.runTest {output = TextIO.stdErr} (suite())
                  ; print ("\n")
		  ; OS.FileSys.chDir cwd
		end
	    val tests = [ ("BG", "kernel/bg/test",   BGTest.suite)
			, ("BDNF", "kernel/ast/test",  BDNFTest.suite ".")
			, ("MiniML", "apps/miniml/test", MiniMLTest.suite)
			]
	    fun say s = TextIO.print(s^"\n")
	in
	    List.app run1 tests
          ; if Flags.getBoolFlag "/misc/timings"
	    then List.app say (Timings.listAll ())
	    else ()
	end
end

val _ = Test.run ()
