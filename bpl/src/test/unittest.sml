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

(** Code for testing BPL under SML/NJ. 
 * @version $LastChangedRevision$
 *)

structure Test = struct

    structure Assert           = SMLUnit.Assert
    structure Test             = SMLUnit.Test
    structure TextUITestRunner = SMLUnit.TextUITestRunner
    structure BG = BG (PrettyPrint);

    structure BGTest 
      = BGTest
	    (structure Assert = Assert
             structure Test = Test
	     structure BG = BG)

    structure BDNFTest 
      = BDNFTest
	    (structure Assert = Assert
             structure Test = Test
	     structure BG = BG)

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
	    fun run1 (dir, suite) =
		let val cwd = OS.FileSys.getDir ()
		in  OS.FileSys.chDir dir
		  ; TextUITestRunner.runTest {output = TextIO.stdErr} (suite())
		  ; OS.FileSys.chDir cwd
		end
	    val tests = [ ("kernel/bg/test",   BGTest.suite)
			, ("kernel/ast/test",  BDNFTest.suite)
			, ("apps/miniml/test", MiniMLTest.suite)
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
