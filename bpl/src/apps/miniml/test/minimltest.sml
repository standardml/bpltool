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

(* 

   The BG code generator roughly has the following phases:

                        De-sugared,
      MiniML               typed                 BDNF
             -frontend->  MiniML     -codegen->  normal
       prog                prog                  form

   The frontend at least does the following: parses and de-sugares, 
   types, match compiles, and alpha-renames (fresh names) the input
   program.

   The codegenerator generates BG terms by first generating BgVal's
   and subsequently normalizing those to get the normal form.

   In principle we would like to test the following:
   (1) That the frontend spits out correctly typed,
       match-compiled, alpha-renamed, etc MiniML.
   (2) That the code generator spits out the correct BG terms.
   (3) That the normalization spits out the correct normal forms.

   For now we simply test whether the complete pipeline works - 
   ie., we do not compare the output of the various phases to
   expected output.
*)

functor MiniMLTest(structure BG : BG
                   structure MiniMLToBG :
			    sig
                              exception CompileError of string
			      val compile : string -> string -> unit
                            end
                   structure Assert : ASSERT
		  structure Test :
			    sig
			      type testFunction = unit -> unit
			      type test
			      val labelTests : (string * testFunction) list -> test
			    end
		  ) : sig val suite : unit -> Test.test end =
struct

    val suite = 
	let 
	    val testfiles = 
                (* List of files (basenames) to be tested. Each basename
                   gives rise to a file pair by assuming the extension 
                   ".mml" for the input file, and ".bpl" for the expected
                   result.
                *)
		[ "id", "id-app" , "k", "fix", "let", 
                  "unit", "ref", "tuple"
                ]

	    fun test1 infile resfile expectedfile =
		let val _ = MiniMLToBG.compile infile resfile
		    val res = BG.bgvalToString(BG.bgvalUsefile'' resfile)
		    val exp = BG.bgvalToString(BG.bgvalUsefile'' expectedfile)
		in  Assert.assertEqualString res exp ; ()
		end handle MiniMLToBG.CompileError reason => Assert.fail reason
	    fun test1 infile resfile expectedfile =
		let val _ = MiniMLToBG.compile infile resfile
		in  Assert.assertEqualUnit () ()
		end handle MiniMLToBG.CompileError reason => Assert.fail reason
	    fun testfile base =
		(base, fn () => test1 (base^".mml") (base^".tmp.bpl") (base^".bpl"))

	in  Test.labelTests o (fn () => List.map testfile testfiles)
	end

end
