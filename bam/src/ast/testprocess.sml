(* Copyright (c) 2007  Henning Niss, IT University of Copenhagen
 *
 * BAM is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 *
 * BAM is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with BAM; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
 * USA
 *)


structure TestTerm :> sig val suite : unit -> Test.test end = struct

    open Assert Process

    (* matches *)
    local 
	fun checkBindings matchmap bindings =
	    let fun loop [] = ()
		  | loop ((v,p)::ps) = 
		    let val SOME p' = lookup matchmap v
		    in  assertEqual equal toString p p' ; ()
		    end handle Bind => fail(v ^ ": unbound variable")
		in  loop bindings
		end
	fun checkMatch (pat, term, expected) =
	    case (match pat term, expected) of
		(NONE, SOME _) => fail("expected bindings")
	      | (SOME _, NONE) => fail("expected no bindings")
	      | (NONE, NONE) => ()
	      | (SOME match, SOME binds) => checkBindings match binds

	val (C1, C2) = ( Control.ctrl("C1", (), Control.ACTIVE) , 
			 Control.ctrl("C2", (), Control.ACTIVE)
		       )

	val C1Pat = PPrefix(C1, PVar "p1")
	val C2Pat = PPrefix(C2, PVar "p2")
	val C1PatNil = PPrefix(C1, PNil)
	val C2PatNil = PPrefix(C2, PNil)
	val C1ParPat = PPar(C1Pat, PVar "p")

	val C1Term = Prefix(C1, Nil)
	val C2Term = Prefix(C2, Nil)
	val larger = Prefix(C1,Par(C2Term,C2Term))
	val large  = Par(C2Term,C1Term)
	val tests = [ (C1ParPat, C1Term, SOME[("p1", Nil), ("p", Nil)])
		    , (C1ParPat, Prefix(C1,Par(C2Term,C2Term)), 
		            SOME[("p1", Par(C2Term,C2Term)), ("p", Nil)])
		    , (C1ParPat, Par(C2Term,C1Term), 
		            SOME[("p1", Nil), ("p", C2Term)])
		    , (PPar(C1PatNil,C1PatNil), C1Term, NONE)
		    , (PPar(C1PatNil,C1PatNil), Par(C1Term,C2Term), NONE)
		    , (PPar(C1PatNil,C1PatNil), Par(C1Term,C1Term), SOME[])
		    , (PPar(C1PatNil,PSuccess), Par(C2Term,C1Term), SOME[])
		    , (PPar(C1PatNil,PPar(C1PatNil,PSuccess)), 
		          Par(C1Term,Par(C2Term,C1Term)), SOME[])
		    ]

    in
        val matchTests = 
	    Test.TestLabel("Match tests", 
			   Test.TestCase(fn () => app checkMatch tests))
    end

    fun suite () = Test.TestLabel("Term tests", Test.TestList[matchTests])

end (* structure TestTerm *)
