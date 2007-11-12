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
 Ebbe Elsborg, November 9 2007

 Compile: cd <src-dir of BPL-root>; make test-bpl2bgval
 Run: ./test-bpl2bgval
*)

open TextIO;

structure B2 = Bpl2bgval

open B2;

val signat = [Cdef("k1",Control.Passive,1,2),
	      Cdef("k2",Control.Atomic,0,2)]

val b0 = Empty
val b1 = Ctrl("k1",["x"],["y","z"])
val b2 = Abs(["x"], Ctrl("k2",[],["x","q"]))
val b3 = Emb(b1,Com(b2,Empty))
val b4 = Clo(["y"],b3)
val b5 = Pri(Par(b1,b4),Ten(b3,Empty))
val b6 = Par(Id("id1"),Id("id5"))
val b7 = Ten(Wir([Local("y","x"),Global("q","q")]),b0)
val b8 = Emb(Wir([IdleG("v")]),Empty)
val b9 = Ten(b7,b8)

(*HERE: try out rules!*)

val decs = [Value("id1", b0),
	    Value("id5", b5),
	    Value("id6", b6),
	    Value("id", b9)]
val prog = Prog(signat,decs)
val (s,b,r) = prog2bgval prog
val bgval = (B.toString o B.simplify) b

val _ = print bgval
val _ = print "\n"

(*
datatype wire = Global of id * id
	      | Local of id * id
	      | IdleG of id
	      | IdleL of id

datatype bigraph = Wir of wires
		 | Par of bigraph * bigraph
		 | Pri of bigraph * bigraph
		 | Com of bigraph * bigraph
		 | Emb of bigraph * bigraph
		 | Ten of bigraph * bigraph
		 | Ctrl of ctrlid * ports * ports
		 | Clo of names * bigraph
		 | Abs of names * bigraph
		 | Site of siteId * namelist
		 | Id of id
		 | Empty
*)
