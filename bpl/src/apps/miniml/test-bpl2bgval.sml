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

val Passive = Control.Passive
val Active = Control.Active
val Atomic = Control.Atomic

val signat = [Cdef("k1",Passive,1,2),
	      Cdef("k2",Atomic,0,2),
	      Cdef("k3",Atomic,0,0),
	      Cdef("K",Atomic,0,0),
	      Cdef("L",Atomic,0,0),
	      Cdef("M",Passive,0,0),
	      Cdef("null",Atomic,0,0),
	      Cdef("send",Passive,0,2),
	      Cdef("get",Passive,1,1),
	      Cdef("sub",Active,1,0),
	      Cdef("def",Passive,0,1),
	      Cdef("msg",Atomic,0,1)]

fun id1 id = Site(Var(id),Namelist([]))

(*
val b0 = Empty
val b1 = Ion("k1",["x"],["y","z"])
val b2 = Abs(["x"], Ion("k2",[],["x","q"]))
val b3 = Emb(b1,Com(b2,Empty))
val b4 = Clo(["y"],b3)
val b5 = Pri(Par(b1,b4),Ten(b3,Empty))
val b6 = Par(Id("id1"),Id("id5"))
val b7 = Ten(Wir([Local("y","x"),Global("q","q")]),b0)
val b8 = Emb(Wir([IdleG("v")]),Empty)
val b9 = Ten(b7,b8)
val b10 = Ion("k2",[],["f1","f2"])
val b11 = Ion("k3",[],[])

val K = Ion("K",[],[])
val L = Ion("L",[],[])
val M = Ion("M",[],[])
val C = Pri(M,id1 "site")
val a = Ten(K,L)
val b = Ten(L,K)
*)

(* Pi *)
val send = Ion("send",[],["a","w"])
val get = Ion("get",["x"],["a"])

(* barren region with local outer name x *)
val region_idle_x = Com(Wir([IdleL("x")]),Empty)

(* terms *)
val null = Ion("null",[],[])                     (* 0 *)
val send_null = Emb(send,null)                   (* aw.0 *)
val get_null = Emb(get,Pri(null,region_idle_x))  (* a(x).0 *)
val agent = Pri(send_null,get_null)              (* aw.0 | a(x).0 *)

(* sites *)
val site0 = Site(Num(0),Namelist([]))
val site1 = Site(Var("s1"),Namelist(["x"]))

(* contexts *)
val send_ion = Emb(send,site0)                   (* aw.[0] *)
val get_ion = Emb(get,site1)                     (* a(x).[1] *)

val sub__x = Ion("sub",["x"],[])
fun abs_x b = Abs(["x"],b)
fun con_x b = Conc(["x"],b)
val def_x = Ion("def",[],["x"])
val msg_w = Ion("msg",[],["w"])
(* it is troublesome to have to globalise names for Pri... *)
val get_sub = Emb(sub__x,abs_x(Pri(con_x(site1),Emb(def_x,msg_w))))

val idle__a = Wir([IdleG("a")])

(*
rule comm =
  send_aw([0]) | get_a(x)([1]<x>)
    ->
  [0] tt a/ | sub_(x)([1]<x> | ({x})def_x(msg_w))
*)

val decs = [(*Value("v1",Com(C,a)),
	    Value("v2",Com(C,b)),
	    Value("state",Pri(Id("v1"),Id("v2"))),
	    Rule("r1",K,L),*)
	    Value("v_null",null),
	    Value("v_send",send_null),
	    Value("v_get",get_null),
	    Value("agent",agent),
	    Rule("comm",
		 Pri(send_ion,get_ion),
		 Pri(Ten(site0,idle__a),get_sub))]
val prog = Prog(signat,decs)
val (signa,mainbgval,rules) = prog2bgval prog
val state = (B.toString o B.simplify) mainbgval

(* printing *)
fun printRule r = print(Rule.toString r)
(*
    let val {name,redex,react,inst,info} = Rule.unmk r
	val redex' = (B.toString o B.simplify o BgBdnf.unmk) redex
	val react' = (B.toString o B.simplify) react
	val inst' = "dummyinst"
    in ( print("name: " ^ name ^ "\n")
       ; print("redex: " ^ redex' ^ "\n")
       ; print("react: " ^ react' ^ "\n")
       ; print("inst: " ^ inst' ^ "\n") )
    end
*)
fun printRules [] = print "done printing rules...\n"
  | printRules (r::rs) = ( printRule r ; printRules rs)

val _ = print "state = "
val _ = print state
val _ = print "\n"
val _ = printIfaces "state" (B.innerface mainbgval) (B.outerface mainbgval)
val h = Rule.toString(List.hd(rules))
(*
val _ = print "printing rules...\n"
val _ = print("rule = " ^ h ^ "\n")
val _ = printRules rules
val _ = print "\n"
*)
