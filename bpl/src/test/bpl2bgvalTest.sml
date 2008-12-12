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

structure BPL2bgvalTest =
struct
structure BG = BG (structure ErrorHandler = PrintErrorHandler);
structure B2 = BG.BPL2BgVal (*BPL2BgVal (
  structure Info          = BG.Info
  structure Origin        = Origin
  structure Name          = BG.Name
  structure NameSet       = BG.NameSet
  structure Interface     = BG.Interface
  structure Link          = BG.Link
  structure LinkSet       = BG.LinkSet
  structure Wiring        = BG.Wiring
  structure Permutation   = BG.Permutation
  structure Control       = BG.Control
  structure Ion           = BG.Ion
  structure BgTerm        = BG.BgTerm
  structure BgVal         = BG.BgVal 
  structure BgBDNF        = BG.BgBDNF
  structure Instantiation = BG.Instantiation
  structure Rule          = BG.Rule
  structure BPLTerm       = BG.BPLTerm
  )
*)

fun run _ =
let
open TextIO

open BG
open B2
open BG.BPLTerm
open BG.Control

(*val Passive = BG.Control.Passive
val Active = BG.Control.Active
val Atomic = BG.Control.Atomic
*)

val signat = [(*("k1",Passive,1,2),
	      ("k2",Atomic,0,2),
	      ("k3",Atomic,0,0),*)
	      ("K",Atomic,0,0),
	      ("L",Atomic,0,0),
	      ("M",Passive,0,0),
	      ("null",Atomic,0,0),
	      ("send",Passive,0,2),
	      ("get",Passive,1,1),
	      ("sub",Active,1,0),
	      ("def",Passive,0,1),
	      ("msg",Atomic:B2.kind,0,1)]

fun id1 id = Sit(SiteName(id),[])

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
*)

(* K-L-M example *)
val K = Ion("K",[],[])
val L = Ion("L",[],[])
val M = Ion("M",[],[])
val C = Pri(M,id1 "site")
val a = Ten(K,L)
val b = Ten(L,K)

(* Pi, send/get example *)
val send = Ion("send",[],["a","w"])
val get = Ion("get",["x"],["a"])

(* barren region with local outer name x *)
val region_idle_x = Com(Wir([LInt("x")]),Bar)

(* terms *)
val null' = Ion("null",[],[])                     (* 0 *)
val send_null = Com(send,null')                   (* aw.0 *)
val get_null = Com(get,Pri(null',region_idle_x))  (* a(x).0 *)
val agent = Pri(send_null,get_null)              (* aw.0 | a(x).0 *)

(* sites *)
val site0 = Sit(SiteNum(0),[])
val site1 = Sit(SiteName("s1"),["x"])

(* contexts *)
val send_ion = Com(send,site0)                   (* aw.[0] *)
val get_ion = Com(get,site1)                     (* a(x).[1] *)

val sub__x = Ion("sub",["x"],[])
fun abs_x b = Abs(["x"],b)
fun con_x b = Con(["x"],b)
val def_x = Ion("def",[],["x"])
val msg_w = Ion("msg",[],["w"])
(* it is troublesome to have to globalise names for Pri... *)
val get_sub = Com(sub__x,abs_x(Pri(con_x(site1),Com(def_x,msg_w))))
(*val get_sub = Emb(sub__x,abs_x(Pri(site1,Emb(def_x,msg_w))))*)
val idle__a = Wir([GInt("a")])

(*
rule comm =
  send_aw([0]) | get_a(x)([1]<x>)
    -->
  [0] tt a/ | sub_(x)([1]<x> | ({x})def_x(msg_w))
*)

val decs = [Val("v1",Com(C,a)),
	    Val("v2",Com(C,b)),
	    Val("state",Pri(Ref("v1"),Ref("v2"))),
	    Rul("r1",(K,L)),
	    Val("v_null",null'),
	    Val("v_send",send_null),
	    Val("v_get",get_null),
	    Val("agent",agent),
	    Rul("comm",
		 (Pri(send_ion,get_ion),
		 Pri(Ten(site0,idle__a),get_sub)))]
val prog = (signat,(*"default",*) decs)
val ((*signa,*)mainbgval,rules) = prog2bgval prog
val state = (BG.BgVal.toString o BG.BgVal.simplify) mainbgval

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
fun printRules [] = print "\ndone printing rules...\n"
  | printRules (r::rs) = ( printRule r ; printRules rs)

val _ = print "\nbpl2bgvalTest.sml called...\n\n"

val _ = print "state = "
val _ = print state
val _ = print "\n"
val _ = printIfaces "state" (BG.BgVal.innerface mainbgval) (BG.BgVal.outerface mainbgval)
(*
val h = Rule.toString(List.hd(rules))
val _ = print "printing rules...\n"
val _ = print("rule = " ^ h ^ "\n")
*)
val _ = printRules rules
val _ = print "printing rule_r1 ifaces...\n"
val {name,redex,react,inst,info} = Rule.unmk(List.hd(rules))
val _ = printIfaces "redex" (BG.BgBDNF.innerface redex) (BG.BgBDNF.outerface redex)
val _ = printIfaces "react" (BgVal.innerface react) (BgVal.outerface react)
val _ = print "\n"
val _ = print "printing rule_comm ifaces...\n"
val {name,redex,react,inst,info} = Rule.unmk(List.hd(List.tl(rules)))
val _ = printIfaces "redex'" (BG.BgBDNF.innerface redex) (BG.BgBDNF.outerface redex)
val _ = printIfaces "react'" (BG.BgVal.innerface react) (BG.BgVal.outerface react)
val _ = print "\n"

in
  ()
end
end

val _ = BPL2bgvalTest.run ();
