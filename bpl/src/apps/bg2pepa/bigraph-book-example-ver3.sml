(* Example from Milner's book "The Space and Motion of Communicating
 * Agents" extended with symmetric rules allowing agents to leave rooms
 * etc.
 * There are some deviations (though I don't think they are necessary...):
 * - no 'conference call' rules
 * - computers are not connected to a network
 *
 * Implements a variation, where we encode the disconnectedness of ports
 * by connecting them to special ports on the same node; i.e. we rely on
 * an invariant.
 *
 * Compared to version 2, this implements a variant where all nodes in
 * the redexes have a site.
 *
 * NB! Uses the SML/NJ version of the BPLtool command line.
 *)

SMLofNJ.Internals.GC.messages false;
val bg2pepa_dir = OS.FileSys.getDir ();
val _ = OS.FileSys.chDir "../bpl/";
use "smlnj.sml";
val _ = OS.FileSys.chDir bg2pepa_dir;
use "figinfo.sml";

val _ = use_shorthands true;

val _ = CM_make "sources.cm";

fun bgbdnf_eq b1 b2 = BG.BgBDNF.eq b1 b2;
fun bgval_eq b1 b2 = bgbdnf_eq (BG.BgBDNF.make b1) (BG.BgBDNF.make b2);
fun rrule_eq r1 r2 =
  let
    val {name = n1, redex = R1, react = R'1, inst = inst1, ...}
      = BG.Rule.unmk r1
    val {name = n2, redex = R2, react = R'2, inst = inst2, ...}
      = BG.Rule.unmk r2

    val {maps = maps1, ...} = BG.Instantiation.unmk inst1
    val {maps = maps2, ...} = BG.Instantiation.unmk inst2
  in
            n1 = n2
    andalso bgbdnf_eq R1 R2
    andalso bgval_eq R'1 R'2
    andalso BG.Instantiation.eq inst1 inst2
  end

(* structure PCRADT = PCRADT(structure BGADT = BG); *)
structure PCR = PCR(structure BG = BG);

open PCR;
open BgAspects;

val state2absbg
  = ConcreteBigraph.abstract oo ConcreteBigraph.make' oo AspectMap.list;

(* function for creating a PC-rule which also performs
 * some sanity checks. *)
fun make_PCR rr P C =
    let
      val {name, redex = R, react = R', ...} = BG.Rule.unmk rr

      val R_conc = ConcreteBigraph.make' P
      val R_abs  = ConcreteBigraph.abstract R_conc
      val _ = if bgval_eq R R_abs then ()
              else raise Fail "ConcreteBigraph.{abstract o make'} \
                              \is broken"

      val R'_conc = ConcreteBigraph.app C R_conc
      val R'_abs  = ConcreteBigraph.abstract R'_conc
      val _ = if bgval_eq R' R'_abs then ()
              else raise Fail "ConcreteBigraph.{abstract o app} \
                              \is broken"
                     
      val PCR = PCRule.make {name = name,
                             preconds = P,
                             changes = C}
      val PCR_rr = PCRule.reaction_rule PCR
      val _ = if rrule_eq rr PCR_rr then ()
              else raise Fail "PCRule.{reaction_rule o make} is broken"
    in
      PCR
    end


(********************************)
(*     String Declarations      *)
(*                              *)
(* (makes it easier to transfer *)
(* the example to BPLweb)       *)
(********************************)
(* Controls *)
(* val A = "A"; *)
(* val C = "C"; *)
(* val R = "R"; *)
(* Names    *)
(* val x = "x"; *)
(* val y = "y"; *)
(* val z = "z"; *)


(********************************)
(*          Signature           *)
(********************************)
(* val A = atomic (A -: 2); *)
(* val C = atomic (C -: 1); *)
val A = active ("A" -: 4);
val C = active ("C" -: 2);
val R = active0 "R"; (*(R -: 0);*)
val c_A = Control.make ("A", Control.Active, 0, 4);
val c_C = Control.make ("C", Control.Active, 0, 2);
val c_R = Control.make ("R", Control.Active, 0, 0);
val signt = foldr (fn ((id, ctrl), signt) =>
                      Util.StringMap.add (id, ctrl, signt))
                  Util.StringMap.empty
                  [("A", c_A), ("C", c_C), ("R", c_R)]


val x   = Name.make "x";
val y   = Name.make "y";
val z   = Name.make "z";
val v   = Name.make "v";
val e   = Name.make "e";
val y_0 = Name.make "y_0";
val y_1 = Name.make "y_1";
val v_0 = Node.make "v_0";
val v_1 = Node.make "v_1";


(********************************)
(*        Reaction Rules        *)
(********************************)
(*val connect_to_computer = "connect to computer" :::
   (-/y o A[x,y]) `|` (-/y o C[y])
  ----|>
   -/y o (A[x,y] `|` C[y]);*)
val connect_to_computer = "connect to computer" :::
   A["x","y","y","z"] o `[(*0*)]` `|` C["v","v"] o `[(*1*)]`
  ----|>
   A["x","z","y","y"] o `[(*0*)]` `|` C["z","v"] o `[(*1*)]`;
(* Pre-conditions:
 * x, y, z, v
 * v_0: A
 * v_0 -> 0
 * (v_0,0) -> x      <-- in effect this is not a constraint
 * (v_0,1) -> y
 * (v_0,2) -> y
 * (v_0,3) -> z
 * 0 -> v_0
 * v_1 : C
 * v_1 -> 0
 * (v_1,0) -> v
 * (v_1,1) -> v
 * 1 -> v_1
 * 
 * Changes:
 * con (v_0,1) -> z 
 * con (v_0,3) -> y
 * con (v_1,0) -> z
 *)
val conn_P
  = [(Presence (ERoot 0w0),         Present 0w2),
     (Presence (EName x),           Present 0w1),
     (Presence (EName y),           Present 0w2),
     (Presence (EName z),           Present 0w1),
     (Presence (EName v),           Present 0w2),
     (Presence (ENode v_0),         Present 0w1),
     (NodeControl v_0,              Control c_A),
     (ChildParent (CNode v_0),      Place (PRoot 0w0)),
     (PointLink (PPort (v_0, 0w0)), Link (LName x)),
     (PointLink (PPort (v_0, 0w1)), Link (LName y)),
     (PointLink (PPort (v_0, 0w2)), Link (LName y)),
     (PointLink (PPort (v_0, 0w3)), Link (LName z)),
     (ChildParent (CSite 0w0),      Place (PNode v_0)),
     (Presence (ENode v_1),         Present 0w1),
     (NodeControl v_1,              Control c_C),
     (ChildParent (CNode v_1),      Place (PRoot 0w0)),
     (PointLink (PPort (v_1, 0w0)), Link (LName v)),
     (PointLink (PPort (v_1, 0w1)), Link (LName v)),
     (ChildParent (CSite 0w1),      Place (PNode v_1))]
val conn_C
  = [Con ((v_0, 0w1), LName z),
     Con ((v_0, 0w3), LName y),
     Con ((v_1, 0w0), LName z)];
val conn_PCR = make_PCR connect_to_computer conn_P conn_C;


(* val disconnect_from_computer = "disconnect from computer" ::: *)
(*    -/y o (A[x,y] `|` C[y]) *)
(*   ----|> *)
(*    (-/y o A[x,y]) `|` (-/y o C[y]); *)
val disconnect_from_computer = "disconnect from computer" :::
   A["x","z","y","y"] o `[(*0*)]` `|` C["z","v"] o `[(*1*)]`
  ----|>
   A["x","y","y","z"] o `[(*0*)]` `|` C["v","v"] o `[(*1*)]`;
(* Pre-conditions:
 * x, y, z, v
 * v_0 : A
 * v_0 -> 0
 * (v_0,0) -> x
 * (v_0,1) -> z
 * (v_0,2) -> y
 * (v_0,3) -> y
 * 0 -> v_0
 * v_1 : C
 * v_1 -> 0
 * (v_1,0) -> z
 * (v_1,1) -> v
 * 1 -> v_1
 *
 * Changes:
 * con (v_0,1) -> y
 * con (v_0,3) -> z
 * con (v_1,0) -> v
 *)
val disc_P
  = [(Presence (ERoot 0w0), Present 0w2),
     (Presence (EName x), Present 0w1),
     (Presence (EName y), Present 0w2),
     (Presence (EName z), Present 0w2),
     (Presence (EName v), Present 0w1),
     (Presence (ENode v_0), Present 0w1),
     (NodeControl v_0, Control c_A),
     (ChildParent (CNode v_0), Place (PRoot 0w0)),
     (PointLink (PPort (v_0, 0w0)), Link (LName x)),
     (PointLink (PPort (v_0, 0w1)), Link (LName z)),
     (PointLink (PPort (v_0, 0w2)), Link (LName y)),
     (PointLink (PPort (v_0, 0w3)), Link (LName y)),
     (ChildParent (CSite 0w0), Place (PNode v_0)),
     (Presence (ENode v_1), Present 0w1),
     (NodeControl v_1, Control c_C),
     (ChildParent (CNode v_1), Place (PRoot 0w0)),
     (PointLink (PPort (v_1, 0w0)), Link (LName z)),
     (PointLink (PPort (v_1, 0w1)), Link (LName v)),
     (ChildParent (CSite 0w1), Place (PNode v_1))];
val disc_C
  = [Con ((v_0, 0w1), LName y),
     Con ((v_0, 0w3), LName z),
     Con ((v_1, 0w0), LName v)];
val disc_PCR = make_PCR disconnect_from_computer disc_P disc_C;


(* val enter_room = "enter room" ::: *)
(*    A[x,y] `|` R o `[(\*0*\)]` *)
(*   ----|> *)
(*    R o (A[x,y] `|` `[(\*0*\)]`); *)
val enter_room = "enter room" :::
   A["x","y","z","v"] o `[(*0*)]` `|` R o `[(*1*)]`
  ----|>
   R o (A["x","y","z","v"] o `[(*0*)]` `|` `[(*1*)]`);
(* Pre-conditions:
 * x, y, z, v
 * v_0 : A
 * v_0 -> 0
 * (v_0,0) -> x
 * (v_0,1) -> y
 * (v_0,2) -> z
 * (v_0,3) -> v
 * 0 -> v_0
 * v_1 : R
 * v_1 -> 0
 * 1 -> v_1
 *
 * Changes:
 * mov v_0 -> v_1
 *)
val entr_P
  = [(Presence (ERoot 0w0), Present 0w2),
     (Presence (EName x), Present 0w1),
     (Presence (EName y), Present 0w1),
     (Presence (EName z), Present 0w1),
     (Presence (EName v), Present 0w1),
     (Presence (ENode v_0), Present 0w1),
     (NodeControl v_0, Control c_A),
     (ChildParent (CNode v_0), Place (PRoot 0w0)),
     (PointLink (PPort (v_0, 0w0)), Link (LName x)),
     (PointLink (PPort (v_0, 0w1)), Link (LName y)),
     (PointLink (PPort (v_0, 0w2)), Link (LName z)),
     (PointLink (PPort (v_0, 0w3)), Link (LName v)),
     (ChildParent (CSite 0w0), Place (PNode v_0)),
     (Presence (ENode v_1), Present 0w1),
     (NodeControl v_1, Control c_R),
     (ChildParent (CNode v_1), Place (PRoot 0w0)),
     (ChildParent (CSite 0w1), Place (PNode v_1))];
val entr_C
  = [Mov (CNode v_0, PNode v_1)];
val entr_PCR = make_PCR enter_room entr_P entr_C;


(* val exit_room = "exit room" ::: *)
(*    R o (A[x,y] `|` `[(\*0*\)]`) *)
(*   ----|> *)
(*    A[x,y] `|` R o `[(\*0*\)]`; *)
val exit_room = "exit room" :::
   R o (A["x","y","z","v"] o `[(*0*)]` `|` `[(*1*)]`)
  ----|>
   A["x","y","z","v"] o `[(*0*)]` `|` R o `[(*1*)]`;
(* Pre-conditions:
 * x
 * y_0
 * v_0 : R
 * v_0 -> 0
 * v_1 : A
 * v_1 -> v_0
 * (v_1,0) -> x
 * (v_1,1) -> y
 * (v_1,2) -> z
 * (v_1,3) -> v
 * 0 -> v_1
 * 1 -> v_0
 *
 * Changes:
 * mov v_1 -> 0
 *)
val exit_P
  = [(Presence (ERoot 0w0), Present 0w1),
     (Presence (EName x), Present 0w1),
     (Presence (EName y), Present 0w1),
     (Presence (EName z), Present 0w1),
     (Presence (EName v), Present 0w1),
     (Presence (ENode v_0), Present 0w2),
     (NodeControl v_0, Control c_R),
     (ChildParent (CNode v_0), Place (PRoot 0w0)),
     (Presence (ENode v_1), Present 0w1),
     (NodeControl v_1, Control c_A),
     (ChildParent (CNode v_1), Place (PNode v_0)),
     (PointLink (PPort (v_1, 0w0)), Link (LName x)),
     (PointLink (PPort (v_1, 0w1)), Link (LName y)),
     (PointLink (PPort (v_1, 0w2)), Link (LName z)),
     (PointLink (PPort (v_1, 0w3)), Link (LName v)),
     (ChildParent (CSite 0w0), Place (PNode v_1)),
     (ChildParent (CSite 0w1), Place (PNode v_0))];
val exit_C
  = [Mov (CNode v_1, PRoot 0w0)];
val exit_PCR = make_PCR exit_room exit_P exit_C;


val PCR_rules = [conn_PCR, disc_PCR, entr_PCR, exit_PCR];

fun numberXstate2absbg (num, state) = (num, state2absbg state)

fun parse s = parsePepaBgStateStr signt "foo.state" s
    handle e => explain e;
val parse' = numberXstate2absbg oo parse
fun parsel s = parsePepaBgStateListStr signt "foo.state" s
    handle e => explain e;
val parsel' = (map numberXstate2absbg) oo parsel

fun usefile file = PCR.usefile PCR.STATELIST signt file
    handle e => explain e;
val usefile' = (map numberXstate2absbg) oo usefile;

(********************************)
(*        Example Agents        *)
(********************************)
val simple_agent =     -//["y","z"] o A["x","y","y","z"] o <->
                   `|` R o -/"v" o C["v","v"] o <->

val simple_ss = BgAspectsSSGen.gen_state_space
                  {agent = simple_agent,
                   rules = PCR_rules};
(*                    rules = [entr_PCR]}; *)



(* val two_rooms = *)
(*     R o ((-/y o C[y]) `|` (-/y o C[y]) `|` (-/y o C[y])) *)
(* `|` R o ((-/y o C[y]) `|` (-/y o C[y]) `|` (-/y o C[y])) *)
(* `|` -/z *)
(*     o ((-/y o A[z,y]) `|` (-/y o A[z,y]) `|` (-/y o A[z,y])); *)
(* val two_rooms = *)
(*     R o ((-/"y" o C["y"] o <->) `|` (-/"y" o C["y"] o <->) `|` (-/"y" o C["y"] o <->)) *)
(* `|` R o ((-/"y" o C["y"] o <->) `|` (-/"y" o C["y"] o <->) `|` (-/"y" o C["y"] o <->)) *)
(* `|` -/"z" *)
(*     o ((-/"y" o A["z","y"] o <->) `|` (-/"y" o A["z","y"] o <->) `|` (-/"y" o A["z","y"] o <->)); *)
