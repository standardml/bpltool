(* Mobile phones example from Robin Milner's
 * pi-book.
 *)

OS.FileSys.chDir "..";
use "smlnj.sml";
Flags.setIntFlag "/debug/level" 10;
val id_0 = idp(0)
val id_1 = idp(1)

(* Pi calculus reaction rule controls *)
val Sum  = passive0 ("Sum")
val Send0 = passive ("Send0" -: 1)
val Get0  = passive ("Get0"  -: 1)
val Send1 = passive ("Send1" -: 2)
val Get1  = passive ("Get1"  =: 1 --> 1)
val Send2 = passive ("Send2" -: 3)
val Get2  = passive ("Get2"  =: 2 --> 1)

(* Pi calculus reaction rule link names *)
val ( x,  y,  y1,  y2,  z,  z1,  z2)
  = ("x","y","y1","y2","z","z1","z2")

(* Polyadic (up to 2) pi calculus reaction rules *)
val REACT0 =
  {redex = (x/x * Sum) o (Send0[x] `|` idp(1))
       `|` (x/x * Sum) o (Get0[x]  `|` idp(1)),
   react = x//[] * merge(2),
   inst  = [0 |-> 0, 1 |-> 2]}
val REACT1 =
  {redex = (idw[x,y] * Sum) o (Send1[x,y]   `|` idp(1))
            `|` (x/x * Sum) o (Get1[x][[z]] `|` idp(1)),
   react = (x//[] * y/z * idp(1)) o (idp(1) `|` `[z]`),
   inst  = [0 |-> 0, 1 |-> 2]}
val REACT2 =
  {redex = (idw[x,y1,y2] * Sum) o (Send2[x,y1,y2]   `|` idp(1))
                `|` (x/x * Sum) o (Get2[x][[z1],[z2]] `|` idp(1)),
   react = (x//[] * y1/z1 * y2/z2 * idp(1)) o (idp(1) `|` `[z1,z2]`),
   inst  = [0 |-> 0, 1 |-> 2]}
val rules = [REACT0, REACT1, REACT2]

(* System controls *)
val Car   = passive ("Car" -: 2)
val Trans = passive ("Trans" -: 4)
val IdTrans = passive ("IdTrans" -: 2)
val Control1 = passive0 ("Control1")
val Control2 = passive0 ("Control2")

(* System link names *)
val ( talk1,  talk2,  switch1,  switch2,  t,  s,  gain1,  lose1,  gain2,  lose2)
  = ("talk1","talk2","switch1","switch2","t","s","gain1","lose1","gain2","lose2")

(* System *)
val agent = 
    (idw[talk1,switch1] * Sum)
    o ((Send0[talk1] || idw[talk1,switch1]) o Car[talk1,switch1] o <->
       `|` Get2[switch1][[t],[s]] o (<[t,s]> Car[t,s] o <->))
`|` (idw[talk1,switch1,gain1,lose1] * Sum)
    o ((Get0[talk1] || idw[talk1,switch1,gain1,lose1])
       o Trans[talk1,switch1,gain1,lose1] o <->
       `|` (Get2[lose1][[t],[s]] || idw[switch1,gain1,lose1])
           o (<[t,s]> (Send2[switch1,t,s] * idw[gain1,lose1]) o IdTrans[gain1,lose1] o <->))
`|` (Get2[gain2][[t],[s]] || idw[gain2,lose2]) o (<[t,s]> Trans[t,s,gain2,lose2] o <->)
`|` (idw[lose1,talk2,switch2,gain2] * Sum)
    o (Send2[lose1,talk2,switch2] || idw[gain2,talk2,switch2])
    o Send2[gain2,talk2,switch2] o Control2 o <->

  