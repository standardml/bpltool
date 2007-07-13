(* Copyright (c) 2007  The BPL Group at the IT University of Copenhagen
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
 
 (* Mobile phones example from Robin Milner:
 * "Communicating and Mobile Systems: the Pi Calculus,"
 * Cambridge University Press, 1999.
 *)

use "pi.bpl.sml";
Flags.setIntFlag "/debug/level" 10;

(* Pi calculus reaction rule controls for communicating 0 or 2 names. *)
val Send0 = Send 0
val Get0  = Get 0
val Send2 = Send 2
val Get2  = Get 2

(* Pi calculus reaction rules *)
val REACT0 = REACT 0
val REACT2 = REACT 2
val rules = [REACT0, REACT2]

(* Components controls *)
val Car      = atomic ("Car" -: 2)
val Trans    = atomic ("Trans" -: 4)
val Idtrans  = atomic ("Idtrans" -: 2)
val Control  = atomic ("Control" -: 8)

(* System link names *)
val ( talk1,  talk2,  switch1,  switch2,  t,  s,  gain1,  lose1,  gain2,  lose2 )
  = ("talk1","talk2","switch1","switch2","t","s","gain1","lose1","gain2","lose2")

(* Component definition rules *)
val ( talk,  switch,  gain,  lose )
  = ("talk","switch","gain","lose")
val DEF_Car =
  {name  = "def_Car", 
   redex = Car[talk,switch],
   react = Sum o (Send0[talk] o Car[talk,switch]
                  `|` Get2[switch][[t],[s]] o (<[t,s]> Car[t,s])),
   inst  = @[]}
val DEF_Trans =
  {name  = "def_Trans",
   redex = Trans[talk,switch,gain,lose],
   react = Sum o (Get0[talk][] o Trans[talk,switch,gain,lose]
                  `|` Get2[lose][[t],[s]]
                      o (<[t,s]> Send2[switch,t,s] o Idtrans[gain,lose])),
   inst  = @[]}
val DEF_Idtrans =
  {name  = "def_Idtrans",
   redex = Idtrans[gain, lose],
   react = Get2[gain][[t],[s]] o (<[t,s]> Trans[t,s,gain,lose]),
   inst  = @[]}
val DEF_Control =
  {name  = "def_Control",
   redex = Control[lose1,talk2,switch2,gain2,lose2,talk1,switch1,gain1],
   react = Send2[lose1,talk2,switch2] o Send2[gain2,talk2,switch2]
           o Control[lose2,talk1,switch1,gain1,lose1,talk2,switch2,gain2],
   inst  = @[]}

(* System *)
val System1 =
  -//[talk1,switch1,gain1,lose1,talk2,switch2,gain2,lose2]
  o (    Car[talk1,switch1] 
     `|` Trans[talk1,switch1,gain1,lose1]
     `|` Idtrans[gain2,lose2]
     `|` Control[lose1,talk2,switch2,gain2,lose2,talk1,switch1,gain1])handle e=>explain e
