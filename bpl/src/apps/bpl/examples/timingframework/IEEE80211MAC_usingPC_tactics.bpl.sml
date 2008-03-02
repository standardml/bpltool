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
 
(* Model for IEEE 802.11 MAC 4-way handshake, using program counter framework
   for timing.
   This version tries using different tactics, to improve the runspeed
   of the program counter framework
 *)

use "programcounter.bpl.sml";
Flags.setIntFlag "/debug/level" 10;
SMLofNJ.Internals.GC.messages false;
use_shorthands true;

(* Components controls *)
val Sender     = active ("Sender" -: 2)   (* wishes to send data *)
val BackingOff = active0 ("BackingOff")   (* backoff queue in sender *)
val WaitForFree= active0 ("WaitForFree")  (* freechannel queue in sender *)
val Timers     = active0 ("Timers")       (* TO timers in sender *)
val TimeOut    = active ("TimeOut" -: 1)  (* TO timer for a packet *)
val Receiver   = active ("Receiver" -: 2) (* able to receive data *)
val Medium     = active ("Medium" -: 1)   (* medium which packets travel *)
val Packet     = active ("Packet" -: 3)   (* sender, receiver, timer *)
val CollisionPackets = active0 ("CollisionPackets")
val RTS        = atomic0 ("RTS")
val CTS        = atomic0 ("CTS")
val Data       = atomic0 ("Data")
val ACK        = atomic0 ("ACK")
val Collided   = atomic0 ("Collided")


(* System link names *)
val ( pclink,  mediumlink,  inter1,  sender1link,  receiver1link,
      timeoutlink1,  timeoutlink2,  sender2link,  eventlink )
  = ("pclink","mediumlink","inter1","sender1link","receiver1link",
     "timeoutlink1","timeoutlink2","sender2link","eventlink")


(* System *)
val System1 = simplify (
   (-/mediumlink * -/pclink * -/sender1link * -/sender2link * -/receiver1link * 
    -/timeoutlink1 * -/timeoutlink2) o 
   (   Sender[sender1link,mediumlink] o merge(4) o
         (BackingOff o <-> * WaitForFree o <-> * Timers o <-> *
          Packet[sender1link,receiver1link,timeoutlink1] o merge(2) o
            (RTS * PCNewEvent[pclink] o <->)
         ) `|`
       Sender[sender2link,mediumlink] o merge(4) o
         (BackingOff o <-> * WaitForFree o <-> * Timers o <-> *
          Packet[sender2link,receiver1link,timeoutlink2] o merge(2) o
            (RTS * PCNewEvent[pclink] o <->)
         ) `|`
       Receiver[receiver1link, mediumlink] o <-> `|`
       PCMain[pclink] o <-> `|` 
       Medium[mediumlink] o  <->  `|`
       CollisionPackets o <->
   )
) handle e => explain e;

(* Component definition rules *)
val ( send,  receive,  medium,  timer,  pcmainlink,  
      x1,  x2,  y1,  y2,  z1,  z2,
      send2,  receive2,  medium2,  timer2 )
  = ("send","receive","medium","timer","pcmainlink",
     "x1","x2","y1","y2","z1","z2",
     "send2","receive2","medium2","timer2")

(* Sender has waited VULN and sends package *)
val SENDPACKET = "SENDPACKET" ::: 
  (Sender[send,medium] o merge(3) o
     (Packet[send,receive,timer] o merge(2) o
        (idp(1) * PCRunNow[pcmainlink]) *
      Timers o idp(1) *
      idp(1)) ||
   Medium[medium] o idp(1))
  --[2 |-> 0, 0 |-> 1, 1 |-> 2, 3 |-> 3]--|>
  (Sender[send,medium] o merge(2) o
     (Timers o merge(2) o
        (TimeOut[timer] o PCNewEvent[pcmainlink] o merge(8) o
           (PCDot * PCDot * PCDot * PCDot * PCDot * PCDot * PCDot * PCDot)*
                                                          (*wait TimeOut*)
         idp(1)) *
      idp(1)) ||
   Medium[medium] o merge(2) o 
     (Packet[send,receive,timer] o merge(2) o
        (idp(1) * PCNewEvent[pcmainlink] o merge(2) o (PCDot * PCDot)) *
                                         (*time to send RTS and Data*)
      idp(1))
  ) handle e => explain e;

(* Receiver has waited SIFS and sends reply *)
val REPLYPACKET = "REPLYPACKET" ::: 
  (Receiver[receive,medium] o merge(2) o
     (Packet[send,receive,timer] o merge(2) o
        (idp(1) * PCRunNow[pcmainlink]) *
      idp(1)) ||
   Medium[medium] o idp(1))
  --[0 |-> 1, 1 |-> 0, 2 |-> 2]--|>
  (Receiver[receive,medium] o idp(1) ||
   Medium[medium] o merge(2) o
     (Packet[send,receive,timer] o merge(2) o
        (idp(1) * PCNewEvent[pcmainlink] o merge(2) o (PCDot * PCDot)) * 
                                          (*time to send CTS and ACK*)
      idp(1))
  ) handle e => explain e;

(* RTS package reach receiver without collisions *)
val RTSRECEIVESUCCESS = "RTSRECEIVESUCCESS" ::: 
  (Medium[medium] o 
     (Packet[send,receive,timer] o merge(2) o
        (RTS * PCRunNow[pcmainlink])) ||
   Receiver[receive,medium] o idp(1))
  --[0 |-> 0]--|>
  (Medium[medium] o <-> ||
   Receiver[receive,medium] o merge(2) o
     (Packet[send,receive,timer] o merge(2) o
        (CTS * PCNewEvent[pcmainlink] o PCDot) * (*wait SIFS*)
      idp(1))
  ) handle e => explain e;

(* CTS package reach sender without collisions *)
val CTSRECEIVESUCCESS = "CTSRECEIVESUCCESS" ::: 
  (-/timer) o
  (Sender[send,medium] o merge(2) o
     (idp(1) *
      Timers o merge(2) o
        (idp(1) *
         TimeOut[timer] o PCEvent[eventlink] )
     ) ||
   Medium[medium] o 
     (Packet[send,receive,timer] o merge(2) o
        (CTS * PCRunNow[pcmainlink])))
  --[0 |-> 0, 1 |-> 1]--|>
  (-/timer * eventlink//[]) o
  (Sender[send,medium] o merge(3) o
     (idp(1) *
      Timers o idp(1) *
      Packet[send,receive,timer] o merge(2) o
        (Data * PCNewEvent[pcmainlink] o PCDot) (*wait SIFS*)
     ) ||
   Medium[medium] o <->
  ) handle e => explain e;

(* Data package reach receiver without collisions *)
val DATARECEIVESUCCESS = "DATARECEIVESUCCESS" ::: 
  (Medium[medium] o 
     (Packet[send,receive,timer] o merge(2) o
        (Data * PCRunNow[pcmainlink])) ||
   Receiver[receive,medium] o idp(1))
  --[0 |-> 0]--|>
  (Medium[medium] o <-> ||
   Receiver[receive,medium] o merge(2) o
     (Packet[send,receive,timer] o merge(2) o
        (ACK * PCNewEvent[pcmainlink] o PCDot) *  (*wait SIFS*)
      idp(1))
  ) handle e => explain e;

(* ACK package reach sender without collisions *)
val ACKRECEIVESUCCESS = "ACKRECEIVESUCCESS" ::: 
  (-/timer) o
  (Sender[send,medium] o merge(2) o
     (idp(1) *
      Timers o merge(2) o
        (idp(1) *
         TimeOut[timer] o PCEvent[eventlink] )
     ) ||
   Medium[medium] o 
     (Packet[send,receive,timer] o merge(2) o
        (ACK * PCRunNow[pcmainlink])))
  --[0 |-> 0, 1 |-> 1]--|>
  (eventlink//[] * receive//[] * pcmainlink//[]) o
  (Sender[send,medium] o merge(2) o
     (idp(1) *
      Timers o idp(1)
     ) ||
   Medium[medium] o <->
  ) handle e => explain e;

(* A packet is fully sent and reached recipient, but someone else is
 * also sending = collsion
 * Packet is moved to CollidedPackages waiting for timeout, and the
 * other packet is marked Collided, for same treatment when done. *)
val COLLISIONTAILSCRAMBLED = "COLLISIONTAILSCRAMBLED" ::: 
   Medium[medium] o merge(3) o
     (idp(1) *
      Packet[send,receive,timer] o merge(2) o
        (idp(1) * PCRunNow[pcmainlink]) *
      Packet[send2,receive2,timer2] o idp(1)
     ) *
  CollisionPackets o idp(1)
  --[0 |-> 0, 1 |-> 2, 2 |-> 3, 3 |-> 1]--|>
  (pcmainlink//[]) o
  (Medium[medium] o merge(2) o
     (idp(1) *
      Packet[send2,receive2,timer2] o merge(2) o
        (idp(1) * Collided)
     ) *
   CollisionPackets o merge(2) o
     (idp(1) *
      Packet[send,receive,timer] o idp(1))
  ) handle e => explain e;

(* A packet was sent while channel was busy, but channel clear when done.
 * The packet is not received, but sender will not know this and must
 * wait for timeout.
 * Packet is moved to CollidedPackages waiting for timeout. *)
val COLLISIONHEADSCRAMBLED = "COLLISIONHEADSCRAMBLED" ::: 
   Medium[medium] o merge(2) o
     (idp(1) *
      Packet[send,receive,timer] o merge(3) o
        (idp(1) * PCRunNow[pcmainlink] * Collided)
     ) *
  CollisionPackets o idp(1)
  --[0 |-> 0, 1 |-> 2, 2 |-> 1]--|>
  (pcmainlink//[]) o
  (Medium[medium] o idp(1) *
   CollisionPackets o merge(2) o
     (idp(1) *
      Packet[send,receive,timer] o idp(1))
  ) handle e => explain e;

(* A packet with Data is fully sent and reached recipient, but someone
 * else is also sending = collsion.
 * Sender will detect this and queue the message for retransmission without
 * having to wait for timeout.
 * Packet is moved to WaitForFree waiting for free medium, and the
 * other packet is marked Collided. *)
val DATARECEIVEFAILURE = "DATARECEIVEFAILURE" ::: 
  (Sender[send,medium] o merge(3) o
     (idp(1) *
      WaitForFree o idp(1) *
      Timers o merge(2) o
        (idp(1) *
         TimeOut[timer] o PCEvent[eventlink]))) ||
   Medium[medium] o merge(3) o
     (idp(1) *
      Packet[send,receive,timer] o merge(3) o
        (Data * PCRunNow[pcmainlink] * idp(1)) *
      Packet[send2,receive2,timer2] o idp(1)
     )
  --[0 |-> 0, 1 |-> 1, 2 |-> 2, 3 |-> 3, 4 |-> 5]--|> (*remove redex site4*)
  (eventlink//[]) o
  (Sender[send,medium] o merge(3) o
     (idp(1) *
      WaitForFree o merge(2) o
        (idp(1) *
         Packet[send,receive,timer] o merge(5) o
           (PCDot * PCDot * PCDot * PCDot * (*backoff counter 4*)
            PCNewEvent[pcmainlink] o PCDot)) *
      Timers o idp(1))) ||
   Medium[medium] o merge(2) o
     (idp(1) *
      Packet[send2,receive2,timer2] o merge(2) o
        (idp(1) * Collided)
   ) handle e => explain e;

(* A packets timeout has been reached, the sender will put it in queue for
 * resending (new connection with RTS, CTS and all).
 * Packet is moved to WaitForFree waiting for free medium. 
 * BUG: This may remove packet from medium without notifying other packets
 * that they are collided... not intended! *)
val PACKETTIMEOUT = "PACKETTIMEOUT" ::: 
  (Sender[send,medium] o merge(3) o
     (idp(1) *
      WaitForFree o idp(1) *
      Timers o merge(2) o
        (idp(1) *
         TimeOut[timer] o PCRunNow[pcmainlink]))) ||
   Packet[send,receive,timer] o idp(1)
  --[0 |-> 0, 1 |-> 1, 2 |-> 2]--|> (* remove redex site 3 *)
  (Sender[send,medium] o merge(3) o
     (idp(1) *
      WaitForFree o merge(2) o
        (idp(1) *
         Packet[send,receive,timer] o merge(1) o
           (                                     (*backoff counter 0!!!*)
            PCNewEvent[pcmainlink] o PCDot)) *
      Timers o idp(1)) *
   <->
   ) handle e => explain e;

val PACKETTIMEOUT2 = "PACKETTIMEOUT2" ::: 
  (Sender[send,medium] o merge(3) o
     (idp(1) *
      WaitForFree o idp(1) *
      Timers o merge(2) o
        (idp(1) *
         TimeOut[timer] o PCRunNow[pcmainlink]))) ||
   Packet[send,receive,timer] o idp(1)
  --[0 |-> 0, 1 |-> 1, 2 |-> 2]--|> (* remove redex site 3 *)
  (Sender[send,medium] o merge(3) o
     (idp(1) *
      WaitForFree o merge(2) o
        (idp(1) *
         Packet[send,receive,timer] o merge(3) o
           (PCDot * PCDot *                           (*backoff counter 2*)
            PCNewEvent[pcmainlink] o PCDot)) *
      Timers o idp(1)) *
   <->
   ) handle e => explain e;

(* A packet waiting for the channel to become free, has waited enough
 * (the medium is now free)
 * Move packet (back) to the backingOff Queue *)
val WAITFORFREESUCCESS = "WAITFORFREESUCCESS" ::: 
  (Sender[send,medium] o merge(3) o
     (idp(1) *
      WaitForFree o  merge(2) o
        (idp(1) *
         Packet[send,receive,timer] o merge(2) o
          (PCRunNow[pcmainlink] * idp(1))) *
      BackingOff o idp(1)) ||
   Medium[medium] o <->)
  --[0 |-> 0, 1 |-> 1, 2 |-> 3, 3 |-> 2]--|>
  (Sender[send,medium] o merge(3) o
     (idp(1) *
      WaitForFree o idp(1) *
      BackingOff o merge(2) o
        (idp(1) *
	 Packet[send,receive,timer] o merge(2) o
          (idp(1) * PCNewEvent[pcmainlink] o PCDot))) || (*wait DIFS*)
   Medium[medium] o <->
   ) handle e => explain e;

(* A packet waiting for the channel to become free, has not waited enough
 * (the medium is still busy)
 * Make new event for the packet to check medium again in next timeframe. *)
val WAITFORFREEFAILURE = "WAITFORFREEFAILURE" ::: 
  (Sender[send,medium] o merge(2) o
     (idp(1) *
      WaitForFree o  merge(2) o
        (idp(1) *
         Packet[send,receive,timer] o merge(2) o
          (PCRunNow[pcmainlink] * idp(1)))) ||
   Medium[medium] o merge(2) o
     (Packet[send2,receive2,timer2] o idp(1) * idp(1)))
  --[0 |-> 0, 1 |-> 1, 2 |-> 2, 3 |-> 3, 4 |-> 4]--|>
  (Sender[send,medium] o merge(2) o
     (idp(1) *
      WaitForFree o  merge(2) o
        (idp(1) *
         Packet[send,receive,timer] o merge(2) o
          (PCNewEvent[pcmainlink] o PCDot * idp(1)))) ||
   Medium[medium] o merge(2) o
     (Packet[send2,receive2,timer2] o idp(1) * idp(1))
   ) handle e => explain e;

(* A packet on backoff tests channel, and it is in fact free.
 * Decrease backoff counter and create new event for next test. *)
val BACKOFFSTILLFREE = "BACKOFFSTILLFREE" ::: 
  (Sender[send,medium] o merge(2) o
     (idp(1) *
      BackingOff o  merge(2) o
        (idp(1) *
         Packet[send,receive,timer] o merge(3) o
          (PCRunNow[pcmainlink] * PCDot * idp(1)))) ||
   Medium[medium] o <->)
  --[0 |-> 0, 1 |-> 1, 2 |-> 2]--|>
  (Sender[send,medium] o merge(2) o
     (idp(1) *
      BackingOff o  merge(2) o
        (idp(1) *
         Packet[send,receive,timer] o merge(2) o
          (PCNewEvent[pcmainlink] o PCDot * (*wait ASLOTTIME*)
           idp(1)))) ||
   Medium[medium] o <->
   ) handle e => explain e;

(* A packet on backoff tests channel, and it is NOT free.
 * Move packet to WaitForFree, and create event for next timeframe. *)
val BACKOFFCHANNELBUSY = "BACKOFFCHANNELBUSY" ::: 
  (Sender[send,medium] o merge(3) o
     (idp(1) *
      BackingOff o merge(2) o
        (idp(1) *
         Packet[send,receive,timer] o merge(2) o
          (PCRunNow[pcmainlink] * idp(1))) *
      WaitForFree o idp(1)) ||
   Medium[medium] o merge(2) o
     (Packet[send2,receive2,timer2] o idp(1) * idp(1)))
  --[0 |-> 0, 1 |-> 1, 2 |-> 3, 3 |-> 2, 4 |-> 4, 5 |-> 5]--|>
  (Sender[send,medium] o merge(3) o
     (idp(1) *
      BackingOff o idp(1) *
      WaitForFree o  merge(2) o
        (idp(1) *
         Packet[send,receive,timer] o merge(2) o
          (PCNewEvent[pcmainlink] o PCDot * idp(1)))) ||
   Medium[medium] o merge(2) o
     (Packet[send2,receive2,timer2] o idp(1) * idp(1))
   ) handle e => explain e;

(* A packet on backoff has backed enough off, and channel is free.
 * Put RTS packet in queue for transmission. *)
val BACKOFFCOMPLETEANDCHANNELFREE = "BACKOFFCOMPLETEANDCHANNELFREE" ::: 
  (Sender[send,medium] o merge(2) o
     (idp(1) *
      BackingOff o  merge(2) o
        (idp(1) *
         Packet[send,receive,timer] o PCRunNow[pcmainlink]))  ||
   Medium[medium] o <->)
  --[0 |-> 0, 1 |-> 1]--|>
  (Sender[send,medium] o merge(3) o
     (idp(1) *
      BackingOff o idp(1) *
      Packet[send,receive,timer] o merge(2) o
       (RTS * PCNewEvent[pcmainlink] o PCDot)) || (*wait VULN*)
   Medium[medium] o <->
   ) handle e => explain e;

(*Ruleset 1, timeout backoff time = 0 *)
val defrules = [SENDPACKET,REPLYPACKET,RTSRECEIVESUCCESS,CTSRECEIVESUCCESS,
                DATARECEIVESUCCESS,ACKRECEIVESUCCESS,
                COLLISIONTAILSCRAMBLED,COLLISIONHEADSCRAMBLED,
                DATARECEIVEFAILURE, PACKETTIMEOUT, WAITFORFREESUCCESS,
                WAITFORFREEFAILURE, BACKOFFSTILLFREE,
                BACKOFFCHANNELBUSY,BACKOFFCOMPLETEANDCHANNELFREE]
(*Ruleset 2, timeout backoff time = 2 *)
val defrules2= [SENDPACKET,REPLYPACKET,RTSRECEIVESUCCESS,CTSRECEIVESUCCESS,
                DATARECEIVESUCCESS,ACKRECEIVESUCCESS,
                COLLISIONTAILSCRAMBLED,COLLISIONHEADSCRAMBLED,
                DATARECEIVEFAILURE, PACKETTIMEOUT2, WAITFORFREESUCCESS,
                WAITFORFREEFAILURE, BACKOFFSTILLFREE,
                BACKOFFCHANNELBUSY,BACKOFFCOMPLETEANDCHANNELFREE]

(* All rules *)
val rules = mkrules (List.@ (defrules, pcrules));
val pcrulesonly = mkrules (pcrules);
val defrulesonly= mkrules (defrules);
val defrulesonly2= mkrules (defrules2);

val TAC_anyrule = react_rule_any;
val TAC_roundrobin = roundrobin;



val totalstarttime = Time.toReal(Time.now());
val timeused = 0.0;









(* First improved tactic, simplest *)
val PCTAC_smart2  =(REPEAT (react_rule "PC_NEWEVENT")) ++
                   (REPEAT (react_rule "PC_NEWEVENTLOOP")) ++
                   (REPEAT (react_rule "PC_NEWEVENTLOOPADD")) ++
                   (REPEAT (react_rule "PC_NEWEVENTLOOP")) ++
                   (REPEAT (react_rule "PC_NEWEVENTLOOPADD")) ++
                   (REPEAT (react_rule "PC_NEWEVENTLOOPSTOP")) ++
                   (REPEAT (react_rule "PC_CANCELDEADEVENT")) ++
                   (REPEAT (react_rule "PC_NEXTEVENTSLOT")) ++
                   (react_rule "PC_ACTIVATEEVENT");

(* Second improved tactic, advanced, running each step for all events *)
val PCTAC_smart3 = (REPEAT (react_rule "PC_NEWEVENT")) ++
                   (REPEAT (IF   (react_rule "PC_NEWEVENTLOOP")
                            THEN finish
                            ELSE (IF   (react_rule "PC_NEWEVENTLOOPADD")
                                  THEN (REPEAT (react_rule "PC_NEWEVENTLOOPADD")
                                        ++
                                        finish)
                                  ELSE fail)
                           )
                   ) ++
                   (REPEAT (react_rule "PC_NEWEVENTLOOPSTOP")) ++
                   (REPEAT (react_rule "PC_CANCELDEADEVENT")) ++
                   (REPEAT (react_rule "PC_NEXTEVENTSLOT")) ++
                   (react_rule "PC_ACTIVATEEVENT");

(* Third improved tactic, advanced, running each event through all steps *)
val PCTAC_smart4 = 
  (REPEAT (IF   (react_rule "PC_NEWEVENT")
           THEN ((REPEAT (react_rule "PC_NEWEVENTLOOP")) ++
                 (REPEAT (react_rule "PC_NEWEVENTLOOPADD")) ++
                 (react_rule "PC_NEWEVENTLOOPSTOP") ++
                 finish)
           ELSE fail)) ++
  (REPEAT (react_rule "PC_CANCELDEADEVENT")) ++
  (REPEAT (react_rule "PC_NEXTEVENTSLOT")) ++
  (react_rule "PC_ACTIVATEEVENT");

(* CHOSE TACTIC HERE *)
(*fun smarttactics bg =(run pcrulesonly TAC_roundrobin bg);*)
(*fun smarttactics bg =(run pcrulesonly PCTAC_smart2 bg);*)
(*fun smarttactics bg =(run pcrulesonly PCTAC_smart3 bg);*)
fun smarttactics bg =(run pcrulesonly PCTAC_smart4 bg);

val totalstarttime = Time.toReal(Time.now());
val timeused = 0.0;

val starttime = Time.toReal(Time.now());
val System1_ready1 = run pcrulesonly PCTAC_init System1;
val System1_ready1 = smarttactics(System1_ready1);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react1 = run defrulesonly TAC_anyrule System1_ready1;
val starttime = Time.toReal(Time.now());
val System1_ready2 = smarttactics(System1_react1);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react2 = run defrulesonly TAC_anyrule System1_ready2;
val starttime = Time.toReal(Time.now());
val System1_ready3 = smarttactics(System1_react2);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react3 = run defrulesonly TAC_anyrule System1_ready3;
val starttime = Time.toReal(Time.now());
val System1_ready4 = smarttactics(System1_react3);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react4 = run defrulesonly TAC_anyrule System1_ready4;
val starttime = Time.toReal(Time.now());
val System1_ready5 = smarttactics(System1_react4);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react5 = run defrulesonly TAC_anyrule System1_ready5;
val starttime = Time.toReal(Time.now());
val System1_ready6 = smarttactics(System1_react5);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
(* Using rulesset2, to model different backoff timers *)
val System1_react6 = run defrulesonly2 TAC_anyrule System1_ready6;
val starttime = Time.toReal(Time.now());
val System1_ready7 = smarttactics(System1_react6);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react7 = run defrulesonly TAC_anyrule System1_ready7;
val starttime = Time.toReal(Time.now());
val System1_ready8 = smarttactics(System1_react7);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react8 = run defrulesonly TAC_anyrule System1_ready8;
val starttime = Time.toReal(Time.now());
val System1_ready9 = smarttactics(System1_react8);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react9 = run defrulesonly TAC_anyrule System1_ready9;
val starttime = Time.toReal(Time.now());
val System1_ready10 = smarttactics(System1_react9);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react10 = run defrulesonly TAC_anyrule System1_ready10;
val starttime = Time.toReal(Time.now());
val System1_ready11 = smarttactics(System1_react10);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react11 = run defrulesonly TAC_anyrule System1_ready11;
val starttime = Time.toReal(Time.now());
val System1_ready12 = smarttactics(System1_react11);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react12 = run defrulesonly TAC_anyrule System1_ready12;
val starttime = Time.toReal(Time.now());
val System1_ready13 = smarttactics(System1_react12);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react13 = run defrulesonly TAC_anyrule System1_ready13;
val starttime = Time.toReal(Time.now());
val System1_ready14 = smarttactics(System1_react13);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react14 = run defrulesonly TAC_anyrule System1_ready14;
val starttime = Time.toReal(Time.now());
val System1_ready15 = smarttactics(System1_react14);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react15 = run defrulesonly TAC_anyrule System1_ready15;
val starttime = Time.toReal(Time.now());
val System1_ready16 = smarttactics(System1_react15);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react16 = run defrulesonly TAC_anyrule System1_ready16;
val starttime = Time.toReal(Time.now());
val System1_ready17 = smarttactics(System1_react16);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react17 = run defrulesonly TAC_anyrule System1_ready17;
val starttime = Time.toReal(Time.now());
val System1_ready18 = smarttactics(System1_react17);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react18 = run defrulesonly TAC_anyrule System1_ready18;
val starttime = Time.toReal(Time.now());
val System1_ready19 = smarttactics(System1_react18);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react19 = run defrulesonly TAC_anyrule System1_ready19;
val starttime = Time.toReal(Time.now());
val System1_ready20 = smarttactics(System1_react19);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react20 = run defrulesonly TAC_anyrule System1_ready20;
val starttime = Time.toReal(Time.now());
val System1_ready21 = smarttactics(System1_react20);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react21 = run defrulesonly TAC_anyrule System1_ready21;
val starttime = Time.toReal(Time.now());
val System1_ready22 = smarttactics(System1_react21);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react22 = run defrulesonly TAC_anyrule System1_ready22;
val starttime = Time.toReal(Time.now());
val System1_ready23 = smarttactics(System1_react22);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react23 = run defrulesonly TAC_anyrule System1_ready23;
val starttime = Time.toReal(Time.now());
val System1_ready24 = smarttactics(System1_react23);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react24 = run defrulesonly TAC_anyrule System1_ready24;
val starttime = Time.toReal(Time.now());
val System1_ready25 = smarttactics(System1_react24);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react25 = run defrulesonly TAC_anyrule System1_ready25;
val starttime = Time.toReal(Time.now());
val System1_ready26 = smarttactics(System1_react25);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react26 = run defrulesonly TAC_anyrule System1_ready26;
val starttime = Time.toReal(Time.now());
val System1_ready27 = smarttactics(System1_react26);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react27 = run defrulesonly TAC_anyrule System1_ready27;
val starttime = Time.toReal(Time.now());
val System1_ready28 = smarttactics(System1_react27);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react28 = run defrulesonly TAC_anyrule System1_ready28;
val starttime = Time.toReal(Time.now());
val System1_ready29 = smarttactics(System1_react28);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react29 = run defrulesonly TAC_anyrule System1_ready29;
val starttime = Time.toReal(Time.now());
val System1_ready30 = smarttactics(System1_react29);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react30 = run defrulesonly TAC_anyrule System1_ready30;
val starttime = Time.toReal(Time.now());
val System1_ready31 = smarttactics(System1_react30);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react31 = run defrulesonly TAC_anyrule System1_ready31;
val starttime = Time.toReal(Time.now());
val System1_ready32 = smarttactics(System1_react31);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react32 = run defrulesonly TAC_anyrule System1_ready32;
val starttime = Time.toReal(Time.now());
val System1_ready33 = smarttactics(System1_react32);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react33 = run defrulesonly TAC_anyrule System1_ready33;
val starttime = Time.toReal(Time.now());
val System1_ready34 = smarttactics(System1_react33);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react34 = run defrulesonly TAC_anyrule System1_ready34;
val starttime = Time.toReal(Time.now());
val System1_ready35 = smarttactics(System1_react34);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react35 = run defrulesonly TAC_anyrule System1_ready35;
val starttime = Time.toReal(Time.now());
val System1_ready36 = smarttactics(System1_react35);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react36 = run defrulesonly TAC_anyrule System1_ready36;
val starttime = Time.toReal(Time.now());
val System1_ready37 = smarttactics(System1_react36);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react37 = run defrulesonly TAC_anyrule System1_ready37;
val starttime = Time.toReal(Time.now());
val System1_ready38 = smarttactics(System1_react37);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react38 = run defrulesonly TAC_anyrule System1_ready38;
val starttime = Time.toReal(Time.now());
val System1_ready39 = smarttactics(System1_react38);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react39 = run defrulesonly TAC_anyrule System1_ready39;
val starttime = Time.toReal(Time.now());
val System1_ready40 = smarttactics(System1_react39);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);
val System1_react40 = run defrulesonly TAC_anyrule System1_ready40;
val starttime = Time.toReal(Time.now());
val System1_ready41 = smarttactics(System1_react40);
val timeused = timeused + (Time.toReal(Time.now()) - starttime);

val totaltimeused = Time.toReal(Time.now()) - totalstarttime;

(* RUNNINGTIMES FOUND USING THIS TACTIC ON ESKES LAPTOP (tm) *)

(* RoundRobin tactic *)
(* RoundRobin: PCruntime 2854, totalruntime: 5856 seconds *)
(* RoundRobin: PCruntime 2849, totalruntime: 5844 seconds *)

(* Tactic2 *)
(* Tactic2: PCruntime 771, totalruntime: 3554 seconds *)
(* Tactic2: PCruntime 767, totalruntime: 3535 seconds *)
(* Tactic2: PCruntime 770, totalruntime: 3552 seconds *)

(* Tactic3 *)
(* Tactic3: PCruntime 768, totalruntime: 3558 seconds *)
(* Tactic3: PCruntime 768, totalruntime: 3555 seconds *)
(* Tactic3: PCruntime 768, totalruntime: 3550 seconds *)


(* Tactic4 *)
(* Tactic4: PCruntime 753, totalruntime: 3534 seconds *)
(* Tactic4: PCruntime 756, totalruntime: 3543 seconds *)

