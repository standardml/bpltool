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
 
(* A progam counter module for BPL Tool
 * To use create a PCMain-node in the system.
 * Require all rules to have PCRunNow node in them, and delete
 * this node in the reaction.
 * To insert an event put a PCNewEvent node in the place you want PCRunNow to
 * be inserted at the right time, and let PCNewEvent have as many PCDot children
 * as the should be cycles before PCRunNow is inserted. PCNewEvent must be
 * placed in an active context! PCNewEvent should link to PCMain's port.
 *
 * Inserting events into current eventslot (zero PCDot in PCNewEvent) should
 * be used with care - it models instant reactions, and could lock the system
 * in the current eventslot!
 *)

OS.FileSys.chDir "../..";
use "smlnj.sml";

(* Controls of the PC module *)
val PCMain          = active ("PCMain" -: 1)        (* Module container *)
val PCQueue         = active ("PCQueue" =: 1 --> 0) (* link to list end *)
val PCEventSlot     = active0 ("PCEventSlot")
val PCRunNow        = atomic ("PCRunNow" -: 1)
val PCNewEvent      = passive ("PCNewEvent" -: 1)
val PCEvent         = atomic ("PCEvent" -: 1)
val PCDot           = atomic0 ("PCDot")
val PCAddEvent      = passive0 ("PCAddEvent")
val PCNotCounted    = passive0 ("PCNotCounted")
val PCQueueEnd      = atomic ("PCQueueEnd" -: 1)


(* Reaction rule for the module *)
val ( pcmainlink,  pcqueuefirst,  pcqueuenext , pcdeadlink , pcneweventlink)
  = ("pcmainlink","pcqueuefirst","pcqueuenext","pcdeadlink","pcneweventlink")
val ( pceventlink,  pcqueueprev,  pcqueuenext2,  pcqueueendlink)
  = ("pceventlink","pcqueueprev","pcqueuenext2","pcqueueendlink")

(* Initialise the program counter framework, runs once only *)
val PC_INITUNFOLDPCMAIN = "PC_INITUNFOLDPCMAIN" ::: 
  PCMain[pcmainlink] o <->
  ----|>
  (PCMain[pcmainlink] o PCQueue[][[pcqueueendlink]] o  
     (<[pcqueueendlink]> PCEventSlot o PCQueueEnd[pcqueueendlink])
  ) handle e => explain e;

(* User want to add a new event, initialise insertion-iteration *)
val PC_NEWEVENT = "PC_NEWEVENT" ::: 
  PCNewEvent[pcmainlink] o idp(1) ||
  PCMain[pcmainlink] o 
  PCQueue[][[pcqueueendlink]] o 
    (<[pcqueueendlink]> PCEventSlot o `[pcqueueendlink]`)
  --[0 |-> 0, 1 |-> 1]--|>
  -/pceventlink o
  (PCEvent[pceventlink] ||
   PCMain[pcmainlink] o 
   PCQueue[][[pcqueueendlink]] o 
     (<[pcqueueendlink]> PCEventSlot o 
         ((PCNewEvent[pcmainlink] o merge(2) o 
             (idp(1) * PCEvent[pceventlink])) `|`
          `[pcqueueendlink]`)
     )
  ) handle e => explain e;

(* Insertion of new event has reached the correct eventslot, stop loop *)
val PC_NEWEVENTLOOPSTOP = "PC_NEWEVENTLOOPSTOP" ::: 
  <[pcqueueendlink]> PCEventSlot o merge(2) o
    (PCNewEvent[pcmainlink] o PCEvent[pceventlink] *
     `[pcqueueendlink]`)
  --[0 |-> 0]--|>
  pcmainlink//[] *
  (<[pcqueueendlink]> PCEventSlot o merge(2) o
    (PCEvent[pceventlink] *
     `[pcqueueendlink]`)
  ) handle e => explain e;

(* Insertion has not reached correct eventslot, move to next eventslot *)
val PC_NEWEVENTLOOP = "PC_NEWEVENTLOOP" ::: 
  <[pcqueueendlink]> PCEventSlot o merge(3) o
    (PCNewEvent[pcmainlink] o merge(2) o (PCDot * idp(1)) *
     idp(1) *
     PCEventSlot o `[pcqueueendlink]`)
  --[0 |-> 1, 1 |-> 0, 2 |-> 2]--|>
  <[pcqueueendlink]> PCEventSlot o merge(2) o
    (idp(1) *
     PCEventSlot o merge(2) o
       (PCNewEvent[pcmainlink] o idp(1) *
        `[pcqueueendlink]`)
    ) handle e => explain e;

(* Insertion has not reached correct eventslot, add next eventslot *)
val PC_NEWEVENTLOOPADD = "PC_NEWEVENTLOOPADD" ::: 
  <[pcqueueendlink]> PCEventSlot o merge(3) o
    (PCNewEvent[pcmainlink] o merge(2) o (PCDot * idp(1)) *
     idp(1) *
     PCQueueEnd[pcqueueendlink])
  --[0 |-> 1, 1 |-> 0]--|>
  <[pcqueueendlink]> PCEventSlot o merge(2) o
    (idp(1) *
     PCEventSlot o merge(2) o
       (PCNewEvent[pcmainlink] o idp(1) *
        PCQueueEnd[pcqueueendlink])
    ) handle e => explain e;

(* Time to find next event to rum but first eventslot empty, move forward
   to next eventslot *)
val PC_NEXTEVENTSLOT = "PC_NEXTEVENTSLOT" ::: 
  -/pcmainlink o
  PCMain[pcmainlink] o 
  PCQueue[][[pcqueueendlink]] o 
    (<[pcqueueendlink]> PCEventSlot o PCEventSlot o `[pcqueueendlink]`)
  --[0 |-> 0]--|>
  -/pcmainlink o
  PCMain[pcmainlink] o 
  PCQueue[][[pcqueueendlink]] o 
    (<[pcqueueendlink]> PCEventSlot o `[pcqueueendlink]`)
  handle e => explain e;

(* Active an event from current eventslot, but inserting PCRunNow in the agent*)
val PC_ACTIVATEEVENT = "PC_ACTIVATEEVENT" ::: 
  (-/pceventlink * -/pcmainlink) o
  (PCEvent[pceventlink] ||
   PCMain[pcmainlink] o 
   PCQueue[][[pcqueueendlink]] o 
     (<[pcqueueendlink]> PCEventSlot o merge(2) o 
       (PCEvent[pceventlink] * `[pcqueueendlink]`))
  )
  --[0 |-> 0]--|>
  (-/pcmainlink) o
  (PCRunNow[pcmainlink] ||
   PCMain[pcmainlink] o 
   PCQueue[][[pcqueueendlink]] o 
     (<[pcqueueendlink]> PCEventSlot o `[pcqueueendlink]`)
  ) handle e => explain e;

(* Remove dead events from the queue, to avoid lockups *)
val PC_CANCELDEADEVENT = "PC_CANCELDEADEVENT" ::: 
  (-/pceventlink) o
  (<[pcqueueendlink]> PCEventSlot o merge(2) o 
    (PCEvent[pceventlink] * `[pcqueueendlink]`)
  )
  --[0 |-> 0]--|>
  (<[pcqueueendlink]> PCEventSlot o 
    (`[pcqueueendlink]`)
  ) handle e => explain e;


(* Program Counter reaction rules *)
val pcrules = [PC_INITUNFOLDPCMAIN, PC_NEWEVENT, PC_NEWEVENTLOOPSTOP, 
               PC_NEWEVENTLOOP,PC_NEWEVENTLOOPADD,PC_NEXTEVENTSLOT,
               PC_ACTIVATEEVENT,PC_CANCELDEADEVENT];


val PCTAC_neweventloop = (REPEAT (react_rule "PC_NEWEVENTLOOP")) ++
                         (REPEAT (react_rule "PC_NEWEVENTLOOPADD"));
val PCTAC_runcounter = (REPEAT (react_rule "PC_CANCELDEADEVENT")) ++
                       (REPEAT (react_rule "PC_NEWEVENT")) ++
                       (REPEAT (PCTAC_neweventloop)) ++
                       (REPEAT (react_rule "PC_NEWEVENTLOOPSTOP")) ++
                       (TRY (react_rule "PC_ACTIVATEEVENT")
                         ORTHEN ((REPEAT (react_rule "PC_NEXTEVENTSLOT")) ++
                                 (react_rule "PC_ACTIVATEEVENT")));
val PCTAC_init = react_rule "PC_INITUNFOLDPCMAIN";
