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
 
(* Modeling the ARAN protocol
 *)

OS.FileSys.chDir "../..";
use "smlnj.sml";
Flags.setIntFlag "/debug/level" 10;
SMLofNJ.Internals.GC.messages false;
use_shorthands true;

(* Components controls *)
val Location        = active0 ("Location") (* Areas the nodes reside in *)
val Nodes           = active0 ("Nodes")    (* Container for nodes *)
val Node            = active ("Node" -: 1) (* A node of the network *)
val Nonces          = active0 ("Nonces")   (* Contains old requests *)
val Neighbours      = passive0 ("Neighbours") (* Location neighbours *)
val Neighbour       = atomic ("Neighbour" -: 1)
val Send            = passive0 ("Send")
val Packet          = passive0 ("Packet") (* The actual packet of the network *)
val Target          = atomic ("Target" -: 1)
val TargetBroadcast = atomic0 ("TargetBroadcast")
val RDP             = atomic0 ("RDP")     (* Request packet type *)
val REP             = atomic0 ("REP")     (* Reply packet type *)
val Destination     = atomic ("Destination" -: 1)
val Certificate     = atomic ("Certificate" -: 1)
val Forward         = passive0 ("Forward") (* Contains the original part of a
                                             forwarded packet *)
val Nonce           = atomic ("Nonce" -: 1)
val NonceEntry      = passive ("NonceEntry" -: 1)
val NonceMain       = atomic ("NonceMain" -: 1)
val Paths           = passive0 ("Paths")  (* Old paths for reply routing *)
val PathEntry       = passive ("PathEntry" -: 1)
val CheckSig        = passive0 ("CheckSig")
val Resend          = passive0 ("Resend")
val ReceiveItterator= active0 ("ReceiveItterator")
val NonceItterator  = passive0 ("NonceItterator")
val Renegade        = atomic0 ("Renegade")




val ( neighbour1,  neighbour2,  neighbour3,  neighbour,  neighbour0,
      keyset1,  keyset2,  keyset3,  keyset4,  keyset,
      ip1,  ip2,  ip3,  ip4,  ip,  ip0a,  ip0b, ip0c,  ip0d,
      nonce1,  nonce2,  nonce3,  nonce)
  = ("neighbour1","neighbour2","neighbour3","neighbour","neighbour0",
     "keyset1","keyset2","keyset3","keyset4","keyset",
     "ip1","ip2","ip3","ip4","ip","ip0a","ip0b","ip0c","ip0d",
     "nonce1","nonce2","nonce3","nonce")

(* Sending a packet from a location with only 1 neighbour location.
 * Packet will be sent to neighbour location and current location. *)
val SENDPACKET1NEIGHBOUR = "SENDPACKET1NEIGHBOUR" ::: 
 (-/neighbour) o
 (Neighbours o merge(2) o 
    (Neighbour[neighbour] ||
     Send o merge(2) o
       (idp(1) ||
        Packet o idp(1))) ||
  Neighbours o merge(2) o
    (idp(1) ||
     Neighbour[neighbour]))
  --[0 |-> 0, 1 |-> 1, 2 |-> 2, 3 |-> 1]--|>
 (-/neighbour) o
 ((Neighbours o merge(2) o 
     (Neighbour[neighbour] ||
      Send o idp(1)) `|`
   Packet o idp(1)) ||
  (Neighbours o merge(2) o
     (idp(1) ||
      Neighbour[neighbour]) `|`
   Packet o idp(1)))

(* Sending a packet from a location with 2 neighbour locations.
 * Packet will be sent to neighbour locations and current location. *)
val SENDPACKET2NEIGHBOURS = "SENDPACKET2NEIGHBOURS" ::: 
 (-/neighbour * -/neighbour0) o
 (Neighbours o merge(3) o 
    (Neighbour[neighbour] ||
     Neighbour[neighbour0] ||
     Send o merge(2) o
       (idp(1) ||
        Packet o idp(1))) ||
  Neighbours o merge(2) o
    (idp(1) ||
     Neighbour[neighbour]) ||
  Neighbours o merge(2) o
    (idp(1) ||
     Neighbour[neighbour0]))
  --[0 |-> 0, 1 |-> 1, 2 |-> 2, 3 |-> 1, 4 |-> 3, 5 |-> 1]--|>
 (-/neighbour * -/neighbour0) o
 ((Neighbours o merge(3) o 
     (Neighbour[neighbour] ||
      Neighbour[neighbour0] ||
      Send o idp(1)) `|`
   Packet o idp(1)) ||
  (Neighbours o merge(2) o
     (idp(1) ||
      Neighbour[neighbour]) `|`
   Packet o idp(1)) ||
  (Neighbours o merge(2) o
     (idp(1) ||
      Neighbour[neighbour0]) `|`
   Packet o idp(1)))

(* Packet arived at a location, all nodes in location should recieve it.
 * Put receiveitterator around the nodes to start itteration. *)
val RECEIVEINIT = "RECEIVEINIT" ::: 
 (Packet o idp(1) `|`
  Nodes o idp(1))
  --[0 |-> 0, 1 |-> 1]--|>
 (Nodes o ReceiveItterator o merge(2) o
    (Packet o idp(1) ||
     idp(1)))

(* Unicast packet arived at the wrong destination.
 * Remove the itterator items. *)
val RECEIVEITTERATEUNICASTMISS = "RECEIVEITTERATEUNICASTMISS" ::: 
 (ReceiveItterator o merge(2) o
    (idp(1) ||
     Packet o merge(2) o
       (idp(1) ||
        Target[ip])) ||
  Node[ip] o idp(1))
  --[0 |-> 0, 1 |-> 2]--|>
 (idp(1) ||
  Node[ip] o idp(1))

(* Unicast packet arived at the destination.
 * Destination node recieves the packet, all else ignore it. *)
val RECEIVEITTERATEUNICASTHIT = "RECEIVEITTERATEUNICASTHIT" ::: 
 (ReceiveItterator o merge(3) o
    (idp(1) ||
     Packet o merge(2) o
       (idp(1) ||
        Target[ip]) ||
     Node[ip] o idp(1)))
  --[0 |-> 0, 1 |-> 2, 2 |-> 1]--|>
 (merge(2) o
    (idp(1) ||
     Node[ip] o merge(2) o
       (idp(1) ||
        Packet o merge(2) o
          (idp(1) ||
           Target[ip]))))

(* Broadcast packet arived at a location.
 * Copy the packet into one node (and repeat itteration). *)
val RECEIVEITTERATEBROADCASTLOOP = "RECEIVEITTERATEBROADCASTLOOP" ::: 
 (ReceiveItterator o merge(3) o
    (idp(1) ||
     Packet o merge(2) o
       (idp(1) ||
        TargetBroadcast) ||
     Node[ip0a] o idp(1)))
  --[0 |-> 0, 1 |-> 1, 2 |-> 2, 3 |-> 1]--|>
 (ReceiveItterator o merge(2) o
    (idp(1) ||
     Packet o merge(2) o
       (idp(1) ||
        TargetBroadcast)) `|`
  Node[ip0a] o merge(2) o
    (idp(1) ||
     Packet o merge(2) o
       (idp(1) ||
        TargetBroadcast)))

(* Packet arived at a renegade node.
 * Copy the packet into the renbegate node, no matter what. *)
val RECEIVEITTERATERENEGADE = "RECEIVEITTERATERENEGADE" ::: 
 (ReceiveItterator o merge(3) o
    (idp(1) ||
     Packet o idp(1) ||
     Node[ip0a] o merge(2) o
       (idp(1) ||
        Renegade)))
  --[0 |-> 0, 1 |-> 1, 2 |-> 2, 3 |-> 1]--|>
 (ReceiveItterator o merge(2) o
    (idp(1) ||
     Packet o merge(2) o
       (idp(1) ||
        TargetBroadcast)) `|`
  Node[ip0a] o merge(3) o
    (idp(1) ||
     Renegade ||
     Packet o idp(1))) handle e => explain e;

(* Itteration complete, delete the itteration items. *)
val RECEIVEITTERATESTOP = "RECEIVEITTERATESTOP" ::: 
 (ReceiveItterator o Packet o idp(1))
  ----|>
 <->

(* Node has received a packet, prepare to test if it is a reapeat/echo.
 * Put nonceitterator around packet and copy of nonces. *)
val NONCEITTERATEINIT = "NONCEITTERATEINIT" ::: 
 (Node[ip] o merge(3) o
    (idp(1) ||
     Packet o idp(1) ||
     Nonces o idp(1)))
  --[0 |-> 0, 1 |-> 2, 2 |-> 2, 3 |-> 1]--|>
 (Node[ip] o merge(3) o
    (idp(1) ||
     Nonces o idp(1) ||
     NonceItterator o merge(2) o
       (idp(1) ||
        Packet o idp(1))))

(* Node has received a RDP packet, which has not been seen before.
 * Not matches in loop, move packet to a CheckSig. *)
val NONCEITTERATESTOPRDP = "NONCEITTERATESTOPRDP" ::: 
 (NonceItterator o Packet o merge(3) o
       (idp(1) ||
        Nonce[nonce] ||
        RDP) `|`
  Nonces o idp(1))
  --[0 |-> 0, 1 |-> 1]--|>
 (CheckSig o 
    (Packet o merge(3) o
       (idp(1) ||
        Nonce[nonce] ||
        RDP)) `|`
  Nonces o merge(2) o
    (idp(1) ||
     NonceEntry[nonce] o RDP))

(* Node has received a RDP packet, which has been seen before.
 * Delete packet and itterator. *)
val NONCEITTERATEHITRDP = "NONCEITTERATEHITRDP" ::: 
 (NonceItterator o merge(3) o
    (idp(1) ||
     Packet o merge(3) o
       (idp(1) ||
        Nonce[nonce] ||
        RDP) ||
     NonceEntry[nonce] o RDP))
  ----|>
 (nonce//[] * idp(1))

(* Node has received a REP packet, which has not been seen before.
 * Not matches in loop, move packet to a CheckSig. *)
val NONCEITTERATESTOPREP = "NONCEITTERATESTOPREP" ::: 
 (NonceItterator o merge(3) o 
    (idp(1) ||
     NonceEntry[nonce] o RDP ||
     Packet o merge(3) o
       (idp(1) ||
        Nonce[nonce] ||
        REP)) `|`
  Nonces o merge(2) o
    (idp(1) ||
     NonceEntry[nonce] o RDP))
  --[0 |-> 1, 1 |-> 2]--|>
 (CheckSig o 
    (Packet o merge(3) o
       (idp(1) ||
        Nonce[nonce] ||
        REP)) `|`
  Nonces o merge(2) o
    (idp(1) ||
     NonceEntry[nonce] o REP)) handle e => explain e;

(* Itteration through unmatching NonceEntry.
 * Wrong NonceEntry found, discard NonceEntry copy. *)
val NONCEITTERATELOOP = "NONCEITTERATELOOP" ::: 
 (NonceItterator o merge(3) o 
    (idp(1) ||
     NonceEntry[nonce1] o idp(1) ||
     Packet o merge(2) o
       (idp(1) ||
        Nonce[nonce2])) ||
  NonceMain[nonce1] ||
  NonceMain[nonce2])
  --[0 |-> 0, 1 |-> 2]--|>
 (NonceItterator o merge(2) o 
    (idp(1) ||
     Packet o merge(2) o
       (idp(1) ||
        Nonce[nonce2])) ||
  NonceMain[nonce1] ||
  NonceMain[nonce2]) handle e => explain e;


(* Node has received a new packet, not forwarded, check certificate.
 * Pass on packet with correct certificate. *)
val CHECKSIGNATURE = "CHECKSIGNATURE" ::: 
 (CheckSig o 
    (Packet o merge(3) o
       (Certificate[ip] ||
        Destination[ip0b] || (* Destination here means it is not forward! *)
        idp(1))))
  --[0 |-> 0]--|>
 (Resend o
    (Packet o merge(3) o
       (Certificate[ip] ||
        Forward o merge(2) o
          (Destination[ip0b] ||
           Certificate[ip]) ||
        idp(1)))) handle e => explain e;

(* Node has received a new packet, forwardeded, check certificates.
 * Pass on packet with correct certificates. *)
val CHECKSIGNATURES = "CHECKSIGNATURES" ::: 
 (CheckSig o 
    (Packet o merge(3) o
       (Certificate[ip] ||
        Forward o merge(2) o
          (Destination[ip0b] ||
           Certificate[ip0c]) ||
        idp(1))))
  --[0 |-> 0]--|>
 (Resend o
    (Packet o merge(3) o
       (Certificate[ip] ||
        Forward o merge(2) o
          (Destination[ip0b] ||
           Certificate[ip0c]) ||
        idp(1)))) handle e => explain e;

(* Broadcast has been checked, forward the broadcast
 * Move packet to resend location, and sign it with own certificate. *)
val RESENDRDPBROADCAST = "RESENDRDPBROADCAST" ::: 
 (Nodes o merge(2) o
    (idp(1) ||
     Node[ip0c] o merge(4) o
       (idp(1) ||
        Resend o 
          (Packet o merge(4) o
             (idp(1) || 
              TargetBroadcast ||
              Forward o merge(2) o
                (Destination[ip] ||
                 Certificate[ip0a]) ||
              Certificate[ip0b])) ||
        Paths o idp(1) ||
        Certificate[ip0c])) `|`
  Neighbours o merge(2) o
    (idp(1) ||
     Send o idp(1))) ||
 Node[ip]
  --[0 |-> 0, 1 |-> 1, 2 |-> 3, 3 |-> 4, 4 |-> 5, 5 |-> 2]--|>
 (Nodes o merge(2) o
    (idp(1) ||
     Node[ip0c] o merge(3) o
       (idp(1) ||
        Paths o merge(2) o
          (idp(1) ||
           PathEntry[ip0a] o Target[ip0b]) ||
        Certificate[ip0c])) `|`
  Neighbours o merge(2) o
    (idp(1) ||
     Send o merge(2) o
       (idp(1) ||
        Packet o merge(4) o
          (idp(1) ||
           TargetBroadcast ||
           Forward o merge(2) o
             (Destination[ip] ||
              Certificate[ip0a]) ||
           Certificate[ip0c])))) ||
 Node[ip] handle e => explain e;

(* REP unicast packet has been checked, forward the packet using 
 * Move packet to resend location, and sign it with own certificate. *)
val RESENDREPUNICAST = "RESENDREPUNICAST" ::: 
 (Nodes o merge(2) o
    (idp(1) ||
     Node[ip0c] o merge(4) o
       (idp(1) ||
        Resend o 
          (Packet o merge(4) o
             (idp(1) || 
              Target[ip0c] ||     (* not broadcast *)
              Forward o merge(2) o
                (Destination[ip] ||
                 Certificate[ip0a]) ||
              Certificate[ip0d])) || (* remove this old certificate *)
        Paths o merge(2) o
          (idp(1) ||
           PathEntry[ip] o  Target[ip0b]) ||
        Certificate[ip0c])) `|`
  Neighbours o merge(2) o
    (idp(1) ||
     Send o idp(1))) ||
 Node[ip]
  --[0 |-> 0, 1 |-> 1, 2 |-> 3, 3 |-> 4, 4 |-> 5, 5 |-> 2]--|>
 (ip0d//[]) o
 (Nodes o merge(2) o
    (idp(1) ||
     Node[ip0c] o merge(3) o
       (idp(1) ||
        Paths o merge(2) o
          (idp(1) ||
           PathEntry[ip] o Target[ip0b]) ||
        Certificate[ip0c])) `|`
  Neighbours o merge(2) o
    (idp(1) ||
     Send o merge(2) o
       (idp(1) ||
        Packet o merge(4) o
          (idp(1) ||
           Target[ip0b] ||
           Forward o merge(2) o
             (Destination[ip] ||
              Certificate[ip0a]) ||
           Certificate[ip0c])))) ||
 Node[ip] handle e => explain e;

(* Broadcast has been checked, and is at it's destination
 * Send a REP packet back to original source. *)
val RDPRECEIVED = "RDPRECEIVED" ::: 
 (Nodes o merge(2) o
    (idp(1) ||
     Node[ip0c] o merge(4) o
       (idp(1) ||
        Resend o 
          (Packet o merge(4) o
             (idp(1) || 
              Nonce[nonce] ||
              Forward o merge(2) o
                (Destination[ip0c] ||
                 Certificate[ip0a]) ||
              Certificate[ip0b])) || (* RDP TYPE??? *)
        Paths o idp(1) ||
        Certificate[ip0c])) `|`
  Neighbours o merge(2) o
    (idp(1) ||
     Send o idp(1)))
  --[0 |-> 0, 1 |-> 1, 2 |-> 3, 3 |-> 4, 4 |-> 5]--|>
 (Nodes o merge(2) o
    (idp(1) ||
     Node[ip0c] o merge(3) o
       (idp(1) ||
        Paths o merge(2) o
          (idp(1) ||
           PathEntry[ip0a] o Target[ip0b]) ||
        Certificate[ip0c])) `|`
  Neighbours o merge(2) o
    (idp(1) ||
     Send o merge(2) o
       (idp(1) ||
        Packet o merge(5) o
          (Target[ip0b] ||      (* send back to last hop *)
           Destination[ip0a] || (* Destination is original RDP source *)
           REP ||
           Nonce[nonce] ||
           Certificate[ip0c]))))

(* REP has been checked, and is at it's destination
 * Delete REP packet from system. *)
val REPRECEIVED = "REPRECEIVED" ::: 
 (Nodes o merge(2) o
    (idp(1) ||
     Node[ip0c] o merge(2) o
       (idp(1) ||
        Resend o 
          (Packet o merge(3) o
             (idp(1) ||
	      REP ||
              Forward o merge(2) o
                (Destination[ip0c] ||
                 idp(1)))))))
  --[0 |-> 0, 1 |-> 1]--|>
 (Nodes o merge(2) o
    (idp(1) ||
     Node[ip0c] o idp(1)))

(* Renegade nodes has received a packet, and retransmits it.
 * Move packet to resend location, unaltered. *)
val RENEGADERESEND = "RENEGADERESEND" ::: 
 (Nodes o merge(2) o
    (idp(1) ||
     Node[ip0c] o merge(3) o
       (idp(1) ||
        Renegade ||
        CheckSig o Packet o idp(1))) `|`
  Neighbours o merge(2) o
    (idp(1) ||
     Send o idp(1)))
  --[0 |-> 0, 1 |-> 1, 2 |-> 3, 3 |-> 4, 4 |-> 2]--|>
 (Nodes o merge(2) o
    (idp(1) ||
     Node[ip0c] o merge(2) o
       (idp(1) ||
        Renegade)) `|`
  Neighbours o merge(2) o
    (idp(1) ||
     Send o merge(2) o
       (idp(1) ||
        Packet o idp(1))))


(* reaction rules *)
val rulelist = [SENDPACKET1NEIGHBOUR,SENDPACKET2NEIGHBOURS,
                RECEIVEINIT, 
                RECEIVEITTERATEUNICASTHIT, RECEIVEITTERATEUNICASTMISS,
                RECEIVEITTERATEBROADCASTLOOP,RECEIVEITTERATESTOP,
                RECEIVEITTERATERENEGADE,
                NONCEITTERATEINIT,
                NONCEITTERATESTOPRDP,NONCEITTERATEHITRDP,
		NONCEITTERATESTOPREP,NONCEITTERATELOOP,
                CHECKSIGNATURE,CHECKSIGNATURES,
                RESENDRDPBROADCAST,RESENDREPUNICAST,
                RDPRECEIVED,REPRECEIVED,
                RENEGADERESEND];

val rules = mkrules (rulelist);

(*
(* ----------------------------------------------------------------------------
   MODEL 1
   3 nodes in different locations. The middle node forwards the requests 
   between sender and receiver, signing the nodes as required by the 
   ARAN protocol. No errors in this system. *)

(* The system *)
val System0 = simplify (
  (-/neighbour1 * -/neighbour2) o
  (Location o merge(2) o
     (Nodes o 
        (Node[ip1] o merge(4) o
           (Nonces o NonceEntry[nonce1] o RDP ||
            NonceMain[nonce1] ||
            Paths o <-> ||
            Certificate[ip1])) ||
      Neighbours o merge(2) o
        (Neighbour[neighbour1] ||
         Send o Packet o merge(5) o
           (TargetBroadcast ||
            RDP ||
            Destination[ip3] ||
            Nonce[nonce1] || 
            Certificate[ip1]))) `|`
   Location o merge(2) o
     (Nodes o 
        (Node[ip2] o merge(3) o
           (Nonces o <-> ||
            Paths o <-> ||
            Certificate[ip2])) ||
      Neighbours o merge(3) o
        (Neighbour[neighbour1] ||
         Neighbour[neighbour2] ||
         Send o <->)) `|`
   Location o merge(2) o
     (Nodes o 
        (Node[ip3] o merge(3) o
           (Nonces o <-> ||
            Paths o <-> ||
            Certificate[ip3])) ||
      Neighbours o merge(2) o
        (Neighbour[neighbour2] ||
         Send o <->))
  )
) handle e => explain e;

(*print_mv(matches rules System0);*)
val TAC_SENDPACKET1NEIGHBOUR = react_rule "SENDPACKET1NEIGHBOUR";
val System0_step1 = run rules TAC_SENDPACKET1NEIGHBOUR System0;
(*print_mv(matches rules System0_step1);*)
val TAC_RECEIVEINIT = REPEAT (react_rule "RECEIVEINIT");
val System0_step2 = run rules TAC_RECEIVEINIT System0_step1;
(*print_mv(matches rules System0_step2);*)
val TAC_RECEIVEITTERATEBROADCASTLOOP = REPEAT (react_rule "RECEIVEITTERATEBROADCASTLOOP");
val System0_step3 = run rules TAC_RECEIVEITTERATEBROADCASTLOOP System0_step2;
(*print_mv(matches rules System0_step3);*)
val TAC_RECEIVEITTERATESTOP = REPEAT (react_rule "RECEIVEITTERATESTOP");
val System0_step4 = run rules TAC_RECEIVEITTERATESTOP System0_step3;
(*print_mv(matches rules System0_step4);*)
val TAC_NONCEITTERATEINIT = REPEAT (react_rule "NONCEITTERATEINIT");
val System0_step5 = run rules TAC_NONCEITTERATEINIT System0_step4;
(*print_mv(matches rules System0_step5);*)
val TAC_NONCEITTERATEHITRDP = REPEAT (react_rule "NONCEITTERATEHITRDP");
val System0_step6 = run rules TAC_NONCEITTERATEHITRDP System0_step5;
(*print_mv(matches rules System0_step6);*)
val TAC_NONCEITTERATESTOPRDP = REPEAT (react_rule "NONCEITTERATESTOPRDP");
val System0_step7 = run rules TAC_NONCEITTERATESTOPRDP System0_step6;
(*print_mv(matches rules System0_step7);*)
val TAC_CHECKSIGNATURE = (*REPEAT*) (react_rule "CHECKSIGNATURE");
val System0_step8 = run rules TAC_CHECKSIGNATURE System0_step7;
(*print_mv(matches rules System0_step8);*)
val TAC_RESENDRDPBROADCAST = (*REPEAT*) (react_rule "RESENDRDPBROADCAST");
val System0_step9 = run rules TAC_RESENDRDPBROADCAST System0_step8;
(*print_mv(matches rules System0_step9);*)
val TAC_SENDPACKET2NEIGHBOURS = react_rule "SENDPACKET2NEIGHBOURS";
val System0_step10 = run rules TAC_SENDPACKET2NEIGHBOURS System0_step9;

(*print_mv(matches rules System0_step10);*)
val System0_step11 = run rules TAC_RECEIVEINIT System0_step10;
(*print_mv(matches rules System0_step11);*)
val System0_step12 = run rules TAC_RECEIVEITTERATEBROADCASTLOOP System0_step11;
(*print_mv(matches rules System0_step12);*)
val System0_step13 = run rules TAC_RECEIVEITTERATESTOP System0_step12;

(*print_mv(matches rules System0_step13);*)
val System0_step14 = run rules TAC_NONCEITTERATEINIT System0_step13;
(*print_mv(matches rules System0_step14);*)
val System0_step15 = run rules TAC_NONCEITTERATEHITRDP System0_step14;
(*print_mv(matches rules System0_step15);*)
val System0_step16 = run rules TAC_NONCEITTERATESTOPRDP System0_step15;
(*print_mv(matches rules System0_step16);*)
val TAC_CHECKSIGNATURES = (*REPEAT*) (react_rule "CHECKSIGNATURES");
val System0_step17 = run rules TAC_CHECKSIGNATURES System0_step16;

(*print_mv(matches rules System0_step17);*)
val TAC_RDPRECEIVED = (*REPEAT*) (react_rule "RDPRECEIVED");
val System0_step18 = run rules TAC_RDPRECEIVED System0_step17;
(*print_mv(matches rules System18);*)
val System0_step19 = run rules TAC_SENDPACKET1NEIGHBOUR System0_step18;
(*print_mv(matches rules System0_step19);*)
val System0_step20 = run rules TAC_RECEIVEINIT System0_step19;
(*print_mv(matches rules System0_step20);*)
val TAC_RECEIVEITTERATEUNICASTMISS = REPEAT (react_rule "RECEIVEITTERATEUNICASTMISS");
val System0_step21 = run rules TAC_RECEIVEITTERATEUNICASTMISS System0_step20;
(*print_mv(matches rules System0_step21);*)
val TAC_RECEIVEITTERATEUNICASTHIT = REPEAT (react_rule "RECEIVEITTERATEUNICASTHIT");
val System0_step22 = run rules TAC_RECEIVEITTERATEUNICASTHIT System0_step21;

(*print_mv(matches rules System0_step22);*)
val System0_step23 = run rules TAC_NONCEITTERATEINIT System0_step22;
(*print_mv(matches rules System0_step23);*)
val TAC_NONCEITTERATESTOPREP = REPEAT (react_rule "NONCEITTERATESTOPREP");
val System0_step24 = run rules TAC_NONCEITTERATESTOPREP System0_step23;
(*print_mv(matches rules System0_step24);*)
val System0_step25 = run rules TAC_CHECKSIGNATURE System0_step24;

(*print_mv(matches rules System0_step25);*)
val TAC_RESENDREPUNICAST = (*REPEAT*) (react_rule "RESENDREPUNICAST");
val System0_step26 = run rules TAC_RESENDREPUNICAST System0_step25;
(*print_mv(matches rules System0_step26);*)
val System0_step27 = run rules TAC_SENDPACKET2NEIGHBOURS System0_step26;

(*print_mv(matches rules System0_step27);*)
val System0_step28 = run rules TAC_RECEIVEINIT System0_step27;
(*print_mv(matches rules System0_step28);*)
val System0_step29 = run rules TAC_RECEIVEITTERATEUNICASTMISS System0_step28;
(*print_mv(matches rules System0_step29);*)
val System0_step30 = run rules TAC_RECEIVEITTERATEUNICASTHIT System0_step29;

(*print_mv(matches rules System0_step30);*)
val System0_step31 = run rules TAC_NONCEITTERATEINIT System0_step30;
(*print_mv(matches rules System0_step31);*)
val System0_step32 = run rules TAC_NONCEITTERATESTOPREP System0_step31;
(*print_mv(matches rules System0_step32);*)
val System0_step33 = run rules TAC_CHECKSIGNATURES System0_step32;
(*print_mv(matches rules System0_step33);*)
val TAC_REPRECEIVED = (*REPEAT*) (react_rule "REPRECEIVED");
val System0_step34 = run rules TAC_RDPRECEIVED System0_step33;
*)



(* ----------------------------------------------------------------------------
   MODEL 2
   4 nodes in different locations. A renegade node is only path to the receiver
   and the ARAN protokol allows the renegade to repeat the packets unaltered,
   without the neighbours detecting it!
   This is an uninteded weakness in the ARAN protokol! *)

(* The system *)
val System1 = simplify (
  (-/neighbour1 * -/neighbour2 * -/neighbour3) o
  (Location o merge(2) o
     (Nodes o 
        (Node[ip1] o merge(4) o
           (Nonces o NonceEntry[nonce1] o RDP ||
            NonceMain[nonce1] ||
            Paths o <-> ||
            Certificate[ip1])) ||
      Neighbours o merge(2) o
        (Neighbour[neighbour1] ||
         Send o Packet o merge(5) o
           (TargetBroadcast ||
            RDP ||
            Destination[ip3] ||
            Nonce[nonce1] || 
            Certificate[ip1]))) `|`
   Location o merge(2) o
     (Nodes o 
        (Node[ip2] o merge(3) o
           (Nonces o <-> ||
            Paths o <-> ||
            Certificate[ip2])) ||
      Neighbours o merge(3) o
        (Neighbour[neighbour1] ||
         Neighbour[neighbour2] ||
         Send o <->)) `|`
   Location o merge(2) o
     (Nodes o 
        (Node[ip4] o merge(2) o
           (Renegade ||
            Nonces o <->)) ||
      Neighbours o merge(3) o
        (Neighbour[neighbour2] ||
         Neighbour[neighbour3] ||
         Send o <->)) `|`
   Location o merge(2) o
     (Nodes o 
        (Node[ip3] o merge(3) o
           (Nonces o <-> ||
            Paths o <-> ||
            Certificate[ip3])) ||
      Neighbours o merge(2) o
        (Neighbour[neighbour3] ||
         Send o <->))
  )
) handle e => explain e;

(*THE EVENT ORDER FOR THE SYSTEM*)
(*print_mv(matches rules System1);*)
val TAC_SENDPACKET1NEIGHBOUR = react_rule "SENDPACKET1NEIGHBOUR";
val System1_step1 = run rules TAC_SENDPACKET1NEIGHBOUR System1;
(*print_mv(matches rules System1_step1);*)
val TAC_RECEIVEINIT = REPEAT (react_rule "RECEIVEINIT");
val System1_step2 = run rules TAC_RECEIVEINIT System1_step1;
(*print_mv(matches rules System1_step2);*)
val TAC_RECEIVEITTERATEBROADCASTLOOP = REPEAT (react_rule "RECEIVEITTERATEBROADCASTLOOP");
val System1_step3 = run rules TAC_RECEIVEITTERATEBROADCASTLOOP System1_step2;
(*print_mv(matches rules System1_step3);*)
val TAC_RECEIVEITTERATESTOP = REPEAT (react_rule "RECEIVEITTERATESTOP");
val System1_step4 = run rules TAC_RECEIVEITTERATESTOP System1_step3;
(*print_mv(matches rules System1_step4);*)
val TAC_NONCEITTERATEINIT = REPEAT (react_rule "NONCEITTERATEINIT");
val System1_step5 = run rules TAC_NONCEITTERATEINIT System1_step4;
(*print_mv(matches rules System1_step5);*)
val TAC_NONCEITTERATEHITRDP = REPEAT (react_rule "NONCEITTERATEHITRDP");
val System1_step6 = run rules TAC_NONCEITTERATEHITRDP System1_step5;
(*print_mv(matches rules System1_step6);*)
val TAC_NONCEITTERATESTOPRDP = REPEAT (react_rule "NONCEITTERATESTOPRDP");
val System1_step7 = run rules TAC_NONCEITTERATESTOPRDP System1_step6;
(*print_mv(matches rules System1_step7);*)
val TAC_CHECKSIGNATURE = (*REPEAT*) (react_rule "CHECKSIGNATURE");
val System1_step8 = run rules TAC_CHECKSIGNATURE System1_step7;
(*print_mv(matches rules System1_step8);*)
val TAC_RESENDRDPBROADCAST = (*REPEAT*) (react_rule "RESENDRDPBROADCAST");
val System1_step9 = run rules TAC_RESENDRDPBROADCAST System1_step8;
(*print_mv(matches rules System1_step9);*)
val TAC_SENDPACKET2NEIGHBOURS = react_rule "SENDPACKET2NEIGHBOURS";
val System1_step10 = run rules TAC_SENDPACKET2NEIGHBOURS System1_step9;

(*print_mv(matches rules System1_step10);*)
val System1_step11 = run rules TAC_RECEIVEINIT System1_step10;
(*print_mv(matches rules System1_step11);*)
val System1_step12 = run rules TAC_RECEIVEITTERATEBROADCASTLOOP System1_step11;
(*print_mv(matches rules System1_step12);*)
val System1_step13 = run rules TAC_RECEIVEITTERATESTOP System1_step12;

(* --- START RENEGADE RDP FORWARD --- *)
(*print_mv(matches rules System1_step13);*)
val System1_step13ren1 = run rules TAC_NONCEITTERATEINIT System1_step13;
(*print_mv(matches rules System1_step13ren1);*)
val System1_step13ren2 = run rules TAC_NONCEITTERATEHITRDP System1_step13ren1;
(*print_mv(matches rules System1_step13ren2);*)
val System1_step13ren3 = run rules TAC_NONCEITTERATESTOPRDP System1_step13ren2;
(*print_mv(matches rules System1_step13ren3);*)
val TAC_RENEGADERESEND = react_rule "RENEGADERESEND";
val System1_step13ren4 = run rules TAC_RENEGADERESEND System1_step13ren3;
(*print_mv(matches rules System1_step13ren4);*)
val System1_step13ren5 = run rules TAC_SENDPACKET2NEIGHBOURS System1_step13ren4;
(*print_mv(matches rules System1_step13ren5);*)
val System1_step13ren6 = run rules TAC_RECEIVEINIT System1_step13ren5;
(*print_mv(matches rules System1_step13ren6);*)
val System1_step13ren7 = run rules TAC_RECEIVEITTERATEBROADCASTLOOP System1_step13ren6;
(*print_mv(matches rules System1_step13ren7);*)
val System1_step13ren8 = run rules TAC_RECEIVEITTERATESTOP System1_step13ren7;
(*print_mv(matches rules System1_step13ren8);*)
(* --- END RENEGADE RDP FORWARD --- *)

(*print_mv(matches rules System1_step13);*)
val System1_step14 = run rules TAC_NONCEITTERATEINIT System1_step13ren8;
(*print_mv(matches rules System1_step14);*)
val System1_step15 = run rules TAC_NONCEITTERATEHITRDP System1_step14;
(*print_mv(matches rules System1_step15);*)
val System1_step16 = run rules TAC_NONCEITTERATESTOPRDP System1_step15;
(*print_mv(matches rules System1_step16);*)
val TAC_CHECKSIGNATURES = (*REPEAT*) (react_rule "CHECKSIGNATURES");
val System1_step17 = run rules TAC_CHECKSIGNATURES System1_step16;

(*print_mv(matches rules System1_step17);*)
val TAC_RDPRECEIVED = (*REPEAT*) (react_rule "RDPRECEIVED");
val System1_step18 = run rules TAC_RDPRECEIVED System1_step17;
(*print_mv(matches rules System1_step18);*)
val System1_step19 = run rules TAC_SENDPACKET1NEIGHBOUR System1_step18;
(*print_mv(matches rules System1_step19);*)
val System1_step20 = run rules TAC_RECEIVEINIT System1_step19;

(* --- START RENEGADE REP FORWARD --- *)
(*print_mv(matches rules System1_step20);*)
val TAC_RECEIVEITTERATERENEGADE = (*REPEAT*) (react_rule "RECEIVEITTERATERENEGADE");
val System1_step20ren1 = run rules TAC_RECEIVEITTERATERENEGADE System1_step20;
(*print_mv(matches rules System1_step20ren1);*)
val TAC_RECEIVEITTERATEUNICASTMISS = REPEAT (react_rule "RECEIVEITTERATEUNICASTMISS");
val System1_step20ren2 = run rules TAC_RECEIVEITTERATEUNICASTMISS System1_step20ren1;
(*print_mv(matches rules System1_step20ren2);*)
val System1_step20ren3 = run rules TAC_NONCEITTERATEINIT System1_step20ren2;
(*print_mv(matches rules System1_step20ren3);*)
val TAC_NONCEITTERATESTOPREP = REPEAT (react_rule "NONCEITTERATESTOPREP");
val System1_step20ren4 = run rules TAC_NONCEITTERATESTOPREP System1_step20ren3;
(*print_mv(matches rules System1_step20ren4);*)
val System1_step20ren5 = run rules TAC_RENEGADERESEND System1_step20ren4;
(*print_mv(matches rules System1_step20ren5);*)
val System1_step20ren6 = run rules TAC_SENDPACKET2NEIGHBOURS System1_step20ren5;
(*print_mv(matches rules System1_step20ren6);*)
val System1_step20ren7 = run rules TAC_RECEIVEINIT System1_step20ren6;
(* --- END RENEGADE REP FORWARD --- *)


(*print_mv(matches rules System1_step20);*)
val TAC_RECEIVEITTERATEUNICASTMISS = REPEAT (react_rule "RECEIVEITTERATEUNICASTMISS");
val System1_step21 = run rules TAC_RECEIVEITTERATEUNICASTMISS System1_step20ren7;
(*print_mv(matches rules System1_step21);*)
val TAC_RECEIVEITTERATEUNICASTHIT = REPEAT (react_rule "RECEIVEITTERATEUNICASTHIT");
val System1_step22 = run rules TAC_RECEIVEITTERATEUNICASTHIT System1_step21;

(*print_mv(matches rules System1_step22);*)
val System1_step23 = run rules TAC_NONCEITTERATEINIT System1_step22;
(*print_mv(matches rules System1_step23);*)
val TAC_NONCEITTERATESTOPREP = REPEAT (react_rule "NONCEITTERATESTOPREP");
val System1_step24 = run rules TAC_NONCEITTERATESTOPREP System1_step23;
(*print_mv(matches rules System1_step24);*)
val System1_step25 = run rules TAC_CHECKSIGNATURE System1_step24;

(*print_mv(matches rules System1_step25);*)
val TAC_RESENDREPUNICAST = (*REPEAT*) (react_rule "RESENDREPUNICAST");
val System1_step26 = run rules TAC_RESENDREPUNICAST System1_step25;
(*print_mv(matches rules System1_step26);*)
val System1_step27 = run rules TAC_SENDPACKET2NEIGHBOURS System1_step26;

(*print_mv(matches rules System1_step27);*)
val System1_step28 = run rules TAC_RECEIVEINIT System1_step27;
(*print_mv(matches rules System1_step28);*)
val System1_step29 = run rules TAC_RECEIVEITTERATEUNICASTMISS System1_step28;
(*print_mv(matches rules System1_step29);*)
val System1_step30 = run rules TAC_RECEIVEITTERATEUNICASTHIT System1_step29;

(*print_mv(matches rules System1_step30);*)
val System1_step31 = run rules TAC_NONCEITTERATEINIT System1_step30;
(*print_mv(matches rules System1_step31);*)
val System1_step32 = run rules TAC_NONCEITTERATESTOPREP System1_step31;
(*print_mv(matches rules System1_step32);*)
val System1_step33 = run rules TAC_CHECKSIGNATURES System1_step32;
(*print_mv(matches rules System1_step33);*)
val TAC_REPRECEIVED = (*REPEAT*) (react_rule "REPRECEIVED");
val System1_step34 = run rules TAC_REPRECEIVED System1_step33;

(* End system. The sender has received the reply, and noone has noticed the
   attack from the renegade *)

