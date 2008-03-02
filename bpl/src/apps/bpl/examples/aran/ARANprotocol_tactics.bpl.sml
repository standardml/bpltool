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
 
(* Modeling the ARAN protocol.
   Improved greatly by the use of tactics!
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
val Paths           = passive0 ("Paths")  (* Old paths for reply routing *)
val PathEntry       = passive ("PathEntry" -: 1)
val Resend          = passive0 ("Resend")
val TacticToken     = passive0("TacticToken") (* New unified tactic token *)
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
 * Put receiveitterator around the nodes to start itteration. 
 * Changed for tactics, new tokenname TacticToken *)
val RECEIVEINIT = "RECEIVEINIT" ::: 
 (Packet o idp(1) `|`
  Nodes o idp(1))
  --[0 |-> 0, 1 |-> 1]--|>
 (Nodes o TacticToken o merge(2) o
    (Packet o idp(1) ||
     idp(1)))

(* Unicast packet arived at the wrong destination.
 * Remove the itterator items. 
 * Improved for tactics, original rulename RECEIVEITTERATEUNICASTMISS *)
val RECEIVEITTERATEDELETEUNICAST = "RECEIVEITTERATEDELETEUNICAST" ::: 
 (TacticToken o merge(2) o
    (idp(1) ||
     Packet o merge(2) o
       (idp(1) ||
        Target[ip])))
  --[0 |-> 0]--|>
 (ip//[] * idp(1))

(* Unicast packet arived at the destination.
 * Destination node recieves the packet, all else ignore it.
 * Changed for tactics, new tokenname TacticToken *)
val RECEIVEITTERATEUNICASTHIT = "RECEIVEITTERATEUNICASTHIT" ::: 
 (TacticToken o merge(3) o
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
 * Copy the packet into one node (and repeat itteration).
 * Changed for tactics, new tokenname TacticToken *)
val RECEIVEITTERATEBROADCASTLOOP = "RECEIVEITTERATEBROADCASTLOOP" ::: 
 (TacticToken o merge(3) o
    (idp(1) ||
     Packet o merge(2) o
       (idp(1) ||
        TargetBroadcast) ||
     Node[ip0a] o idp(1)))
  --[0 |-> 0, 1 |-> 1, 2 |-> 2, 3 |-> 1]--|>
 (TacticToken o merge(2) o
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
 * Copy the packet into the renbegate node, no matter what.
 * Changed for tactics, new tokenname TacticToken *)
val RECEIVEITTERATERENEGADE = "RECEIVEITTERATERENEGADE" ::: 
 (TacticToken o merge(3) o
    (idp(1) ||
     Packet o idp(1) ||
     Node[ip0a] o merge(2) o
       (idp(1) ||
        Renegade)))
  --[0 |-> 0, 1 |-> 1, 2 |-> 2, 3 |-> 1]--|>
 (TacticToken o merge(2) o
    (idp(1) ||
     Packet o merge(2) o
       (idp(1) ||
        TargetBroadcast)) `|`
  Node[ip0a] o merge(3) o
    (idp(1) ||
     Renegade ||
     Packet o idp(1))) handle e => explain e;

(* Itteration complete, delete the itteration items.
 * Changed for tactics, new tokenname TacticToken *)
val RECEIVEITTERATESTOP = "RECEIVEITTERATESTOP" ::: 
 (TacticToken o Packet o idp(1))
  ----|>
 (<->) handle e => explain e;

(* Node has received a packet, prepare to test if it is a reapeat/echo.
 * Put noncetest around packet and nonce entries.
 * Improved for tactics, now moving Nonces content instead of copying.
 * Original rulename NONCEITTERATEINIT *)
val NONCETESTINIT = "NONCETESTINIT" ::: 
 (Node[ip] o merge(3) o
    (idp(1) ||
     Packet o idp(1) ||
     Nonces o idp(1)))
  --[0 |-> 0, 1 |-> 2, 2 |-> 1]--|>
 (Node[ip] o merge(2) o
    (idp(1) ||
     TacticToken o merge(2) o
       (idp(1) ||
        Packet o idp(1)))) handle e => explain e;

(* Node has received a REP packet, which has not been seen before.
 * Not matches in loop, packet ready for CheckSignature. 
 * Simplified for tactics, original rulename NONCEITTERATESTOPREP *)
val NONCETESTREPOK = "NONCETESTREPOK" ::: 
 (TacticToken o merge(3) o 
    (idp(1) ||
     NonceEntry[nonce] o RDP ||
     Packet o merge(3) o
       (idp(1) ||
        Nonce[nonce] ||
        REP)))
  --[0 |-> 1, 1 |-> 0]--|>
 (TacticToken o 
    (Packet o merge(3) o
       (idp(1) ||
        Nonce[nonce] ||
        REP)) `|`
  Nonces o merge(2) o
    (idp(1) ||
     NonceEntry[nonce] o REP)) handle e => explain e;

(* Node has received a packet, which has been seen before.
 * Delete packet and testcontainer. 
 * New rule for tactics. Like the original rule NONCEITTERATEHITRDP but
 * covering far more cases *)
val NONCETESTHIT = "NONCETESTHIT" ::: 
 (TacticToken o merge(3) o
    (idp(1) ||
     Packet o merge(2) o
       (idp(1) ||
        Nonce[nonce]) ||
     NonceEntry[nonce] o idp(1)))
  --[0 |-> 0, 1 |-> 2]--|>
 (Nonces o merge(2) o
    (idp(1) ||
     NonceEntry[nonce] o idp(1))) handle e => explain e;

(* Node has received a RDP packet, which has not been seen before.
 * No matches in IF tactic, packet ready for CheckSignature 
 * Simplified for tactics, original rulename NONCEITTERATESTOPRDP *)
val NONCETESTRDPOK = "NONCETESTRDPOK" ::: 
 (TacticToken o merge(2) o
    (idp(1) ||
     Packet o merge(3) o
       (idp(1) ||
        Nonce[nonce] ||
        RDP)))
  --[0 |-> 1, 1 |-> 0]--|>
 (TacticToken o 
    (Packet o merge(3) o
       (idp(1) ||
        Nonce[nonce] ||
        RDP)) `|`
  Nonces o merge(2) o
    (idp(1) ||
     NonceEntry[nonce] o RDP))

(* Node has received REP a packet, for which no RDP has been seen.
 * Delete packet and testcontainer. 
 * New rule for tactics. Improves model to handle more cases*)
val NONCETESTIGNOREREP = "NONCETESTIGNOREREP" ::: 
 (TacticToken o merge(2) o
    (idp(1) ||
     Packet o idp(1)))
  --[0 |-> 0]--|>
 (Nonces o idp(1)) handle e => explain e;


(* Node has received a new packet, not forwarded, check certificate.
 * Pass on packet with correct certificate. *)
val CHECKSIGNATURE = "CHECKSIGNATURE" ::: 
 (TacticToken o 
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
 (TacticToken o 
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

(* Node has received a new packet, certificates not legal.
 * Delete the packet and the CheckSig packet. 
 * New rule for tactics, improving the model *)
val CHECKSIGNATUREIGNORE = "CHECKSIGNATUREIGNORE" ::: 
 (TacticToken o 
    (Packet o idp(1)))
  ----|>
 (<->) handle e => explain e;


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
          (Packet o merge(5) o
             (idp(1) || 
              Nonce[nonce] ||
              Forward o merge(2) o
                (Destination[ip0c] ||
                 Certificate[ip0a]) ||
              Certificate[ip0b] ||
              RDP)) ||
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
              Forward o merge(2) o
                (Destination[ip0c] ||
                 idp(1)) ||
              REP)))))
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
        TacticToken o Packet o idp(1))) `|`
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
                RECEIVEITTERATEUNICASTHIT, RECEIVEITTERATEDELETEUNICAST,
                RECEIVEITTERATEBROADCASTLOOP,RECEIVEITTERATESTOP,
                RECEIVEITTERATERENEGADE,
                NONCETESTINIT, NONCETESTREPOK, NONCETESTHIT,
                NONCETESTRDPOK, NONCETESTIGNOREREP,
                CHECKSIGNATURE,CHECKSIGNATURES,CHECKSIGNATUREIGNORE,
                RESENDRDPBROADCAST,RESENDREPUNICAST,
                RDPRECEIVED,REPRECEIVED,
                RENEGADERESEND];

(* List of rules that can initiate a tactic *)
val ruleliststarters = [SENDPACKET1NEIGHBOUR,SENDPACKET2NEIGHBOURS,
                        RECEIVEINIT,NONCETESTINIT,
                        RESENDRDPBROADCAST,RESENDREPUNICAST,
                        RDPRECEIVED,REPRECEIVED];

val rules         = mkrules (rulelist);
val rulesstarters = mkrules (ruleliststarters);

val TAC_SENDPACKET1NEIGHBOUR  = react_rule "SENDPACKET1NEIGHBOUR";
val TAC_SENDPACKET2NEIGHBOURS = react_rule "SENDPACKET2NEIGHBOURS";

(* Tactic for handling receiving packets *)
val TAC_RECEIVE =
 IF   (react_rule "RECEIVEINIT")
 THEN ((react_rule "RECEIVEITTERATERENEGADE")
       ++
       (TRY    (react_rule "RECEIVEITTERATEUNICASTHIT")
        ORTHEN (TRY    (react_rule "RECEIVEITTERATEDELETEUNICAST")
                ORTHEN ((REPEAT (react_rule "RECEIVEITTERATEBROADCASTLOOP"))
                        ++
                        (react_rule "RECEIVEITTERATESTOP")))))
 ELSE fail;

(* Tactic for testing packet identity (Nonce) *)
val TAC_NONCETEST = 
 IF   (react_rule "NONCETESTINIT")
 THEN (TRY    (react_rule "NONCETESTREPOK")
       ORTHEN (TRY    (react_rule "NONCETESTHIT")
               ORTHEN (TRY    (react_rule "NONCETESTRDPOK")
                       ORTHEN (react_rule "NONCETESTIGNOREREP")
                      )
              )
      )
 ELSE fail;

(* Tactic for testing packet signature(s) *)
val TAC_CHECKSIG =
  (react_rule "RENEGADERESEND")
  ++
  TRY    (react_rule "CHECKSIGNATURE")
  ORTHEN (TRY    (react_rule "CHECKSIGNATURES")
          ORTHEN (react_rule "CHECKSIGNATUREIGNORE"));

(* Tactic for testing packet identity and signatures in a joint, optimal way *)
val TAC_NONCESIGNATURETEST = 
 IF   (react_rule "NONCETESTINIT")
 THEN (IF   (react_rule "NONCETESTREPOK")
       THEN (TAC_CHECKSIG)                          (*REP ok, check signature*)
       ELSE (TRY    (react_rule "NONCETESTHIT")
             ORTHEN (IF   (react_rule "NONCETESTRDPOK")
                     THEN (TAC_CHECKSIG)            (*RDP ok, check signature*)
                     ELSE (react_rule "NONCETESTIGNOREREP")
                    )
            )
      )
 ELSE fail;

(* Tactic for resending, tiny improvement to runspeed only *)
val TAC_RESEND = 
  TRY    (react_rule "RESENDRDPBROADCAST")
  ORTHEN (TRY    (react_rule "RESENDREPUNICAST")
          ORTHEN (TRY    (react_rule "RDPRECEIVED")
                  ORTHEN (react_rule "REPRECEIVED")
                 )
         );




(* ----------------------------------------------------------------------------
   MODEL 2
   4 nodes in different locations. A renegade node is only path to the receiver
   and the ARAN protokol allows the renegade to repeat the packets unaltered,
   without the neighbours detecting it!
   This is an uninteded weakness in the ARAN protokol! *)

val System1 = simplify (
  (-/neighbour1 * -/neighbour2 * -/neighbour3) o
  (Location o merge(2) o
     (Nodes o 
        (Node[ip1] o merge(3) o
           (Nonces o NonceEntry[nonce1] o RDP ||
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

(*print_mv(matches rulesstarters System1);*)
val System1_step1 = run rules TAC_SENDPACKET1NEIGHBOUR System1;
(*print_mv(matches rulesstarters System1_step1);*)
val System1_step2 = run rules TAC_RECEIVE System1_step1;
(*print_mv(matches rulesstarters System1_step2);*)
val System1_step3 = run rules TAC_RECEIVE System1_step2;
(*print_mv(matches rulesstarters System1_step3);*)
val System1_step4 = run rules TAC_NONCESIGNATURETEST System1_step3;
(*print_mv(matches rulesstarters System1_step4);*)
val System1_step5 = run rules TAC_NONCESIGNATURETEST System1_step4;
(*print_mv(matches rulesstarters System1_step5);*)
val System1_step6 = run rules TAC_RESEND System1_step5;
(*print_mv(matches rulesstarters System1_step6);*)
val System1_step7 = run rules TAC_SENDPACKET2NEIGHBOURS System1_step6;
(*print_mv(matches rulesstarters System1_step7);*)
val System1_step8 = run rules TAC_RECEIVE System1_step7;
val System1_step9 = run rules TAC_RECEIVE System1_step8;
val System1_step10 = run rules TAC_RECEIVE System1_step9;
(*print_mv(matches rulesstarters System1_step10);*)
val System1_step11 = run rules TAC_NONCESIGNATURETEST System1_step10;
val System1_step12 = run rules TAC_NONCESIGNATURETEST System1_step11;
val System1_step13 = run rules TAC_NONCESIGNATURETEST System1_step12;
(*print_mv(matches rulesstarters System1_step13);*)
(*RENEGADE NODES PASS CHECKSIGNATURE AND GOES DIRECTLY TO RESEND*)
val System1_step14 = run rules TAC_SENDPACKET2NEIGHBOURS System1_step13;
(*print_mv(matches rulesstarters System1_step14);*)
val System1_step15 = run rules TAC_RECEIVE System1_step14;
val System1_step16 = run rules TAC_RECEIVE System1_step15;
val System1_step17 = run rules TAC_RECEIVE System1_step16;
(*print_mv(matches rules System1_step17);*)
val System1_step18 = run rules TAC_NONCESIGNATURETEST System1_step17;
val System1_step19 = run rules TAC_NONCESIGNATURETEST System1_step18;
val System1_step20 = run rules TAC_NONCESIGNATURETEST System1_step19;
(*print_mv(matches rules System1_step20);*)
(*Receiver send REP reply packet, slow step due to heavy rules in tactic*)
val System1_step21 = run rules TAC_RESEND System1_step20;
(*print_mv(matches rules System1_step21);*)
val System1_step22 = run rules TAC_SENDPACKET1NEIGHBOUR System1_step21;
(*print_mv(matches rules System1_step22);*)
val System1_step23 = run rules TAC_RECEIVE System1_step22;
val System1_step24 = run rules TAC_RECEIVE System1_step23;
(*print_mv(matches rules System1_step24);*)
val System1_step25 = run rules TAC_NONCESIGNATURETEST System1_step24;
(*print_mv(matches rules System1_step25);*)
(*RENEGADE NODES PASS CHECKSIGNATURE AND GOES DIRECTLY TO RESEND*)
val System1_step26 = run rules TAC_SENDPACKET2NEIGHBOURS System1_step25;
(*print_mv(matches rules System1_step26);*)
val System1_step27 = run rules TAC_RECEIVE System1_step26;
val System1_step28 = run rules TAC_RECEIVE System1_step27;
val System1_step29 = run rules TAC_RECEIVE System1_step28;
(*print_mv(matches rules System1_step29);*)
val System1_step30 = run rules TAC_NONCESIGNATURETEST System1_step29;
val System1_step31 = run rules TAC_NONCESIGNATURETEST System1_step30;
(*print_mv(matches rules System1_step30);*)
val System1_step32 = run rules TAC_RESEND System1_step31;
(*print_mv(matches rules System1_step31);*)
val System1_step33 = run rules TAC_SENDPACKET2NEIGHBOURS System1_step32;
(*print_mv(matches rules System1_step32);*)
val System1_step34 = run rules TAC_RECEIVE System1_step33;
val System1_step35 = run rules TAC_RECEIVE System1_step34;
val System1_step36 = run rules TAC_RECEIVE System1_step35;
(*print_mv(matches rules System1_step35);*)
val System1_step37 = run rules TAC_NONCESIGNATURETEST System1_step36;
val System1_step38 = run rules TAC_NONCESIGNATURETEST System1_step37;
(*print_mv(matches rulesstarters System1_step38);*)
val System1_step39 = run rules TAC_RESEND System1_step38;


