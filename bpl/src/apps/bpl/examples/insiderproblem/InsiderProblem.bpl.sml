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
 
(* Modeling the Insider Problem/Threat
 *)

OS.FileSys.chDir "../..";
use "smlnj.sml";
Flags.setIntFlag "/debug/level" 10;
SMLofNJ.Internals.GC.messages false;
use_shorthands true;

(* Components controls *)
val Room            = active0 ("Room")
val Person          = active0 ("Person")
val CheckRights     = active0 ("CheckRights")
val Door            = passive ("Door" -: 1)
val Locks           = passive0 ("Locks")
val Key             = passive ("Key" -: 1)
val PC              = passive0 ("PC")
val Folder          = passive0 ("Folder")
val Data            = passive0 ("Data")
val CheckData       = passive0 ("CheckData")
val Lock            = atomic ("Lock" -: 1)
val Pin             = atomic0 ("Pin")
val Ruko            = atomic0 ("Ruko")
val Biometric       = atomic0 ("Biometric")
val Rights          = atomic ("Rights" -: 1)
val Network         = atomic ("Network" -: 1)
val Dot             = atomic ("Dot" -: 1)
val ERROR           = atomic0 ("ERROR")

val ( doorway1,  doorway2,  doorway3,  doorway4,  doorway, 
      keyset1,  keyset2,  keyset3,  keyset4,  keyset,
      rights1,  rights2,  rights3,  rights4,  rights,
      networklink)
  = ("doorway1","doorway2","doorway3","doorway4","doorway",  
     "keyset1","keyset2","keyset3","keyset4","keyset",
     "rights1","rights2","rights3","rights4","rights",
     "networklink")

(* Regel R1 *)
val PERSONMOVEUNLOCKEDDOOR = "PERSONMOVEUNLOCKEDDOOR" ::: 
 (-/doorway) o
 (Room o merge(3) o
    (idp(1) *
     Person o idp(1) *
     Door[doorway] o <->) ||
  Room o merge(2) o
    (idp(1) *
     Door[doorway] o idp(1)))
  --[0 |-> 0, 1 |-> 2, 2 |-> 1, 3 |-> 3]--|>
 (-/doorway) o
 (Room o merge(2) o
    (idp(1) *
     Door[doorway] o <->) ||
  Room o merge(3) o
    (idp(1) *
     Person o idp(1) *
     Door[doorway] o idp(1)))

(* Regel R2 *)
val PERSONMOVELOCKEDDOOR = "PERSONMOVELOCKEDDOOR" ::: 
 (-/doorway) o
 (Room o merge(3) o
    (idp(1) *
     Person o merge(2) o
       (idp(1) *
        Key[keyset1] o idp(1)) ||
     Door[doorway] o Lock[keyset1]) ||
  Room o merge(2) o
    (idp(1) *
     Door[doorway] o idp(1)))
  --[0 |-> 0, 1 |-> 3, 2 |-> 1, 3 |-> 2, 4 |-> 4]--|>
 (-/doorway) o
 (Room o merge(2) o
    (idp(1) *
     Door[doorway] o Lock[keyset1]) ||
  Room o merge(3) o
    (idp(1) *
     Person o merge(2) o
       (idp(1) *
        Key[keyset1] o idp(1)) *
     Door[doorway] o idp(1)))

(* Regel R3 *)
val PERSONTAKEKEY = "PERSONTAKEKEY" ::: 
   Person o idp(1) `|`
   Key[keyset] o idp(1)
  --[0 |-> 0, 1 |-> 1]--|>
   Person o merge(2) o
     (idp(1) *
      Key[keyset] o idp(1))

(* Regel R4 *)
val PERSONGIVESKEY = "PERSONGIVESKEY" ::: 
   Person o idp(1) `|`
   Person o merge(2) o
     (idp(1) *
      Key[keyset] o Ruko)
  --[0 |-> 0, 1 |-> 1]--|>
   Person o merge(2) o
     (idp(1) *
      Key[keyset] o Ruko) `|`
   Person o idp(1)

(* Regel R5 *)
val PERSONSHAREPIN = "PERSONSHAREPIN" ::: 
   Person o idp(1) `|`
   Person o merge(2) o
     (idp(1) *
      Key[keyset] o Pin)
  --[0 |-> 0, 1 |-> 1]--|>
   Person o merge(2) o
     (idp(1) *
      Key[keyset] o Pin) `|`
   Person o merge(2) o
     (idp(1) *
      Key[keyset] o Pin)

(* Regel R6 *)
val PERSONTAKESDATA = "PERSONTAKESDATA" ::: 
   Person o idp(1) `|`
   Data o idp(1)
  --[0 |-> 0, 1 |-> 1]--|>
   Person o merge(2) o
     (idp(1) *
      Data o idp(1))

(* Regel R7 *)
val PERSONCOPYDATANOLOCKS = "PERSONCOPYDATANOLOCKS" ::: 
   Person o idp(1) `|`
   PC o merge(3) o
     (idp(1) *
      Locks o <-> *
      Folder o merge(2) o
        (Locks o <-> *
         idp(1)))
  --[0 |-> 0, 1 |-> 2, 2 |-> 1, 3 |-> 2]--|>
   Person o merge(2) o
     (idp(1) *
      idp(1)) `|`
   PC o merge(3) o
     (idp(1) *
      Locks o <-> *
      Folder o merge(2) o
        (Locks o <-> *
         idp(1)))

(* Regel R8 *)
val PERSONCOPYDATABOTHLOCKED = "PERSONCOPYDATABOTHLOCKED" ::: 
   Person o merge(3) o
     (idp(1) *
      Key[keyset1] o idp(1) *
      Key[keyset2] o idp(1)) `|`
   PC o merge(3) o
     (idp(1) *
      Locks o Lock[keyset1] *
      Folder o merge(2) o
        (Locks o Lock[keyset2] *
         idp(1)))
  --[0 |-> 0, 1 |-> 4, 2 |-> 1, 3 |-> 2, 4 |-> 3, 5 |-> 4]--|>
   Person o merge(4) o
     (idp(1) *
      idp(1) *
      Key[keyset1] o idp(1) *
      Key[keyset2] o idp(1)) `|`
   PC o merge(3) o
     (idp(1) *
      Locks o Lock[keyset1] *
      Folder o merge(2) o
        (Locks o Lock[keyset2] *
         idp(1)))

(* Regel R9 *)
val PERSONCOPYNETWORKDATABOTHLOCKED = "PERSONCOPYNETWORKDATABOTHLOCKED" ::: 
   Person o merge(3) o
     (idp(1) *
      Key[keyset1] o idp(1) *
      Key[keyset2] o idp(1)) `|`
   PC o merge(3) o
     (idp(1) *
      Network[networklink] *
      Locks o Lock[keyset1]) `|`
   PC o merge(3) o
     (idp(1) *
      Network[networklink] *
      Folder o merge(2) o
        (Locks o Lock[keyset2] *
         idp(1)))
  --[0 |-> 0, 1 |-> 5, 2 |-> 1, 3 |-> 2, 4 |-> 3, 5 |-> 4, 6 |-> 5]--|>
   Person o merge(4) o
     (idp(1) *
      idp(1) *
      Key[keyset1] o idp(1) *
      Key[keyset2] o idp(1)) `|`
   PC o merge(3) o
     (idp(1) *
      Network[networklink] *
      Locks o Lock[keyset1]) `|`
   PC o merge(3) o
     (idp(1) *
      Network[networklink] *
      Folder o merge(2) o
        (Locks o Lock[keyset2] *
         idp(1)))

(* Regel R9d *)
val PERSONCOPYNETWORKDATASHAREDLOCKED = "PERSONCOPYNETWORKDATASHAREDLOCKED" ::: 
  (Person o merge(2) o
     (idp(1) *
      Key[keyset1] o idp(1)) `|`
   PC o merge(3) o
     (idp(1) *
      Network[networklink] *
      Locks o Lock[keyset1])) ||
   PC o merge(3) o
     (idp(1) *
      Network[networklink] *
      Folder o merge(2) o
        (Locks o Lock[keyset1] *
         idp(1)))
  --[0 |-> 0, 1 |-> 4, 2 |-> 1, 3 |-> 2, 4 |-> 3, 5 |-> 4]--|>
  (Person o merge(3) o
     (idp(1) *
      idp(1) *
      Key[keyset1] o idp(1)) `|`
   PC o merge(3) o
     (idp(1) *
      Network[networklink] *
      Locks o Lock[keyset1])) ||
   PC o merge(3) o
     (idp(1) *
      Network[networklink] *
      Folder o merge(2) o
        (Locks o Lock[keyset1] *
         idp(1)))

(* Regel R10 *)
val TESTDATAINIT = "TESTDATAINIT" ::: 
   Person o merge(3) o
     (idp(1) *
      Rights[rights] *
      Data o idp(1))
  --[0 |-> 0, 1 |-> 1]--|>
   Person o merge(2) o
     (idp(1) *
      CheckRights o merge(2) o
        (Rights[rights] *
         Data o idp(1)))

(* Regel R11 *)
val TESTDATASUCCESS = "TESTDATASUCCESS" ::: 
   Person o merge(3) o
     (idp(1) *
      CheckRights o merge(2) o
        (Rights[rights] ||
         Data o merge(2) o
           (idp(1) *
            Dot[rights])) *
      CheckData o idp(1)
     ) 
  --[0 |-> 0, 1 |-> 2]--|>
   Person o merge(3) o
     (idp(1) *
      Rights[rights] ||
      CheckData o merge(2) o 
        (idp(1) *
         Data)
     ) 

(* Regel R12 *)
val TESTDATAITTERATOR = "TESTDATAITTERATOR" ::: 
   CheckRights o merge(2) o
     (Rights[rights] *
      Data o merge(2) o
        (idp(1) *
         Dot[rights2])) ||
   Rights[rights2]
  --[0 |-> 0]--|>
   CheckRights o merge(2) o
     (Rights[rights] *
      Data o idp(1)) ||
   Rights[rights2]

(* Regel R13 *)
val TESTDATAFAILURE = "TESTDATAFAILURE" ::: 
   CheckRights o merge(2) o
     (Rights[rights] *
      Data o <->) `|`
   CheckData o idp(1)
  --[0 |-> 0]--|>
   Rights[rights] `|`
   CheckData o merge(2) o
     (idp(1) *
      Data o ERROR)

(* reaction rules *)
val rulelist = [PERSONMOVEUNLOCKEDDOOR,PERSONMOVELOCKEDDOOR,
		PERSONTAKEKEY, PERSONGIVESKEY, PERSONSHAREPIN,
                PERSONTAKESDATA, PERSONCOPYDATANOLOCKS,
		PERSONCOPYDATABOTHLOCKED,
		PERSONCOPYNETWORKDATABOTHLOCKED,
		PERSONCOPYNETWORKDATASHAREDLOCKED,
                TESTDATAINIT, TESTDATASUCCESS, 
                TESTDATAITTERATOR, TESTDATAFAILURE];

val rules = mkrules (rulelist);



(*EXAMPLE 1: UNSAFE SECURITY SETUP
 *The insider can get to the data because it is not protected correctly *)
val System0 = simplify (
  (-/doorway1 * -/doorway2 * -/doorway3) o
  (Room o merge(2) o
     (Door[doorway1] o <-> ||
      Door[doorway2] o Lock[keyset1]) `|`
   Room o merge(3) o
     (Door[doorway1] o <-> ||
      Door[doorway3] o Lock[keyset2] ||
      Data o <->) `|`
   Room o merge(3) o
     (Door[doorway2] o Lock[keyset1] ||
      Door[doorway3] o Lock[keyset3] ||
      Person o merge(3) o
        (CheckData o <-> || 
         Rights[rights1] ||
         Key[keyset1] o Ruko))
  )
) handle e => explain e;

(*print_mv(matches rules System0);*)
val TAC_PERSONMOVELOCKEDDOOR = react_rule "PERSONMOVELOCKEDDOOR";
val System0_step1 = run rules TAC_PERSONMOVELOCKEDDOOR System0;
(*print_mv(matches rules System0_step1);*)
val TAC_PERSONMOVEUNLOCKEDDOOR = react_rule "PERSONMOVEUNLOCKEDDOOR";
val System0_step2 = run rules TAC_PERSONMOVEUNLOCKEDDOOR System0_step1;
(*print_mv(matches rules System0_step2);*)
val TAC_PERSONTAKESDATA = react_rule "PERSONTAKESDATA";
val System0_step3 = run rules TAC_PERSONTAKESDATA System0_step2;
(*print_mv(matches rules System0_step3);*)
val TAC_TESTDATAINIT = react_rule "TESTDATAINIT";
val System0_step4 = run rules TAC_TESTDATAINIT System0_step3;
(*print_mv(matches rules System0_step4);*)
val TAC_TESTDATAFAILURE = react_rule "TESTDATAFAILURE";
val System0_step5 = run rules TAC_TESTDATAFAILURE System0_step4;


(*
(*EXAMPLE 2: UNPROTECTED ACCESSKEY
 *The insider has access to a room, where another key is stored unprotected *)
val System1 = simplify (
  (-/doorway1 * -/doorway2) o
  (Room o merge(2) o
     (Door[doorway1] o Lock[keyset1] ||
      Key[keyset2] o Ruko) `|`
   Room o merge(2) o
     (Door[doorway2] o Lock[keyset2] ||
      Data o <->) `|`
   Room o merge(3) o
     (Door[doorway1] o Lock[keyset1] ||
      Door[doorway2] o Lock[keyset2] ||
      Person o merge(3) o
        (CheckData o <-> || 
         Rights[rights1] ||
         Key[keyset1] o Ruko))
  )
) handle e => explain e;

(*print_mv(matches rules System1);*)
val TAC_PERSONMOVELOCKEDDOOR = react_rule "PERSONMOVELOCKEDDOOR";
val System1_step1 = run rules TAC_PERSONMOVELOCKEDDOOR System1;
(*print_mv(matches rules System1_step1);*)
val TAC_PERSONTAKEKEY = react_rule "PERSONTAKEKEY";
val System1_step2 = run rules TAC_PERSONTAKEKEY System1_step1;
(*print_mv(matches rules System1_step2);*)
val System1_step3 = run rules TAC_PERSONMOVELOCKEDDOOR System1_step2;
val mz = matches rules System1_step3;
(*print_mv(mz);*)
val System1_step4 = simplify(react (lzhd (lztl mz)));
(*print_mv(matches rules System1_step4);*)
val TAC_PERSONTAKESDATA = react_rule "PERSONTAKESDATA";
val System1_step5 = run rules TAC_PERSONTAKESDATA System1_step4;
(*print_mv(matches rules System1_step5);*)
val TAC_TESTDATAINIT = react_rule "TESTDATAINIT";
val System1_step6 = run rules TAC_TESTDATAINIT System1_step5;
(*print_mv(matches rules System1_step6);*)
val TAC_TESTDATAFAILURE = react_rule "TESTDATAFAILURE";
val System1_step7 = run rules TAC_TESTDATAFAILURE System1_step6;
*)

(*
(*EXAMPLE 3: UNTRUSTWORTHY COWORKERS
 *The insider receives keys from other workers, and use them to access a 
 *room with a PC, and retrieves the illegal data over the network from the PC *)
val System2 = simplify (
  (-/doorway1 * -/doorway2 * -/doorway3) o
  (Room o merge(3) o
     (Door[doorway1] o <-> ||
      Door[doorway2] o Lock[keyset1] ||
      Person o merge(3) o
        (Rights[rights1] *
         CheckData o <-> *
         Key[keyset1] o Ruko)) `|`
   Room o merge(2) o
     (Door[doorway2] o Lock[keyset1] ||
      PC o merge(2) o
        (Locks o Lock[keyset2] *
         Network[networklink])) `|`
   Room o merge(2) o 
     (PC o merge(2) o
        (Network[networklink] *
         Folder o merge(2) o
           (Locks o Lock[keyset2] *
            Data o Dot[rights2])) *
      Door[doorway3] o Lock[keyset3]) `|`
   Room o merge(4) o
     (Door[doorway1] o <-> (*Lock[keyset1] cutpaste error*) ||
      Door[doorway3] o Lock[keyset4] ||
      Person o merge(3) o
        (CheckData o <-> || 
         Rights[rights2] ||
         Key[keyset2] o Pin) ||
      Person o merge(2) o
        (CheckData o <-> || 
         Rights[rights3])
     )
  )
) handle e => explain e;

(*print_mv(matches rules System2);*)
val TAC_PERSONSHAREPIN = react_rule "PERSONSHAREPIN";
val System2_step1 = run rules TAC_PERSONSHAREPIN System2;
val mz = matches rules System2_step1;
(*print_mv(mz);*)
val System2_step2 = simplify(react (lzhd (lztl (lztl (lztl (lztl (lztl (lztl mz))))))));
(*print_mv(matches rules System2_step2);*)
val TAC_PERSONGIVESKEY = react_rule "PERSONGIVESKEY";
val System2_step3 = run rules TAC_PERSONGIVESKEY System2_step2;
(*print_mv(matches rules System2_step3);*)
val TAC_PERSONMOVELOCKEDDOOR = react_rule "PERSONMOVELOCKEDDOOR";
val System2_step4 = run rules TAC_PERSONMOVELOCKEDDOOR System2_step3;
(*print_mv(matches rules System2_step4);*)
val TAC_PERSONCOPYNETWORKDATASHAREDLOCKED = react_rule "PERSONCOPYNETWORKDATASHAREDLOCKED";
val System2_step5 = run rules TAC_PERSONCOPYNETWORKDATASHAREDLOCKED System2_step4;
(*print_mv(matches rules System2_step5);*) (*DUPLICATES REMOVED*)
val TAC_TESTDATAINIT = react_rule "TESTDATAINIT";
val System2_step6 = run rules TAC_TESTDATAINIT System2_step5;
(*print_mv(matches rules System2_step6);*) (*DUPLICATES REMOVED*)
val TAC_TESTDATAITTERATOR = react_rule "TESTDATAITTERATOR";
val System2_step7 = run rules TAC_TESTDATAITTERATOR System2_step6;
(*print_mv(matches rules System2_step7);*) (*DUPLICATES REMOVED*)
val TAC_TESTDATAFAILURE = react_rule "TESTDATAFAILURE";
val System2_step8 = run rules TAC_TESTDATAFAILURE System2_step7;
*)
