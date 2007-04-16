(************************************************************************
  Ebbe Elsborg and Henning Niss, 11/4/2007
 
  This is the "location-aware application" part A of a Plato-graphical
  system; C || P || A = C || (S || L) || A.
************************************************************************)

(* export spinlockA, spinunlockA, lockA, waitA, queueA from *)
(* and also the functions on lists used by L... *)

type lid = int
type dev = int
datatype res = Res of lid
datatype whereis = WhereIs of dev * (lid -> unit)
datatype enql = enqL of whereis

(* just for SML typechecking *)
fun exchange (r,s) = let val tmp = !r in r:=(!s) ; s:=tmp end

val lockA = ref false

fun spinlockA l =
    let val t = ref true
        fun loop () = ( exchange(t,l) ; if !t then loop() else () )
    in loop ()
    end

fun spinunlockA l = 
    let val t = ref false
    in exchange (t,l)
    end

fun waitA i = if i<=0 then () else waitA(i-1)

val queueA = ref []

fun deqA () =
    ( spinlockA lockA;
      (case (!queueA) of [] => NONE
		      | (q::qs) => let val _ = queueA:=qs
				   in SOME(q)
				   end)
      before
      spinunlockA lockA )

fun enqA e =
    ( spinlockA lockA;
      queueA := (!queueA)@[e];
      spinunlockA lockA )

fun getRes () =
    case deqA () of
	NONE => ( waitA(100); getRes () )
      | SOME(Res(l)) => l

fun whereIs d =
    ( enqL(WhereIs(d,fn r => enqA(Res r)));
      getRes () )