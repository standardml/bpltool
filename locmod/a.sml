(************************************************************************
  Ebbe Elsborg and Henning Niss, 10/4/2007
 
  This is the "location-aware application" part A of a Plato-graphical
  system; C || P || A = C || (S || L) || A.
************************************************************************)

(* export enqA,queueA from *)

type lid = int
type dev = int
datatype res = Res of lid
datatype whereis = WhereIs of dev * (lid -> unit)
datatype enql = enqL of whereis

(* just for SML typechecking *)
fun exchange r =
	fn s => let val tmp = !r in r:=(!s) ; s:=tmp end

val lockA = ref false

fun spinlockA l =
    let val t = ref true
        fun loop () = ( exchange t l ; if !t then loop() else () )
    in loop ()
    end

fun spinunlockA l = 
    let val t = ref false
    in exchange t l
    end

fun waitA i = if i<=0 then () else waitA(i-1)

val queueA = ref []

fun deq () =
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
    ( spinlockA lockA;
      (case (!queueA) of [] => ( waitA(100); getRes() )
 	              | (q::qs) => q)
      before
      spinunlockA lockA )

fun whereIs d =
    ( enqL(WhereIs(d,fn r => enqA(Res r)));
      getRes() )