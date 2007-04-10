(************************************************************************
  Ebbe Elsborg and Henning Niss, 10/4/2007
 
  This is the "location-aware application" part A of a Plato-graphical
  system; C || P || A = C || (S || L) || A.
************************************************************************)

(* export enqA from *)

type lid = int
type dev = int
datatype res = Res of lid
datatype whereis = WhereIs of dev * (lid -> unit)
datatype enql = enqL of whereis

(* just for SML typechecking *)
fun exchange r =
	fn s => let val tmp = !r in r:=(!s) ; s:=tmp end

val lock = ref false

fun spinlock l =
    let val t = ref true
        fun loop () = ( exchange t l ; if !t then loop() else () )
    in loop ()
    end

fun spinunlock l = 
    let val t = ref false
    in exchange t l
    end

fun wait i = if i<=0 then () else wait(i-1)

val queueA = ref []

fun deq () =
    ( spinlock lock;
      (case (!queueA) of [] => NONE
		      | (q::qs) => let val _ = queueA:=qs 
				   in SOME(q)
				   end)
      before
      spinunlock lock )

fun enqA e =
    ( spinlock lock;
      queueA := (!queueA)@[e];
      spinunlock lock )

fun getRes () =
    ( spinlock lock;
      (case (!queueA) of [] => ( wait(100); getRes() )
 	              | (q::qs) => q)
      before
      spinunlock lock )

fun whereIs d =
    ( enqL(WhereIs(d,fn r => enqA(Res r)));
      getRes() )