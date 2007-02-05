(************************************************************************
  Ebbe Elsborg and Henning Niss, 5/2/2007
 
  This is the "location-aware application" part A of a Plato-graphical
  system; C || P || A = C || (S || L) || A.
************************************************************************)

type lid = int
type dev = int
datatype res = Res of lid
datatype whereis = WhereIs of dev * (lid -> unit)
datatype enql = enqL of whereis

fun exchange (r,s) = (* just for typechecking -- remove later *)
    let val tmp = !r
    in r:=(!s) ; s:=tmp
    end

fun new () = ref false

val lock = new ()

fun spinlock l =
    let val t = ref true
        fun loop () = ( exchange(t,l); if !t then loop() else () )
    in loop ()
    end

fun spinunlock l = 
    let val t = ref false
    in exchange(t,l)
    end

fun wait i = if i<0 then () else wait(i-1)

val queue = ref []

fun deq () =
    ( spinlock lock;
      (case (!queue) of [] => NONE
		      | (q::qs) => let val _ = queue:=qs 
				   in SOME(q)
				   end)
      before
      spinunlock lock )

fun enqA e =
    ( spinlock lock;
      queue:=(!queue)@[e];
      spinunlock lock )

fun getRes () =
    ( spinlock lock;
      (case (!queue) of [] => ( wait(100); getRes() )
		     | (q::qs) => q)
      before
      spinunlock lock )

fun whereIs d =
    ( enqL(WhereIs(d,fn r => enqA(Res r)));
      getRes() )