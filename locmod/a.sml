(************************************************************************
  Ebbe Elsborg and Henning Niss, 5/2/2007
 
  This is the "location-aware application" part A of a Plato-graphical
  system; C || P || A = C || (S || L) || A.
************************************************************************)

datatype bool = True | False
type loc = int
type dev = int
datatype res = Res of loc
datatype whereis = WhereIs of dev * (loc -> unit)
datatype enql = enqL of whereis

(* just for typechecking -- remove later *)
fun exchange (r,s) =
    let val tmp = !r
    in r:=(!s) ; s:=tmp
    end

fun new () = ref False

val lock = new ()

fun spinlock l =
    let val t = ref True
        fun loop () = ( exchange(t,l); if !t = True then loop() else () )
    in loop ()
    end

fun spinunlock l = 
    let val t = ref False
    in exchange(t,l)
    end

fun wait i = if i<0 then () else wait(i-1)

val queue = ref []

fun deq () =
    ( spinlock lock;
      case (!queue) of [] => NONE
		     | (q::qs) => let val _ = queue:=qs 
				  in SOME(q)
				  end;
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