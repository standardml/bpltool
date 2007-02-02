(************************************************************************
  Ebbe Elsborg and Henning Niss, 2/2/2007
 
  This is the "location-aware application" part A of a Plato-graphical
  system; C || P || A = C || (S || L) || A.
************************************************************************)

datatype bool = True | False
datatype loc = Loc of int
datatype dev = Dev of int
datatype res = Res of loc
datatype whereIs = WhereIs of dev * (loc -> unit)

(* just for typechecking -- remove later *)
fun exchange (r,s) =
    let val tmp = !r
    in r:=(!s) ; s:=tmp
    end

(* just for typechecking -- remove later *)
fun enqL x = ()

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
    if !lock = True then wait(100)
    else ( spinlock lock;
	   case (!queue) of [] => NONE
			  | (q::qs) => let val _ = queue:=qs 
				       in SOME(q)
				       end;
	   spinunlock lock
	   )

fun enqA e =
    if !lock = True then wait(100)
    else ( spinlock lock;
	   queue:=(!queue)@[e];
	   spinunlock lock
	   )

fun whereIs d = enqL(WhereIs(d,fn r => enqA(Res r)))