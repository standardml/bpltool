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

fun new () = ref False

(* just for typechecking -- remove later *)
fun exchange (r,s) =
    let val tmp = !r
    in r := (!s) ; s := tmp
    end

(* just for typechecking -- remove later *)
fun enqL x = ()

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
    case (!queue) of [] => NONE
		   | (q::qs) => let val _ = queue:=qs 
				in SOME(q)
				end

fun enqA e = queue:=(!queue)@[e]

fun whereIs d =
    enqL(WhereIs(d,fn r => enqA(Res r)));
    wait(100)