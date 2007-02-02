(************************************************************************
  Ebbe Elsborg and Henning Niss
  18/12/2005: 
 
  This is the "location-based service" part A of a Plato-graphical system;
  C || P || A = C || (S || L) || A.
************************************************************************)

fun isCoffee dev = ...
fun parent   loc = ...

fun filter args =
  let val p = fst args
      fun loop l =
	case l of
	  []    => []
        | x::xs => if p x then x :: loop xs else loop xs
  in  loop (snd args)
  end

fun service' loc =
  (* very stupid, repeatedly looks through the same locations *)
  let val all = aFindall loc
      val coffee = filter (isCoffee, all)
  in  case coffee of
         []    => service' (parent loc)
       | x::xs => coffee
  end

fun service me =
  let val my_loc = whr me
  in  service' my_loc
  end