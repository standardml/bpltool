


(*--------------------------------------------------------------------------*)
(* Structure: UtilTime                                                      *)
(*                                                                          *)
(* Depends on:                                                              *)
(*                                                                          *)
(* Exceptions raised by functions in this structure:                        *)
(*   time  : none                                                           *)
(*   timeN : none                                                           *)
(*--------------------------------------------------------------------------*)
signature UtilTime =
   sig
      val time  : ('a -> 'b) -> 'a -> 'b * {usr:Time.time, sys:Time.time, gc:Time.time} 
      val timeN : int -> ('a -> 'b) -> 'a -> 'b * {usr:Time.time, sys:Time.time, gc:Time.time} 
   end

structure UtilTime : UtilTime =
   struct
      (*--------------------------------------------------------------------*)
      (* run f on x, and measure the runtime. return the result and time.   *)
      (*--------------------------------------------------------------------*)
      fun time f x = let val timer = Timer.startCPUTimer ()
			 val y     = f x
			 val {usr, sys} = Timer.checkCPUTimer timer
			 val gc = Timer.checkGCTime timer
		     in (y,{usr = usr, sys = sys, gc = gc})
		     end
		  
      (*--------------------------------------------------------------------*)
      (* run f n times on x, and measure the runtime. return the time.      *)
      (*--------------------------------------------------------------------*)
      fun timeN n f x = 
	 let fun iter m = if m<=1 then f x else (ignore (f x); iter (m-1))
	 in time iter n
	 end
    end