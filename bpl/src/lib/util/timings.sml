(* Copyright (c) 2006  The BPL Group at the IT University of Copenhagen
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

structure Timings :> TIMINGS = struct

    val do_timings = 
	Flags.makeBoolFlag{name="/misc/timings",arg="",
			   short="",long="timings",default=true,
			   desc="Enable timings"}

    (* timers *)
(* 
    (* for now use real timers - straightforward to change to cpu *)
    type timer = Time.time
    fun timer () = ref Time.zeroTime
    val startTimer = Timer.startRealTimer
    val checkTimer = Timer.checkRealTimer
    fun addTimerResult timer spent =
	timer := Time.+(!timer, spent)
    fun showTimer timer = Time.toString(!timer) ^ " secs"
*)
    type timer = {sys: Time.time, usr: Time.time}
    fun timer () = ref {sys=Time.zeroTime, usr=Time.zeroTime}
    val startTimer = Timer.startCPUTimer
    val checkTimer = Timer.checkCPUTimer
    fun addTimerResult timer {sys,usr} =
	let val {sys=s,usr=u} = !timer
	in  timer := {sys=Time.+(s,sys),usr=Time.+(u,usr)}
	end
    fun showTimer (ref {sys,usr}) =
	let val s = Time.toString
	in  String.concat["user: ",s usr,"s, system: ",s sys,"s"]
	end

    structure HT 
      = HashTableFn(type hash_key = string
                    val hashVal = HashString.hashString
                    val sameKey = op =)

    exception FlagNotFound
    val timers : ({desc:string,timer:timer ref}) HT.hash_table
     = HT.mkTable (37, FlagNotFound)

    fun time (t : timer ref) (f : unit -> 'a) : 'a =
	if !do_timings then 
	    let val rt = startTimer()
		val r = f ()
		val spent = checkTimer rt
		val _ = addTimerResult t spent
	    in  r
	    end
	else f ()
	
    fun add {name:string,desc:string} =
	let val t = timer ()
	in  HT.insert timers (name, {desc=desc,timer=t})
	  ; time t
	end

    fun list0 p =
	let val ts = List.filter (p o #1) (HT.listItemsi timers)
	    val width = List.foldl (fn ((_,{desc,timer}),m) => 
				       if size desc > m then size desc else m) 
				   0 ts
	    fun toString (name,{desc,timer}) = 
		String.concat[ StringCvt.padRight #" " width desc,": "
			     , showTimer timer]
	in  List.map toString ts
	end

    structure NameSet
      = OrderSet(type T = string val lt = fn (s:string) => fn s' => s < s')

    fun list names =
	let val ns = NameSet.fromList names
	in  list0 (fn n => NameSet.member n ns)
	end

    fun listAll () = list0 (fn _ => true)

end (* structure Timings *)

