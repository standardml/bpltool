(*

  Ebbe Elsborg and Henning Niss

  14/10/2005: Created
   8/11/2005: Slight revision
  17/11/2005: Introduced state and interface to S (s.bpl) and A (a.sml)
  25/11/2005: Minor code changes and expanded interface to S
  16/05/2006: Removed mutual recursion - need to interface with miniml
	      tool, bplgen etc.
  18/05/2006: Rewrote some functions to take only one parameter,
	      explicit string equality,
              rewrote local statement to use 'let val' instead.
  25/07/2006: Introduced 'dev' as data type,
              rewrote functions to take one one parameter,
	      rewrote some functions to return option type,
              embedded 'model = ref(...)' in 'val funs = ...',
	      exported 'funs',
              changed building example,
              changed tests.
  26/07/2006: Introduced parent map (is that cheating?),
	      range query,
	      'lid' as data type,
	      navigation query.
  02/08/2006: Removed 'map',
	      corrected a comment.
  27/10/2006: Introduced 'event' as data type,
	      introduced event loop,
	      introduced a queue with enqueue and dequeue functions,
	      redefined 'sobs', 'slost', and 'state'.
	      COMMENT: Do we also need to export aux. function names?
  30/10/2006: Replaced ~1 with options.
  02/02/2007: Introduced spinlock on queue.
  05/02/2007: Introduced WhereIs event,
              removed some dead code.
  09/02/2007: Changed sobs and slost to not introduce new devices,
	      introduced allDevs variable,
	      changed type of 'inactive' from dev list to hierarchy,
	      ported functions to use new state and export format,
	      tested interfaces to S and A.
 
  This is the "location model" part L of a Plato-graphical system;
  C || P || A = C || (S || L) || A.

  The code is kept as simple (not optimised for performance) as possible
  because the bigraphical image tends to explode sizewise reverting the
  effect of complicated optimisations.

*)

(* export enqL from *)

(* consider making these two an abstract type 'Link' *)
type lid = int
type dev = int

datatype res = Res of lid | EmptyRes of string | ListRes of lid list
datatype enqa = enqA of res

datatype event = Obs of dev * lid
	       | Loss of dev
	       | WhereIs of dev * (lid -> enqa)
	       | FindAll of lid * (lid list -> enqa)

datatype hierarchy = (* id, devices, sublocations *)
	 Loc of lid * dev list * hierarchy list

(* remove an element from a list *)
fun del_list e =
    fn [] => []
     | (x::xs) => if e=x then del_list e xs else x :: del_list e xs

(* test whether element 'e' is in list 'l' *)
fun listmember e =
    fn l => case l
	     of [] => false
	      | (x::xs) => if e=x then true else listmember e xs

(* delete device 'dev' from hierarchy 'id' *)
fun delete dev =
    fn (Loc(id,ds,ls)) =>
       Loc(id, del_list dev ds, map (delete dev) ls)

(* insert device 'dev' into location 'lname' in hierarchy 'id' *)
fun insert dev =
    fn lname =>
       fn (Loc(id,ds,ls)) =>
	  if lname=id then Loc(id, dev::ds, map (insert dev lname) ls)
	  else Loc(id, ds, map (insert dev lname) ls)

(* find all devices in a hierarchy - depth first *)
fun fall (Loc(_,ds,[])) = ds
  | fall (Loc(_,ds,l::ls)) =
    let fun fall' [] = []
	  | fall' (loc::locs) = (fall loc) @ (fall' locs)
    in ds @ (fall l) @ (fall' ls) end

(* pick the subtree with id 'loc' from a hierarchy *)
fun pickloc lname =
    fn (Loc(id,ds,ls)) =>
       if lname=id then SOME(Loc(id,ds,ls))
       else let fun pickloc' lname [] = NONE
		  | pickloc' lname (loc::locs) =
		    case pickloc lname loc
		     of SOME(l) => SOME(l)
		      | NONE => pickloc' lname locs
	    in pickloc' lname ls end

(* unpack option, return list of devices *)
fun flocs option =
    case option of NONE => []
		 | SOME(l) => fall l

(* find the identifier of a device's location - depth first *)
fun whr dev =
    fn l =>
       case l
	of (Loc(_,[],[])) => NONE
	 | (Loc(id,d::ds,ls)) => if dev=d then SOME(id)
				 else whr dev (Loc(id,ds,ls))
	 | (Loc(_,[],ls)) => 
	   let fun whr' dev =
		   fn list =>
		      case list
		       of [] => NONE
			| (loc::locs) =>
			  case whr dev loc
			   of SOME(i) => SOME(i)
			    | NONE => whr' dev locs
	   in whr' dev ls end

(* Building, initially *)
val state = (Loc(1,[15],
		 [Loc(2,[10,11],[]),
		  Loc(3,[],[]),
		  Loc(4,[],
		      [Loc(5,[12],[]),
		       Loc(6,[],
			   [Loc(7,[],
				[Loc(8,[13],[]),
				 Loc(9,[14],[])])])])]) ,
	     Loc(~1,[16,17,18],[]))

val allDevs = fall(#1(state)) @ fall(#2(state))

(* this map must correspond exactly to the hierarchy 'state' below *)
val prntmap =
    [(1,1),(2,1),(3,1),(4,1),(5,4),(6,4),(7,6),(8,7),(9,7)]

(* find value of key 'k' in association list 'l' (e.g. prntmap) *)
fun assoc l =
    fn k => case l
	     of [] => []
	      | ((x,y)::pairs) => if k=x then [y] else assoc pairs k

(* climb prntmap 'i' steps *)
fun prnt i =
    fn lid =>
       if i<=0 then lid else prnt (i-1) (hd (assoc prntmap lid))

(* find devices in tree-specified range of 'd' excl. 'd' itself *)
fun findinrange d =
    fn h =>
       fn i => case whr d h
		of NONE => NONE
		 | SOME(l) =>
		   let val area = prnt i l
		       val devsinrange = flocs (pickloc area h)
		   in SOME(del_list d devsinrange)
		   end

(* find the root of the parent map *)
fun findroot map =
    case map of [] => NONE
	      | ((x,y)::pairs) => if x=y then SOME(x) else findroot pairs

(* find a path (list) from loc. 'lid' to its /ancestor/ loc. 'ancid' *)
fun ancpath lid =
    fn ancid =>
       fn a => let val prnt1 = prnt 1 lid in
		   if lid=ancid then a
		   else ancpath prnt1 ancid (prnt1::a)
	       end

(* find the nearest common ancestor of two locations *)
fun commonanc p1 =
    fn p2 =>
       case p1
	of [] => NONE
	 | (x::xs) => case p2
		       of [] => NONE
			| (y::ys) => if listmember x (y::ys)
				     then SOME(x)
				     else commonanc xs (y::ys)

(* find a path from location 'lid1' to location 'lid2' *)
(*
fun findpath lid1 =
    fn lid2 => let val root = findroot prntmap
		   val path1 = rev (ancpath lid1 root [lid1])
		   val path2 = rev (ancpath lid2 root [lid2])
		   val nearestanc = commonanc path1 path2
		   val path1' = rev (ancpath lid1 nearestanc [])
		   val path2' = tl(ancpath lid2 nearestanc [lid2])
	       in path1' @ path2' end
*)

(*ARGH...!!! Ballade med SOME(5) vs. 5 af lokationsværdier...*)
fun findpath l1 =
    fn lid2 =>
       case l1
	of NONE => NONE
	 | SOME(lid1) =>
	   case findroot prntmap
	    of NONE => NONE
	     | SOME(r) =>
	       let val path1 = rev (ancpath lid1 r [lid1])
		   val path2 = rev (ancpath lid2 r [lid2])
		   val nearestanc = commonanc path1 path2
	       in case nearestanc
		   of NONE => NONE
		    | SOME(l) =>
		      let val path1' = rev (ancpath lid1 l [])
			  val path2' = tl(ancpath lid2 l [lid2])
		      in SOME(path1' @ path2') end
	       end

(* Spinlock *)
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

(* Event queue with operations *)
val queue = ref []

fun deq () =
    ( spinlock lock;
      (case (!queue) of [] => NONE
		      | (q::qs) => let val _ = queue:=qs 
				   in SOME(q)
				   end)
      before
      spinunlock lock )

fun enqL e = (* THE interface function *)
    ( spinlock lock;
      queue:=(!queue)@[e];
      spinunlock lock )

(* handler functions used by event loop *)
fun sobs s =
    fn d =>
       fn l =>
	  if listmember d allDevs
	  then let val active' = delete d (#1(s))
		   val inactive = delete d (#2(s))
		   val active = insert d l active'
	       in (active,inactive) end
	  else s

fun slost s =
    fn d =>
       if listmember d allDevs
       then let val active = delete d (#1(s))
		val inactive' = delete d (#2(s))
		val inactive = insert d ~1 inactive'
	    in (active,inactive) end
       else s

fun awhere s =
    fn d =>
       fn f =>
	  case whr d (#1(s))
	   of SOME(l) => f l
	    | NONE => case whr d (#2(s))
		       of SOME(l) => f l
			| NONE => f ~1

fun afindall s =
    fn l =>
       fn f => case pickloc l (#1(s))
		of NONE => f (fall (#2(s)))
		 | SOME(h) => f (fall h)

(* Event loop *)
fun loop state =
    case deq () of
	NONE => ( wait(100); loop state )
      | SOME(Obs(d,l)) => loop (sobs state d l)
      | SOME(Loss(d)) => loop (slost state d)
      | SOME(WhereIs(d,f)) => ( awhere state d f; loop state )
      | SOME(FindAll(l,f)) => ( afindall state l f; loop state )

(* OLD CODE BEGIN
val funs =
    (* initial configuration *)
    let val state =
	    ref(Loc(1,[15],
		    [Loc(2,[10,11],[]),
		     Loc(3,[],[]),
		     Loc(4,[],
			 [Loc(5,[12],[]),
			  Loc(6,[],
			      [Loc(7,[],
				   [Loc(8,[13],[]),
				    Loc(9,[14],[])])])])]))
	val devs = ref []
	(* interface to S; reconfigurations *)
	fun sobs' d =
	    fn l =>
	       let val state' = delete d (!state)
		   val devs' = del_list d (!devs)
		   val state'' = insert d l (!state)
	       in state:=state''; devs:=devs' end
	fun slost' d =
	    let val state' = delete d (!state)
		val devs' = del_list d (!devs)
		val devs'' = d::devs'
	    in state:=state'; devs:=devs'' end
(*
	fun smove d =
	    fn l =>
	       let val state' = move d l (!state) in state:=state' end
*)
	(* interface to A; queries *)
	fun awher d = whr d (!state) (* where'' d (!state) *)
	(* fun afind lname = flocs (pickloc lname (!state)) *)
	fun afind lname = case (pickloc lname (!state)) of
			      NONE => NONE
			    | SOME(h) => SOME(fall h)
	fun arange d i = findinrange d (!state) i
	(* fun anavig d l = findpath (where'' d (!state)) l *)
	fun anavig d l = findpath (whr d (!state)) l
    (* find nearest neighbour query *)
    in (state,devs,sobs',slost',awher,afind,arange,anavig) end
OLD CODE END *)

(***** Tests *****)

(* testing queue operations *)
(*
val s0 = state;
val q0 = !queue;
val e1 = enqL(Obs(15,6));
val q1 = !queue;
val e2 = enqL(Loss(12));
val q2 = !queue;
val e3 = enqL(WhereIs(15,fn r => enqA(Res r)))
val q3 = !queue;
val o4 = deq();
val q4 = !queue;
val o5 = deq();
val q5 = !queue;
val o6 = deq();
val q6 = !queue
*)

(* testing interface to S *)
val s0:hierarchy = #1(state)
val d0:hierarchy = #2(state)
val (s1,d1) = slost (s0,d0) 14
val (s2,d2) = sobs (s1,d1) 14 4
val (s3,d3) = slost (s2,d2) 20
val (s4,d4) = sobs (s3,d3) 42 20

(* testing interface to A *)
val whereA = fn x => if x = ~1 then EmptyRes("nowhere") else Res(x)
val w0 = awhere (s1,d1) 14 whereA
val w1 = awhere (s4,d4) 12 whereA
val w2 = awhere (s4,d4) 20 whereA

val findallA = fn x => ListRes(x)
val f0 = afindall (s4,d4) 2 findallA
val f1 = afindall (s4,d4) 20 findallA
(*
val a_range_d13_prnt3 = fun5 13 3
val a_navig_d13_l2 = fun6 13 2
val a_navig_d14_l7 = fun6 14 7
*)