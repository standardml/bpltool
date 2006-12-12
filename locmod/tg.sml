(*
 Implementation of the context-aware tourist guide GUIDE;
 see the MobiCom 2000 paper "Experiences of Developing and Deploying
 a Context-Aware Tourist Guide: The GUIDE Project" by K. Cheverst,
 N. Davies, K. Mitchell, and A. Friday.

 Attractions are from the virtual tour at
 http://www.lancasterukonline.net/visitors/v-tour/index.htm.

 A demo of the "real" application can be found at
 http://www.guide.lancs.ac.uk/whatisguide.html.

 Ebbe Elsborg, December 2006
*)

(* use "l.sml"; *)
open TextIO;
open List;

(* Basic entities *)
datatype attraction =
	 Att of string (* name *) * string (* info *) * string (* more info *)

val voidLoc = "voidLoc"
val voidAtt = Att("voidAtt","","")
fun deattract (Att tup) = tup

type link = string
type device = link
type location = link
type display = string
type btnid = string
type btnval = string
type message = string
type tourflag = bool
type path = (location * attraction) list
type group = device list

type state = display * location * attraction list * message list 
	     * (btnid * btnval) list * tourflag * path * location
	     * attraction

datatype event =
	 DeviceObserved of device * location
       | ButtonClicked of btnid

type Queue = event list
type Stack = state list

(* Attractions *)
val castle = Att("Lancaster Castle","info","more-info")
val williamson = Att("Williamson Park and the Ashton Memorial","info","more-info")
val queenvic = Att("Queen Victoria monument","info","more-info")
val seagull = Att("Saltayre Seagull Colony","info","more-info")
val halfmoon = Att("Half Moon Bay","info","more-info")
val market = Att("Farmers Street Market","info","more-info")
val canal = Att("Lancaster Canal","info","more-info")
val nightingale = Att("Lancashire Witches","info","more-info")
val spooky = Att("Spooky Paths","info","more-info")
val ruxton = Att("Dr. Buck Ruxton's House","info","more-info")
val priory = Att("Lancaster Priory","info","more-info")
val merchants = Att("J Atkinson & Co, Tea & Coffee Merchants","info","more-info")
val humbug = Att("Humbugs Sweetshop","info","more-info")
val cemetery = Att("Lancaster Cemetery","info","more-info")
val museum = Att("City Museum and The King's Own Museum","info","more-info")
val cottage = Att("Cottage Museum + Roman Bath-House","info","more-info")
val meetinghouse = Att("Friends Meeting House","info","more-info")
val theatre = Att("The Grand Theatre","info","more-info")
val judges = Att("Judges Lodgings + Doll Museum","info","more-info")
val maritime = Att("Maritime Museum","info","more-info")
val leisure = Att("Lancaster Leisure Park","info","more-info")
val millenium = Att("Lancaster Millenium Bridge and River Lune Millennium Park","info","more-info")
val music = Att("The Music Room","info","more-info")
val stpeters = Att("St Peters RC Cathedral","info","more-info")
val townhall = Att("Lancaster Town Hall","info","more-info")

(* Popular attractions and location-attraction relationship *)
val popular : attraction list = [castle,williamson,queenvic,seagull,halfmoon,market,canal,nightingale,spooky,ruxton,priory]

val locAtts : (location * attraction list) list =
    [("TIC", []),
     ("l1", [castle,Att("'dummy'","info","moreinfo")]),
     ("l2", [williamson]),
     ("l3", [queenvic]),
     ("l4", [seagull]),
     ("l5", [halfmoon]),
     ("l6", [market]),
     ("l7", [canal]),
     ("l8", [nightingale]),
     ("l9", [spooky]),
     ("l10", [ruxton]),
     ("l11", [priory])]

(* Constants *)
val our_id : device ref = ref "ab:cd:ef:gh:ij:kl"
val our_grp : group ref = ref ["d1","d2","d3"]

(* Auxiliary functions *)
fun app f [] = ()
  | app f (x::xs) = ( f x ; app f xs )

fun exists p [] = false
  | exists p (x::xs) = p x orelse exists p xs

fun filter p [] = []
  | filter p (x::xs) = if p x then x :: filter p xs else filter p xs

fun last x = (hd o rev) x

fun lookup1 map x =
    let fun loop [] = NONE
	  | loop ((l,a)::m) = if l = x then SOME l else loop m
    in loop map end

fun lookup2 map x =
    let fun loop [] = NONE
	  | loop ((l,a)::m) = if l = x then SOME a else loop m
    in loop map end

fun findLoc map a =
    let fun loop [] = NONE
	  | loop ((l,a')::m) =
	    if exists (fn x => a = x) a' then SOME l else loop m
    in loop map end

fun first [] = []
  | first ((l,a)::m) = l :: first m

fun second [] = []
  | second ((l,a)::m) = a @ second m

fun remDubs [] = []
  | remDubs (x::xs) =
    if exists (fn y => y = x) xs then remDubs xs else x :: remDubs xs

fun remBtn b [] = []
  | remBtn b ((i,n)::m) = if b = i then m else (i,n) :: remBtn b m

fun remBtns x [] = []
  | remBtns [] x = x
  | remBtns (b::bs) x = remBtn b x @ remBtns bs x

(*fun addBtn b l = if exists (fn x => x = b) l then l else l @ [b]*)

fun findNextElm [] x = NONE
	  | findNextElm [e] x = SOME e
	  | findNextElm (e::e'::m) x =
	    if x = e then SOME e' else findNextElm (e'::m) x

fun findPrevElm [] x = NONE
  | findPrevElm (e::es) x = findNextElm (rev (e::es)) x

val locs = first locAtts
val atts = second locAtts

val std_btns = [("locator","Locator"),
		("tour","Follow A Tour"),
		("message","Message"),
		("quit","Quit")]

val tour_btns = [("message","Message"),
		 ("back","Back"),
		 ("quit","Quit")]

fun whichLocBtns hl =
    case locs
     of [] => []
      | (l::ls) =>
	if ls = [] then [("select-loc","Select")]
	else
	    if hl = l
	    then [("select-loc","Select"),
		  ("next-loc","Next"),
		  ("back","Back"),
		  ("quit","Quit")]
	    else
		if hl = last ls
		then [("select-loc","Select"),
		      ("previous-loc","Previous"),
		      ("back","Back"),
		      ("quit","Quit")]
		else [("select-loc","Select"),
		      ("previous-loc","Previous"),
		      ("next-loc","Next"),
		      ("back","Back"),
		      ("quit","Quit")]

fun whichSelBtns [] ha = []
  | whichSelBtns [x] ha = [("select-att","Select")]
  | whichSelBtns (x::xs) ha =
    if ha = x then [("select-att","Select"),
		    ("next-tour","Next")]
    else
	if ha = last xs
	then [("select-att","Select"),
	      ("previous-tour","Previous")]
	else [("select-att","Select"),
	      ("previous-tour","Previous"),
	      ("next-tour","Next")]

fun whichInfoBtns [] ha = []
  | whichInfoBtns [x] ha = [("info","Info")]
  | whichInfoBtns (x::xs) ha =
    if ha = x then [("info","Info"),
		    ("next-att","Next")]
    else
	if ha = last xs
	then [("info","Info"),
	      ("previous-att","Previous")]
	else [("info","Info"),
	      ("previous-att","Previous"),
	      ("next-att","Next")]

fun whichTourBtns [] = []
  | whichTourBtns [b] = [("end","End")]
  | whichTourBtns (b::bs) = [("cont","Continue")]

(* State stack with operations *)
val stack : Stack ref = ref []

fun stackSize s = case s of [] => 0 | (x::xs) => 1 + stackSize xs

fun push s = stack := s::(!stack)

fun pop () =
    case (!stack)
     of [] => NONE
      | (e::es) => let val _ = stack := es in SOME(e) end

(* Event queue with operations, 'enq' is visible from L *)
val queue : Queue ref = ref []

fun enq e = queue := (!queue)@[e]

fun deq () =
    case (!queue)
     of [] => NONE
      | (e::es) => let val _ = queue := es in SOME(e) end

(* gui *)
fun printDisp d = print("GUIDE: " ^ d ^ "\n")

fun printLocs (locs,hl) =
    let fun printLocList [] x = print "\n"
	  | printLocList (l::ls) x =
	    if x=l then ( print("*" ^ l ^ "  ") ; printLocList ls x )
	    else ( print(l ^ "  ") ; printLocList ls x )
    in ( print "Locations: " ; printLocList locs hl ) end

fun printAtts (alist,ha) =
    let fun printAttList [] att = print "\n"
	  | printAttList (Att(n,i,m)::t) (att as Att(x,i',m')) =
	    if x=n then ( print("*" ^ n ^ "  ") ; printAttList t att )
	    else ( print(n ^ "  ") ; printAttList t att )
    in ( print "Attractions: " ; printAttList alist ha ) end

fun printCurLoc l =
    print("Location: Device " ^ !our_id ^ " is in " ^ l ^ "\n")

fun printBtns b =
    ( print "Buttons: ";
      app (print o (fn s => s ^ "   ") o #2) b;
      print "\n" )

fun printMsgs m =
    ( print "Messages: ";
      if m = [] then print "No messages."
      else app (print o (fn s => s ^ " ")) m;
      print "\n" )

val pathflag = ref true (* elminate ref? *)

fun printPath p =
    let (*fun printAttList [] = ()
	  | printAttList (Att(n,i,m)::t) =
	    ( print(" " ^ n) ; printAttList t )*)
	fun printPathList [] flag = print "\n"
	  | printPathList ((l,Att(n,i,m))::t) flag =
	    if flag
	    then ( print("#" ^ "(" ^ l ^ ",");
		   print(" " ^ n); (*printAttList a;*)
		   print ")";
		   printPathList t false )
	    else ( print(" -> " ^ "(" ^ l ^ ",");
		   print(" " ^ n); (*printAttList a;*)
		   print ")";
		   printPathList t false )
    in ( print "Path: " ; printPathList p true ; pathflag := true ) end

(* temp code begin *)
fun printStackSize s =
    print("stacksize: " ^ Int.toString(stackSize s) ^ "\n")

fun printStack s =
    let fun printTour flag = if flag then print "t: true\n"
			     else print "t: false\n"
	fun printElm (d,l,a,m,b,t,p,hl,ha) =
	    ( printDisp(d);
	      printCurLoc(l);
	      printAtts(a,ha);
	      printMsgs(m);
	      printBtns(b);
	      printTour(t);
	      printPath(p);
	      print("hl: " ^ hl ^ "\n");
	      print("ha: " ^ #1(deattract(ha)) ^ "\n\n") )
	fun pip s = case s
		      of [] => print "\n"
		       | (e::es) => ( printElm e ; pip es )
    in ( print "#####\n"; pip s ; print "#####\n" ) end
(* temp code end *)

(* maybe do something intelligent later, e.g. find all-pairs shortest
path, but this requires a parent map to be retrieved from the location
model *)
fun calcPath l = l

fun locShow () =
    ( print "\n----------------------------------------------------------\n"
    ; case pop()
       of NONE => print "locShow(): empty stack\n"
	| SOME(d,l,a,m,b,t,p,hl,ha) =>
	  ( printDisp(d)
	  ; printLocs(locs,hl)
	  ; printCurLoc(l)
	  ; printBtns(b)
	  ; push(d,l,a,m,b,t,p,hl,ha)
	  )
    ; print "\nPlease press a button.\n"
    ; print "----------------------------------------------------------\n"
    )

fun tourShow () =
    ( print "\n----------------------------------------------------------\n"
    ; case pop()
       of NONE => print "tourShow(): empty stack\n"
	| SOME(d,l,a,m,b,t,p,hl,ha) =>
	  ( printDisp(d)
	  ; printCurLoc(l)
	  ; printAtts(a,ha)
	  ; printPath(p)
	  ; printMsgs(m)
	  ; printBtns(b)
	  ; push(d,l,a,m,b,t,p,hl,ha)
	  )
    ; print "\nPlease press a button.\n"
    ; print "----------------------------------------------------------\n"
    )

fun guiShow () =
    ( print "\n----------------------------------------------------------\n"
    ; case pop()
       of NONE => print "guiShow(): empty stack\n"
	| SOME(d,l,a,m,b,t,p,hl,ha) =>
	  ( printDisp(d)
	  ; printCurLoc(l)
	  ; printAtts(a,ha)
	  ; if t then printPath(p) else ()
	  ; printMsgs(m)
	  ; printBtns(b)
	  ; push(d,l,a,m,b,t,p,hl,ha)
	  )
    ; print "\nPlease press a button.\n"
    ; print "----------------------------------------------------------\n"
    )

(* Location events *)
(*
fun deviceObserved loc = loc
    if loc = !cur_loc then guiShow() (* still here *)
    else () (* temporary code *)
	( cur_loc := loc
         ; case lookup2 locAtts loc
	    of NONE =>
	       ( cur_atts := []
	       ; if !on_tour then
		     (* recalc. tour_path in this branch? *)
		     ( cur_disp := "You are at location "
		       ^ loc ^ ".\n" ^ "The next tour location is "
		       ^ hd(!tour_path) ^ "."
		     ; cur_btns := [("locator","Locator"),
				    ("message","Message"),
				    ("quit","Quit")]
		     ; guiShow()
		     )
		 else guiShow()
	       )
	     | SOME(atts) =>
	       ( cur_disp := "The following attraction(s) are near to you now."
	       ; cur_atts := atts
	       ; cur_btns := std_btns
	       (*if !on_tour then guiRemoveButton "tour" else ()*)
	       ; guiShow()
	       )
         )
*)

(* Button events *)
fun quitClicked (d,l,a,m,b,t,p,hl,ha) = false

fun backClicked (d,l,a,m,b,t,p,hl,ha) =
( print "backClicked()\n";
    ( guiShow() ; true )
)

fun infoClicked (d,l,a,m,b,t,p,hl,ha) =
( print "infoClicked()\n";
    let val ha' = if t then #2(hd p) else ha
	val d' = (#1(deattract ha')) ^ "\n" ^ (#2(deattract ha'))
	val b' = std_btns @ [("more-info","More info"),
			     ("back","Back")]
    in ( push(d,l,a,m,b,t,p,hl,ha);
	 push(d',l,a,m,b',t,p,hl,ha');
	 guiShow();
	 true )
    end
)

fun moreInfoClicked (d,l,a,m,b,t,p,hl,ha) =
( print "moreInfoClicked()\n";
    let val ha' = if t then #2(hd p) else ha
	val d' = (#1(deattract ha')) ^ "\n" ^ (#3(deattract ha'))
	val b' = std_btns @ [("back","Back")]
    in ( push(d,l,a,m,b,t,p,hl,ha);
	 push(d',l,a,m,b',t,p,hl,ha');
	 guiShow();
	 true )
    end
) 

fun locatorClicked (d,l,a,m,b,t,p,hl,ha) =
( print "locatorClicked()\n";
    let val d' = "Please select one of the following locations:\n"
	val b' = whichLocBtns hl
    in ( push(d,l,a,m,b,t,p,hl,ha);
	 push(d',l,a,m,b',t,p,hl,ha);
	 locShow();
	 true )
    end
)

fun messageClicked (d,l,a,m,b,t,p,hl,ha) =
    ( print "Messaging is not implemented.\n";
      push(d,l,a,m,b,t,p,hl,ha);
      guiShow();
      true )

fun selectLocClicked (d,l,a,m,b,t,p,hl,ha) =
( print "selectLocClicked()\n";
  case pop()
   of NONE => ( print "empty stack\n" ; false )
    | SOME(d',l',a',m',b',t',p',hl',ha') =>
      let val new_atts = case lookup2 locAtts hl of NONE => [] | SOME x => x
	  val (a'',ha'') = if new_atts = [] then ([],voidAtt)
			   else
			       if t' then (new_atts,voidAtt)
			       else (new_atts,hd new_atts)
	  val b'' = if t' (* implies length(p')>0 *)
		    then if length p' = 1 then std_btns @ [("end","End")]
			 else std_btns @ [("cont","Continue")]
		    else std_btns @ (whichInfoBtns a'' ha'')
      in ( push(d',hl,a'',m',b'',t',p',hl,ha'');
	   guiShow();
	   true )
      end
)

fun previousLocClicked (d,l,a,m,b,t,p,hl,ha) =
( print "previousLocClicked()\n";
    let val hl' = case findPrevElm locs hl
		   of NONE => voidLoc | SOME loc => loc
	val b' = whichLocBtns hl'
    in ( push(d,l,a,m,b',t,p,hl',ha) ; locShow() ; true ) end
)

fun nextLocClicked (d,l,a,m,b,t,p,hl,ha) =
( print "nextLocClicked()\n";
    let val hl' = case findNextElm locs hl
		   of NONE => voidLoc | SOME loc => loc
	val b' = whichLocBtns hl'
    in ( push(d,l,a,m,b',t,p,hl',ha) ; locShow() ; true ) end
)

fun previousTourAttClicked (d,l,a,m,b,t,p,hl,ha) =
( print "previousTourAttClicked()\n";
    let val ha' = case findPrevElm a ha
		   of NONE => voidAtt | SOME att => att
	val b' = remDubs (tour_btns @ [("pop","Popular")]
			  @ (whichSelBtns a ha') @ [("done","Done")])
    in ( push(d,l,a,m,b',t,p,hl,ha') ; tourShow() ; true ) end
)

fun previousAttClicked (d,l,a,m,b,t,p,hl,ha) =
( print "previousAttClicked()\n";
    let val ha' = case findPrevElm a ha
		   of NONE => voidAtt | SOME att => att
	val tmp_b = std_btns @ (whichInfoBtns a ha')
	val b' = if t then tmp_b @ (whichTourBtns p) else tmp_b
    in ( push(d,l,a,m,b',t,p,hl,ha') ; guiShow() ; true ) end
)

fun nextTourAttClicked (d,l,a,m,b,t,p,hl,ha) =
( print "nextTourAttClicked()\n";
    let val ha' = case findNextElm a ha
		   of NONE => voidAtt | SOME att => att
	val b' = remDubs (tour_btns @ [("pop","Popular")]
			  @ (whichSelBtns a ha') @ [("done","Done")])
    in ( push(d,l,a,m,b',t,p,hl,ha') ; tourShow() ; true ) end
)

fun nextAttClicked (d,l,a,m,b,t,p,hl,ha) =
( print "nextAttClicked()\n";
    let val ha' = case findNextElm a ha
		   of NONE => voidAtt | SOME att => att
	val tmp_b = std_btns @ (whichInfoBtns a ha')
	val b' = if t then tmp_b @ (whichTourBtns p) else tmp_b
    in ( push(d,l,a,m,b',t,p,hl,ha') ; guiShow() ; true ) end
)

fun tourClicked (d,l,a,m,b,t,p,hl,ha) =
( print "tourClicked()\n";
    let	val d' = "Please select a subset of the following attractions, one at a time.\n"
	val a' = atts
	val b' = tour_btns @ [("pop","Popular")] @ (whichSelBtns a' ha)
	val t' = true
	val p' = []
	val ha' = case a' of [] => voidAtt | (x::xs) => x
    in ( push(d,l,a,m,b,t,p,hl,ha);
	 push(d',l,a',m,b',t',p',hl,ha');
	 tourShow();
	 true )
    end
)

fun popularClicked (d,l,a,m,b,t,p,hl,ha) =
( print "popularClicked()\n";
    let val d' = "You chose the popular tour. Please proceed to the first tour location, marked with '#'.\n"
	val a' = case lookup2 locAtts l of NONE => [] | SOME x => x
	val ha' = voidAtt
	val b' = std_btns @ [("cont","Continue")]
	val getLocs =
	    fn att => case findLoc locAtts att
		       of NONE => (voidLoc,att) | SOME loc => (loc,att)
	val locatts = map getLocs popular
	val pred = fn (x,y) => not(x = voidLoc)
	val p' = (calcPath o filter) pred locatts
    in ( push(d',l,a',m,b',t,p',hl,ha');
	 tourShow();
	 true )
    end
)

fun selectAttClicked (d,l,a,m,b,t,p,hl,ha) =
( print "selectAttClicked()\n";
    let val a' = filter (fn x => not(x = ha)) a
	val p' = case findLoc locAtts ha
		  of NONE => [] | SOME loc => calcPath (p @ [(loc,ha)])
	val ha' = case findPrevElm a' ha
		   of NONE => voidAtt | SOME att => att
	val tmp_b = tour_btns @ [("pop","Popular")]
		    @ (whichSelBtns a' ha') @ [("done","Done")]
	val b' = remDubs tmp_b
    in ( push(d,l,a',m,b',t,p',hl,ha');
	 tourShow();
	 true )
    end
)

fun doneClicked (d,l,a,m,b,t,p,hl,ha) =
( print "doneClicked()\n";
  case pop()
   of NONE => ( print "empty stack\n" ; false )
    | SOME(d',l',a',m',b',t',p',hl',ha') =>
      let val d'' = "You have confirmed your tour. Please proceed to the first tour location, marked with '#'.\n"
	  val b'' = if length p = 1 then b' @ [("end","End")]
		    else b' @ [("cont","Continue")] (* p>1 *)
      in ( push(d',l',a',m',b',t',p',hl',ha'); (* init. display *)
	   push(d'',l,a',m,b'',t,p,hl,ha');
	   tourShow();
	   true )
      end
)

fun continueClicked (d,l,a,m,b,t,p,hl,ha) =
( print "continueClicked()\n";
  let val d' = "Please proceed to the next tour location, marked with '#'.\n"
      val b' = if length p = 2
	       then (remBtn "cont" b) @ [("end","End")]
	       else b (* p>2 *)
      val p' = tl p (* non-empty by invariant *)
  in ( push(d',l,a,m,b',t,p',hl,ha);
       tourShow();
       true )
  end
)

fun endClicked (d,l,a,m,b,t,p,hl,ha) =
( print "endClicked()\n";
  case pop()
   of NONE => ( print "empty stack\n" ; false )
    | SOME(d',l',a',m',b',t',p',hl',ha') =>
      let val t'' = false
      in ( push(d',l,a',m',b',t'',p',hl',ha');
	   guiShow();
	   true )
      end
)

val button_clicks =
    [("quit", quitClicked),
     ("back", backClicked),
     ("info", infoClicked),
     ("more-info", moreInfoClicked),
     ("locator", locatorClicked),
     ("message", messageClicked),
     ("tour", tourClicked),
     ("select-loc", selectLocClicked),
     ("previous-loc", previousLocClicked),
     ("next-loc", nextLocClicked),
     ("previous-att", previousAttClicked),
     ("next-att", nextAttClicked),
     ("previous-tour", previousTourAttClicked),
     ("next-tour", nextTourAttClicked),
     ("select-att", selectAttClicked),
     ("done", doneClicked),
     ("pop", popularClicked),
     ("cont", continueClicked),
     ("end", endClicked)]

fun handleEvent event s =
    case event of
	DeviceObserved(dev,loc) => true (* temp. code *)
(*
	if dev = !our_id then ( deviceObserved(loc) ; true )
        else true (* not about us, ignore *)
*)
      | ButtonClicked btn =>
        ( case lookup2 button_clicks btn
	   of NONE => true (* unknown button, ignore *)
            | SOME f => f s
	)
      (* delete this?
      | ShortestPath(from,to,path) =>
          if from = !current_location then displayPath path
          else (* perhaps do something intelligent when we
                  are on the path, for now just ignore *)
               true
       *)

(* I/O *)
fun errmsg () =
    ( output(stdOut, "Unavailable button. Please try again.\n");
      flushOut stdOut )

fun peel s =
    if String.size s <= 0 then ""
    else String.substring(s, 0, (String.size s)-1)

fun read () =
(*
( printStackSize(!stack); printStack(!stack);
*)
    case pop()
     of NONE => print "read(): empty stack\n"
      | SOME(s) =>
	( let val (d,l,a,m,b,t,p,hl,ha) = s
	  in case lookup1 b (peel (inputLine stdIn)) of
		 NONE => ( errmsg() ; push(s) ; read() )
	       | SOME(btn) => ( enq(ButtonClicked btn);
				(* test code begin *)
				if not (btn = "quit") andalso
				   handleEvent(ButtonClicked btn) s
				then read()
				else ()
				     (* test code end *)
				     (*; read()*)
				     )
	  end
	      )
(*
)
*)

(* Event loop
fun eventLoop () =
    case deq()
     of NONE => eventLoop()
      | SOME e => if handleEvent e then eventLoop()
		  else () (* halt *)
 *)

(* this function must be supplied by L *)
fun whereIs d = "dummy_location"

fun main () =
    ( let val disp = "Welcome to Lancaster\n"
	  val loc = case first locAtts
		     of [] => "" | (l::ls) => l
	  val atts = case lookup2 locAtts loc
		      of NONE => [] | SOME(a) => a
	  val msgs = []
	  val btns = std_btns
	  val on_tour = false
	  val tour_path = []
	  val hilite_loc = loc
	  val hilite_att =
	      case atts of [] => voidAtt | (att::atts) => att
      in push(disp,loc,atts,msgs,btns,on_tour,
	      tour_path,hilite_loc,hilite_att) end
    ; guiShow()
    ; read()
    (*; eventLoop()*)
    )