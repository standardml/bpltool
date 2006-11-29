(*
 Implementation of the context-aware tourist guide GUIDE;
 see the MobiCom 2000 paper "Experiences of Developing and Deploying
 a Context-Aware Tourist Guide: The GUIDE Project" by K. Cheverst,
 N. Davies, K. Mitchell, and A. Friday.

 This implementation is simpler than the GUIDE application in the
 the following aspects:
 - lalala

 Ebbe Elsborg, November 2006
*)

(* use "l.sml"; *)
open TextIO;
open List;
(*open Bool;*)

(* Basic entities *)
datatype attraction =
	 Att of string (* name *) * string (* info *) * string (* more info *)
       | VoidAtt

fun deattract (Att tup) = tup

type link = string
type device = link
type location = link
type display = string
type btnid = string
type message = string
type group = device list

type state = display * location * attraction list * message list 
	     * (btnid * string) list * bool * location list 
	     * location * attraction

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

val location_attractions : (location * attraction list) list ref =
    (* attractions are assumed to be ordered spatially wrt. closeness,
     the first attraction in the list is closest *)
    ref [("TIC", []),
	 ("l1", [castle,Att("Dummy","info","moreinfo")]),
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

fun lookup1 map x =
    let fun loop [] = NONE
	  | loop ((l,a)::m) = if l = x then SOME l else loop m
    in loop map end

fun lookup2 map x =
    let fun loop [] = NONE
	  | loop ((l,a)::m) = if l = x then SOME a else loop m
    in loop map end

fun first [] = []
  | first ((l,a)::m) = l :: first m

fun exists p [] = false
  | exists p (x::xs) = p x orelse exists p xs

fun filter p [] = []
  | filter p (x::xs) = if p x then x :: filter p xs else filter p xs

fun last x = (hd o rev) x

fun findNextElm [] x = x
	  | findNextElm [e] x = e
	  | findNextElm (e::e'::m) x =
	    if x=e then e' else findNextElm (e'::m) x

val locs = first (!location_attractions)

fun whichBtns hl =
    if hl = hd locs
    then [("select-loc","Select"),
	  ("next-loc","Next"),
	  ("back","Back"),
	  ("quit","Quit")]
    else if hl = last locs
    then [("select-loc","Select"),
	  ("previous-loc","Previous"),
	  ("back","Back"),
	  ("quit","Quit")]
    else [("select-loc","Select"),
	  ("previous-loc","Previous"),
	  ("next-loc","Next"),
	  ("back","Back"),
	  ("quit","Quit")]

fun whichBtns' a ha =
    if deattract ha = deattract(hd a)
    then [("info","Info"),
	  ("next-att","Next"),
	  ("back","Back"),
	  ("quit","Quit")]
    else if deattract ha = deattract(last a)
    then [("info","Info"),
	  ("previous-att","Previous"),
	  ("back","Back"),
	  ("quit","Quit")]
    else [("info","Info"),
	  ("previous-att","Previous"),
	  ("next-att","Next"),
	  ("back","Back"),
	  ("quit","Quit")]

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
val std_btns = [("locator","Locator"),
		("tour","Follow A Tour"),
		("msg","Message"),
		("quit","Quit")]

fun printDisp d = print("GUIDE: " ^ d ^ "\n")

fun printLocs (locs,hl) =
    let fun printLocList [] x = print "\n"
	  | printLocList (l::ls) x =
	    if x=l then ( print("*" ^ l ^ " ") ; printLocList ls x )
	    else ( print(l ^ " ") ; printLocList ls x )
    in ( print "Locations: " ; printLocList locs hl ) end

fun printAtts (atts,ha) =
    let fun printAttList [] att = print "\n"
	  | printAttList (Att(n,i,m)::t) (att as Att(x,i',m')) =
	    if x=n then ( print("*" ^ n ^ " ") ; printAttList t att )
	    else ( print(n ^ " ") ; printAttList t att )
    in ( print "Attractions: " ; printAttList atts ha ) end

fun printCurLoc l =
    print("Location: Device " ^ !our_id ^ " is in " ^ l ^ "\n")

fun printBtns b =
    ( print "Buttons: "
    ; app (print o (fn s => s ^ "   ") o #2) b
    ; print "\n"
    )

fun printMsgs m =
    ( print "Messages: "
    ; if m = [] then print "No messages."
      else app (print o (fn s => s ^ " ")) m
    ; print "\n"
    )

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

fun guiShow () =
    ( print "\n----------------------------------------------------------\n"
    ; case pop()
       of NONE => print "guiShow(): empty stack\n"
	| SOME(d,l,a,m,b,t,p,hl,ha) =>
	  ( printDisp(d)
	  ; printCurLoc(l)
	  ; printAtts(a,ha)
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
         ; case lookup2 (!location_attractions) loc
	    of NONE =>
	       ( cur_atts := []
	       ; if !on_tour then
		     (* recalc. tour_path in this branch? *)
		     ( cur_disp := "You are at location "
		       ^ loc ^ ".\n" ^ "The next tour location is "
		       ^ hd(!tour_path) ^ "."
		     ; cur_btns := [("locator","Locator"),
				    ("msg","Message"),
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
( print "backClicked()\n"
; print("stacksize: " ^ Int.toString(stackSize(!stack)));
    ( guiShow() ; true )
)

fun infoClicked (d,l,a,m,b,t,p,hl,ha) =
( print "infoClicked()\n"
; print("stacksize: " ^ Int.toString(stackSize(!stack)));
    let val d' = (#1(deattract ha)) ^ "\n" ^ (#2(deattract ha))
	val b' = std_btns @ [("more-info","More info"),
			     ("back","Back")]
    in ( push(d,l,a,m,b,t,p,hl,ha)
       ; push(d',l,a,m,b',t,p,hl,ha)
       ; print("stacksize: " ^ Int.toString(stackSize(!stack)))
       ; guiShow()
       ; true
       )
    end
)

fun moreInfoClicked (d,l,a,m,b,t,p,hl,ha) =
( print "moreInfoClicked()\n"
; print("stacksize: " ^ Int.toString(stackSize(!stack)));
    let val d' = (#1(deattract ha)) ^ "\n" ^ (#3(deattract ha))
	val b' = std_btns @ [("back","Back")]
    in ( push(d,l,a,m,b,t,p,hl,ha)
       ; push(d',l,a,m,b',t,p,hl,ha)
       ; print("stacksize: " ^ Int.toString(stackSize(!stack)))
       ; guiShow()
       ; true
       )
    end
) 

fun locatorClicked (d,l,a,m,b,t,p,hl,ha) =
( print "locatorClicked()\n"
; print("stacksize: " ^ Int.toString(stackSize(!stack)));
    let val d' = "Please select one of the following locations:\n"
	val b' = whichBtns hl
    in ( push(d,l,a,m,b,t,p,hl,ha)
       ; push(d',l,a,m,b',t,p,hl,ha)
       ; locShow()
       ; true
       )
    end
)

fun messageClicked (d,l,a,m,b,t,p,hl,ha) = ( guiShow() ; true )

fun tourClicked (d,l,a,m,b,t,p,hl,ha) = ( guiShow() ; true )

fun selectLocClicked (d,l,a,m,b,t,p,hl,ha) =
( print "selectLocClicked()\n"
; print("stacksize: " ^ Int.toString(stackSize(!stack)));
  case pop()
   of NONE => ( "empty stack\n" ; false )
    | SOME(d',l',a',m',b',t',p',hl',ha') =>
      let val a'' = case lookup2 (!location_attractions) hl
		     of NONE => [] | SOME(a) => a
	  val ha'' = case a'' of [] => VoidAtt | (att::atts) => att
	  val b'' = if a'' = [] then std_btns
		    else
			if length(a'') = 1 then std_btns @ [("info","Info")]
			else std_btns @ [("info","Info"),
					 ("next-att","Next")]
      in ( push(d',hl,a'',m',b'',t',p',hl,ha'')
	 ; guiShow()
	 ; true
	 )
      end
)

fun previousLocClicked (d,l,a,m,b,t,p,hl,ha) =
( print "previousLocClicked()\n"
; print("stacksize: " ^ Int.toString(stackSize(!stack)));
    let val hl' = findNextElm (rev locs) hl
	val b' = whichBtns hl'
    in ( push(d,l,a,m,b',t,p,hl',ha) ; locShow() ; true ) end
)

fun nextLocClicked (d,l,a,m,b,t,p,hl,ha) =
( print "nextLocClicked()\n"
; print("stacksize: " ^ Int.toString(stackSize(!stack)));
    let val hl' = findNextElm locs hl
	val b' = whichBtns hl'
    in ( push(d,l,a,m,b',t,p,hl',ha) ; locShow() ; true ) end
)

fun previousAttClicked (d,l,a,m,b,t,p,hl,ha) =
( print "previousAttClicked()\n"
; print("stacksize: " ^ Int.toString(stackSize(!stack)));
    let val ha' = findNextElm (rev a) ha
	val b' = whichBtns' a ha'
    in ( push(d,l,a,m,b',t,p,hl,ha') ; guiShow() ; true ) end
)

fun nextAttClicked (d,l,a,m,b,t,p,hl,ha) =
( print "nextAttClicked()\n"
; print("stacksize: " ^ Int.toString(stackSize(!stack)));
    let val ha' = findNextElm a ha
	val b' = whichBtns' a ha'
    in ( push(d,l,a,m,b',t,p,hl,ha') ; guiShow() ; true ) end
)

val button_clicks =
    [("quit", quitClicked),
     ("back", backClicked),
     ("info", infoClicked),
     ("more-info", moreInfoClicked),
     ("locator", locatorClicked),
     ("message",messageClicked),
     ("tour",tourClicked),
     ("select-loc",selectLocClicked),
     ("previous-loc",previousLocClicked),
     ("next-loc",nextLocClicked),
     ("previous-att",previousAttClicked),
     ("next-att",nextAttClicked)]

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
    ( output(stdOut, "Unavailable button. Please try again.\n")
    ; flushOut stdOut
    )

fun peel s =
    if String.size s <= 0 then ""
    else String.substring(s, 0, (String.size s)-1)

fun read () =
    case pop()
     of NONE => print "read(): empty stack\n"
      | SOME(s) =>
	( let val (d,l,a,m,b,t,p,hl,ha) = s
	  in case lookup1 b (peel (inputLine stdIn)) of
		 NONE => ( errmsg() ; push(s) ; read() )
	       | SOME(btn) => ( enq(ButtonClicked btn)
			      (* test code begin *)
			      ; output(stdOut, "ButtonClick queued...\n")
			      ; if not (btn = "quit") andalso
				   handleEvent(ButtonClicked btn) s
				then read()
				else ()
			      (* test code end *)
			    (*; read()*)
			      )
	  end
	)

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
	  val loc = case first (!location_attractions)
		     of [] => "" | (l::ls) => l
	  val atts = case lookup2 (!location_attractions) loc
		      of NONE => []
		       | SOME(a) => a
	  val msgs = []
	  val btns = std_btns
	  val on_tour = false
	  val tour_path = []
	  val hilite_loc = loc
	  val hilite_att =
	      case atts of [] => VoidAtt | (att::atts) => att
      in push(disp,loc,atts,msgs,btns,on_tour,
	      tour_path,hilite_loc,hilite_att) end
    ; guiShow()
    ; read()
    (*; eventLoop()*)
    )