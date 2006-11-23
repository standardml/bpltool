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
open Bool;

(* Basic entities *)
datatype attraction = Att of
	 string (* name *) * string (* info *) * string (* more info *)

fun deattract (Att tup) = tup

type link = string
type device = link
type location = link
type display = string
type btnid = string
type message = string
type group = device list

datatype event =
	 DeviceObserved of device * location
       | ButtonClicked of btnid

type state = display * attraction list * location * message list 
	     * (btnid * string) list * bool * location list 
	     * attraction * location
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

(* Auxiliary functions *)
fun app f [] = ()
  | app f (x::xs) = ( f x ; app f xs )

fun lookup map x =
    let fun loop [] = NONE
	  | loop ((l,a)::m) = if l = x then SOME a else loop m
    in loop map end

fun lookupBtn map x =
    let fun loop [] = NONE
	  | loop ((l,a)::m) = if l = x then SOME l else loop m
    in loop map end

fun first [] = []
  | first ((l,a)::m) = l :: first m

fun exists p [] = false
  | exists p (x::xs) = p x orelse exists p xs

fun filter p [] = []
  | filter p (x::xs) = if p x then x :: filter p xs else filter p xs

val last = hd o rev

fun findNextLoc [] x = x
	  | findNextLoc [l] x = l
	  | findNextLoc (l::l'::m) x =
	    if x=l then l'
	    else findNextLoc (l'::m) x

fun printLocList [] x = print "\n"
  | printLocList (l::ls) x =
    if x=l then ( print("*" ^ l ^ " ") ; printLocList ls x )
    else ( print(l ^ " ") ; printLocList ls x )

fun printAttList [] att = print "\n"
  | printAttList (Att(n,i,m)::t) (att as Att(x,i',m')) =
    if x=n then ( print("*" ^ n ^ " ") ; printAttList t att )
    else ( print(n ^ " ") ; printAttList t att )

(* Popular attractions and location-attraction relationship *)
val popular : attraction list = [castle,williamson,queenvic,seagull,halfmoon,market,canal,nightingale,spooky,ruxton,priory]

val location_attractions : (location * attraction list) list ref =
    (* attractions are assumed to be ordered spatially wrt. closeness,
     the first attraction in the list is closest *)
    ref [("l1", [castle]),
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

val locs = first (!location_attractions)

(* State *)
val cur_disp : display ref = ref ""
val cur_atts : attraction list ref = ref []
val cur_loc : location ref = ref ""
val cur_msgs : message list ref = ref []
val cur_btns : (btnid * string) list ref = ref []
val on_tour : bool ref = ref false
val tour_path : location list ref = ref []
val hilite_att : attraction ref = ref(Att("","",""))
val hilite_loc : location ref = case locs of [] => ref ""
					   | (l::ls) => ref l

(* Constants *)
val our_id : device ref = ref "ab:cd:ef:gh:ij:kl"
val our_grp : group ref = ref ["d1","d2","d3"]

(* State stack with operations *)
val stack : Stack ref = ref []

fun push s = stack := s::(!stack)

fun pop () =
    case (!stack) of
	[] => NONE
      | (e::es) => let val _ = stack := es in SOME(e) end

(* State operations *)
fun saveState () =
( print "saveState()\n" ;
    push (!cur_disp, !cur_atts, !cur_loc,
	  !cur_msgs, !cur_btns, !on_tour,
	  !tour_path, !hilite_att, !hilite_loc)
)

fun setState s =
    let val (d,a,l,m,b,t,p,ha,hl) = s in
	( cur_disp := d
	; cur_atts := a
	; cur_loc := l
	; cur_msgs := m
	; cur_btns := b
	; on_tour := t
	; tour_path := p
	; hilite_att := ha
	; hilite_loc := hl
	)
    end

(* Event queue with operations, 'enq' is visible from L *)
val queue : Queue ref = ref []

fun enq e = queue := (!queue)@[e]

fun deq () =
    case (!queue) of
	[] => NONE
      | (e::es) => let val _ = queue := es in SOME(e) end

(* gui *)
val std_btns = [("locator","Locator"),
		("tour","Follow A Tour"),
		("msg","Message"),
		("quit","Quit")]

fun guiAddButton (btn_id, btn_txt) =
    if exists (fn (b,_) => b = btn_id) (!cur_btns) then ()
    else cur_btns := (!cur_btns)@[(btn_id,btn_txt)]

fun guiRemoveButton btn_id =
    cur_btns := filter (fn b => not(#1(b) = btn_id)) (!cur_btns)

fun printDisp () =
    print("GUIDE: " ^ !cur_disp ^ "\n")

fun printAtts () =
    ( print "Attractions: " ; printAttList (!cur_atts) (!hilite_att) )

fun printLocs () =
    ( print "Locations: " ; printLocList locs (!hilite_loc) )

fun printCurLoc () =
    print("Location: Device " ^ !our_id ^ " is in " ^ !cur_loc ^ "\n")

fun printBtns () =
    ( print "Buttons: "
    ; app (print o (fn s => s ^ "   ") o #2) (!cur_btns)
    ; print "\n"
    )

fun printMsgs () =
    ( print "Messages: "
    ; if !cur_msgs = [] then print "No messages."
      else app (print o (fn s => s ^ " ")) (!cur_msgs)
    ; print "\n"
    )

(* test code *)
fun printState () =
    ( print("State...\n")
    ; printDisp()
    ; printAtts()
    ; printCurLoc()
(*    ; printMsgs()*)
    ; printBtns()
(*    ; print("ontour: " ^ Bool.toString(!on_tour) ^ "\n")
    ; let fun printPath () =
	      ( print "tour_path: "
	      ; if !tour_path = [] then print " "
		else app (print o (fn s => s ^ " ")) (!tour_path)
	      ; print "\n"
	      )
      in printPath() end
    ; print("h_att: " ^ (#1(deattract(!hilite_att))) ^ "\n")
    ; print("h_loc: " ^ (!hilite_loc) ^ "\n")*)
    )

fun locShow () =
    ( print "\n----------------------------------------------------------\n" 
    ; printDisp()
    ; printLocs()
    ; printCurLoc()
    ; printBtns()
    ; print "\nPlease press a button.\n"
    ; print "----------------------------------------------------------\n"
    )

fun guiShow () =
    ( print "\n----------------------------------------------------------\n"
    ; printDisp()
    ; printAtts()
    ; printCurLoc()
    ; printMsgs()
    ; printBtns()
    ; print "\nPlease press a button.\n"
    ; print "----------------------------------------------------------\n"
    )

(* Location events *)
fun deviceObserved loc =
    if loc = !cur_loc then guiShow() (* still here *)
    else ( cur_loc := loc
         ; case lookup (!location_attractions) loc of
	       NONE =>
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

(* Button events *)
val flag = ref true

fun quitClicked () = false

(* check this function! *)
fun backClicked () =
(
 print "backClicked()\n";
      case pop() of
	  NONE => ( print "empty stack\n" ; guiShow() ; true )
	| SOME(s) => ( setState s
		     ; printState() (* test code *)
		     ; flag := true
		     ; guiShow()
		     ; true
		     )
)

fun infoClicked () =
    ( saveState()
    ; cur_disp := (#1(deattract(!hilite_att))) ^ "\n" ^
		  (#2(deattract(!hilite_att)))
    ; cur_btns := std_btns
    ; guiAddButton("more-info","More info")
    ; guiAddButton("back","Back")
    ; guiShow()
    ; true
    )

fun moreInfoClicked () =
    ( saveState()
    ; cur_disp := (#1(deattract(!hilite_att))) ^ "\n" ^
		  (#3(deattract(!hilite_att)))
    ; cur_btns := std_btns
    ; guiAddButton("back","Back")
    ; guiShow()
    ; true
    )

fun locatorClicked () =
    ( print "locatorClicked()\n"
    ; if !flag
      then (print "flag was true\n" ;
           ( saveState()
	   ; printState() (* test code *)
	   ; flag := false
	   ; print "flag set to false"
	   ; cur_disp := "Please select one of the following locations:\n"
	   )
	   )
      else printState() (* test code *)
    ; if !hilite_loc = hd locs then
	  cur_btns := [("select-loc","Select"),
		       ("next-loc","Next"),
		       ("back","Back"),
		       ("quit","Quit")]
      else if !hilite_loc = last locs then
	  cur_btns := [("select-loc","Select"),
		       ("previous-loc","Previous"),
		       ("back","Back"),
		       ("quit","Quit")]
	 
      else cur_btns := [("select-loc","Select"),
			("previous-loc","Previous"),
			("next-loc","Next"),
			("back","Back"),
			("quit","Quit")]
    ; locShow()
    ; true
    )

fun messageClicked () = ( guiShow() ; true )

fun tourClicked () = ( guiShow() ; true )

fun selectAttClicked () = ( guiShow() ; true )

fun selectLocClicked () =
(
    print "selectLocClicked\n";
    case pop() of
	NONE => ( print "damn" ; flag := true ; true ) (* shouldn't happen *)
      | SOME(s) => let val (d,a,l,m,b,t,p,ha,hl) = s in
		       ( setState (d,a,!hilite_loc,m,b,t,p,ha,!hilite_loc)
		       ; printState() (* test code *)
		       ; flag := true
		       ; guiShow()
		       ; true
		       )
		   end
)

fun previousLocClicked () =
    ( hilite_loc := findNextLoc (rev locs) (!hilite_loc)
    ; locatorClicked()
    )

fun nextLocClicked () =
(
    print "nextLocClicked\n";
    ( hilite_loc := findNextLoc locs (!hilite_loc)
    ; locatorClicked()
    )
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
     ("select-att",selectAttClicked),
     ("previous-loc",previousLocClicked),
     ("next-loc",nextLocClicked)]

fun handleEvent event =
    case event of
	DeviceObserved(dev,loc) =>
	if dev = !our_id then ( deviceObserved(loc) ; true )
        else true (* not about us, ignore *)
      | ButtonClicked btn =>
        ( case lookup button_clicks btn of
	      NONE => true (* unknown button, ignore *)
            | SOME f => f ()
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
    case lookupBtn (!cur_btns) (peel (inputLine stdIn)) of
	NONE => ( errmsg() ; read() )
      | SOME(b) => ( enq(ButtonClicked b)
		   (* test code begin *)
		   ; output(stdOut, "ButtonClick queued...\n")
		   ; if handleEvent(ButtonClicked b)
			andalso not (b = "quit")
		     then read()
		     else () (* halt *)
		   (* test code end *)
		   (*; read()*)
		   )

(* Event loop *)
fun eventLoop () =
    case deq() of
	NONE => eventLoop()
      | SOME e => if handleEvent e then eventLoop()
		  else () (* halt *)

(* this function must be supplied by L *)
fun whereIs d = "dummy_location"

fun main () =
    (* what goes on here...?
    ( guiRegisterButtonHandler
          (fn button_id => enq(ButtonClicked button_id))
    ; locRegisterHandler
	  (fn dev => fn loc => enq(DeviceObserved(dev,loc))) (!our_id)
    ; dbRegisterResponseHandler
          (fn (request,response) =>
	      enq((eventTypeOf(request))(artifactOf(request)),
		  response)
	   )
     *)
    (* initialise gui *)
    ( cur_disp := "Welcome to Lancaster\n"
    ; cur_loc := whereIs(!our_id)
    ; cur_msgs := [] (* no messages *)
    ; cur_btns := std_btns
    ; guiShow()
    ; read()
    (*; eventLoop()*)
    )