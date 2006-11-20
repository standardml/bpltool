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

val read = fn () => TextIO.inputLine(TextIO.stdIn)

datatype attraction = Att of
	 string (* name *) * string (* info *) * string (* more info *)

fun deattract (Att tup) = tup

(* Attractions *)
(* begin popular *)
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

(* end popular *)
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
    if x=l then ( print("* " ^ l) ; printLocList ls x )
    else ( print l ; printLocList ls x )

fun printAttList [] att = print "\n"
  | printAttList (Att(n,i,m)::t) (att as Att(x,i',m')) =
    if x=n then ( print ("* " ^ n) ; printAttList t att )
    else ( print n ; printAttList t att )

type link = string
type device = link
type location = link
type display = string
type btnid = string
type group = device list

datatype event =
	 DeviceObserved of device * location
       | ButtonClicked of btnid

type queue = event list
type 'a stack = 'a list

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

(* Constants *)
val our_id : device ref = ref "ab:cd:ef:gh:ij:kl"
val our_grp : group ref = ref ["d1","d2","d3"]

(* State *)
type state = string * attraction list * string * string list 
	     * (btnid*string) list * bool * location list 
	     * attraction * string

val cur_disp : display ref = ref ""
val cur_atts : attraction list ref = ref []
val cur_loc : location ref = ref ""
val cur_msgs : string list ref = ref []
val cur_btns : (btnid*string) list ref = ref []
val on_tour : bool ref = ref false
val tour_path : location list ref = ref []
val hilite_att : attraction ref = ref(Att("","",""))
val hilite_loc : location ref = ref ""
val cur_state = ref (!cur_disp, !cur_atts, !cur_loc, !cur_msgs,
		     !cur_btns, !on_tour, !tour_path,
		     !hilite_att, !hilite_loc)
val queue : queue ref = ref []
val stack : state stack ref = ref []

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

fun guiShow () =
    ( print(!cur_disp)
    ; printAttList (!cur_atts) (!hilite_att)
    ; print(!cur_loc)
    ; app print (!cur_msgs)
    ; app (print o #2) (!cur_btns)
    )

fun locShow () =
    ( print(!cur_disp)
    ; printLocList locs (!hilite_loc)
    ; print(!cur_loc)
    (* leave out messages here *)
    ; app (print o #2) (!cur_btns)
    )

(* location event *)
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

(* State stack operations *)
fun push s = stack := s::(!stack)

fun pop () =
    case (!stack) of
	[] => NONE
      | (e::es) => let val _ = stack := es in SOME(e) end

fun saveState () =
    push (!cur_disp, !cur_atts, !cur_loc,
	  !cur_msgs, !cur_btns, !on_tour,
	  !tour_path, !hilite_att, !hilite_loc)

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

(* Button events *)
fun quitClicked () = false

fun backClicked () =
    case pop() of
	NONE => ( guiShow() ; true )
      | SOME(s) => ( setState s; guiShow(); true )

fun infoClicked () =
    ( saveState()
    ; cur_disp := (#1(deattract(!hilite_att))) ^ "\n" ^ (#2(deattract(!hilite_att)))
    ; cur_btns := std_btns
    ; guiAddButton ("more-info","More info")
    ; guiAddButton ("back","Back")
    ; guiShow()
    ; true
    )

fun moreInfoClicked () =
    ( saveState()
    ; cur_disp := (#1(deattract(!hilite_att))) ^ "\n" ^ (#3(deattract(!hilite_att)))
    ; cur_btns := std_btns
    ; guiAddButton ("back","Back")
    ; guiShow()
    ; true
    )

fun locatorClicked () =
    ( cur_disp := "Please select one of the following locations."
    ; if !hilite_loc = hd locs then
	  ( saveState()
	  ; cur_btns := [("select","Select"),
			 ("next-loc","Next"),
			 ("back","Back"),
			 ("quit","Quit")]
	  )
      else if !hilite_loc = last locs then
	  ( saveState()
	  ; cur_btns := [("select","Select"),
			 ("previous-loc","Previous"),
			 ("back","Back"),
			 ("quit","Quit")]
	  )
      else ( saveState()
	   ; cur_btns := [("select","Select"),
			  ("previous-loc","Previous"),
			  ("next-loc","Next"),
			  ("back","Back"),
			  ("quit","Quit")]
           )
    ; locShow()
    ; true
    )

fun messageClicked () = ( guiShow() ; true )

fun tourClicked () = ( guiShow() ; true )

fun selectAttClicked () = ( guiShow() ; true )

fun selectLocClicked () =
    case pop () of 
	NONE => (* shouldn't happen, but return true anyway *) true
      | SOME(s) => let val (d,a,l,m,b,t,p,ha,hl) = s in
		       ( setState s
		       ; cur_loc := !hilite_loc
		       ; guiShow()
		       ; true
		       )
		   end

fun previousLocClicked () =
    let val prevloc = findNextLoc (rev locs) (!hilite_loc)
    in ( hilite_loc := prevloc ; locShow() ; true ) end

fun nextLocClicked () =
    let val nextloc = findNextLoc locs (!hilite_loc)
    in ( hilite_loc := nextloc ; locShow() ; true ) end

val button_clicks =
    [("quit", quitClicked),
     ("back", backClicked),
     ("info", infoClicked),
     ("more-info", moreInfoClicked),
     ("locator", locatorClicked),
     ("message",messageClicked),
     ("tour",tourClicked),
     ("select",selectLocClicked),
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

(* Event queue operations, visible from L *)
fun enq e = queue := (!queue)@[e]

fun deq () =
    case (!queue) of
	[] => NONE
      | (e::es) => let val _ = queue := es in SOME(e) end

fun eventLoop () =
    case deq() of
	NONE => eventLoop()
      | SOME e => if handleEvent e then eventLoop()
		  else () (* halt *)

(* this function must be supplied by L *)
fun whereIs d = d ^ " is in some_location"

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
    ( cur_disp := "Welcome to Lancaster"
    ; cur_loc := whereIs(!our_id)
    ; cur_msgs := [] (* no messages *)
    ; cur_btns := std_btns
    ; guiShow()
    )
