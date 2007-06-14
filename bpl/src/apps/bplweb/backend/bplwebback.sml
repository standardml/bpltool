(* Copyright (c) 2007  The BPL Group at the IT University of Copenhagen
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
 
(** Backend matcher and reactive system for BPLweb.
 * @version $LastChangedRevision$
 *)

functor BPLwebBack (structure BG : BG) =
struct
  open BG
  
  open SignalHandler
  
  val noinfo = Info.noinfo
  open LazyList
  type 'a array = 'a Array.array
  val sub = Array.sub
  infix 4 sub
  type match = Match.match

  open TextIO

  (** Please implement the PrimTextIO.RD readVec function for stdIn. *)
  exception PleaseImplement

  (** Expected some string in input before EOF. *)
  exception Expected of string

  (** This exception ought not to be able to happen. *)
  exception ThisCannotHappen

  local
    val (TextPrimIO.RD {name, chunkSize, readVec, readArr, readVecNB, readArrNB,
             block, canInput, avail, getPos, setPos, endPos, verifyPos,
             close, ioDesc}, line) = StreamIO.getReader (getInstream stdIn)
    val line = ref line
    fun isEmpty s = (s = "")
    fun CharVector_findi f s = 
      let
        fun seek i = 
          let 
            val c = String.sub (s, i)
          in
            if f (i, c) then SOME (i, c) else seek (i + 1)
          end
      in
        seek 0 handle subscript => NONE
      end
    val readVec =
      case readVec of
        SOME f => f
      | NONE => raise PleaseImplement
  in
    fun inputLineNoBuf () =
      case CharVector_findi
             (fn (_, c) => Char.contains "\n\r" c)
             (!line)
        of
        SOME (i, _)
      => SOME (substring (!line, 0, i + 1))
               before line := String.extract (!line, i + 1, NONE)
      | NONE => 
        let
          val moreline = readVec 512
        in
          if isEmpty moreline then
            if isEmpty (!line) then
              NONE
            else
              (SOME (CharVector.concat [!line, "\n"])
               before line := "")
          else
            (line := CharVector.concat [!line, moreline];
             inputLineNoBuf ())
        end
  end
(*  local
    val line = ref []
    fun skiplastemptystr [] = []
      | skiplastemptystr [""] = []
      | skiplastemptystr (s :: ss) = s :: skiplastemptystr ss
    fun inputLine' stream =
      (if endOfStream stream andalso null (!line) then
         NONE
       else
         ((if null (!line) then
             line
               := skiplastemptystr
                    (String.fields
                       (fn c => Char.contains "\n\r" c)
                       (input stream))
           else
             ());
          case !line of
            [] => SOME "\n"
          | s :: ss => (line := ss; SOME s)))
  in
    fun inputLine stream =
      let
        val s = inputLine' stream
      in
        (case s of
           SOME line => output (stdErr, "<<" ^ line ^ ">>\n")
         | NONE => output (stdErr, "<NIL>\n"));
         flushOut stdErr;
        s
      end
  end*)

    fun run () =
    let
      val _ = ignoreInterrupts ()
      val _ = print "READY\n" (* We are now immune to INT signals *)
      fun log errtxt = output (stdErr, "ERROR\n" ^ errtxt ^ "\nEND\n")
      
      fun parsenum str =
        if Substring.isSubstring "ALL" (Substring.full str) then
          ~1
        else
          case Int.fromString str of SOME i => i | NONE => ~1
      
      val agent : BR bgbdnf option ref = ref NONE
      val rules : rule list ref = ref []
      (* Each array element is (i, mz, ms), where i is the number of
       * matches already found, and mz the lazy list of remaining
       * matches, and ms is a reversed list of matches (head element
       * is the most recent match).
       *)
      val matches : (int * match lazylist * match list) array ref
        = ref (Array.fromList [])
      
      fun domatch (SOME agentstr, SOME rulesstr,
                   SOME userulesstr, SOME matchcountstr) =
        let
          val _ =
            agent := SOME
                      (BgBDNF.regularize 
                        (BgBDNF.make
                          (BgVal.make
                            BgTerm.info
                              (parseStr BGTERM "agent" agentstr))))
          val _ = rules := parseStr RULES "rules" rulesstr
          val userules = parsenum userulesstr
          val matchcount = parsenum matchcountstr
val _ = TextIO.output (stdErr, "userules = " ^ Int.toString userules 
	^ ", matchcount = " ^ Int.toString matchcount ^ "\n")
          fun mkfmz agent rule
            = (0, Match.matches {agent = agent, rule = rule}, [])
          val fmzs = map (mkfmz (valOf (!agent))) (!rules)
          val (_, rfmzs)
            = foldl
                (fn (mz, (i, rfmzs)) => (i + 1, (i, mz) :: rfmzs))
                (0, [])
                fmzs
          val rfmzs = rev rfmzs
          (* getnext n tomatchzs matchedzs:
           * Get the next n matches (ALL if n < 0) as the first match of
           * each rule in tomatchzs, followed by each rule in (rev matchedzs).
           *)
					fun getnext matchcount
					    ((ruleno, (found, mz, ms)) :: rfmzs) matchedzs =
					    (TextIO.output (stdErr, "mtchcnt=" ^ Int.toString matchcount ^ 
					            ", rulno=" ^ Int.toString ruleno ^
					            ", found=" ^ Int.toString found ^
					            ", rfmzslen=" ^ Int.toString (length rfmzs + 1) ^
					            ", matchedlen=" ^ Int.toString (length matchedzs) ^
					            "\n");
					  if matchcount <> 0 then
					    let
					      val result : match lazycell option ref = ref NONE
              in
						    if interrupted (fn () => result := SOME (lzunmk mz)) then
						      ()
						    else
							  	case !result of
							  	  SOME Nil
							  	  => (Array.update (!matches, ruleno, (found, lzNil, ms));
							  	      print ("NOMOREMATCHES\nrule = "
							  	               ^ Int.toString ruleno
							  	               ^ "\nmatches = "
							  	               ^ Int.toString found
							  	               ^ "\nEND\n");
							  	      if userules < 0 then
							  	        getnext matchcount rfmzs matchedzs
							  	      else
							  	        ())
							  	| SOME (Cons (m, mz'))
							  	  => let
							  	       val {context, parameter, tree, rule} = Match.unmk' m
							  	     in
							  	       Array.update (!matches, ruleno, (found + 1, mz', m :: ms));
		 					  	       print ("RESULT\nrule = "
							  	              ^ Int.toString ruleno
							  	              ^ "\nmatch = "
							  	              ^ Int.toString found
							  	              ^ "\ncontext = "
							  	              ^ BgVal.toString
							  	                  (BgVal.simplify
							  	                     (BgBDNF.unmk context))
							  	              ^ "\nparameter = "
							  	              ^ BgVal.toString
							  	                  (BgVal.simplify
							  	                     (BgBDNF.unmk parameter))
							  	              ^ "\ntree = "
							  	              ^ Match.treeToString tree
							  	              ^ "\nEND\n");
							  	       if userules < 0 then
							  	         getnext
							  	           (matchcount - 1)
							  	           rfmzs
							  	           ((ruleno, (found + 1, mz', m :: ms)) :: matchedzs)
							  	       else
							  	         getnext
							  	           (matchcount - 1)
							  	           ((ruleno, (found + 1, mz', m :: ms)) :: rfmzs)
							   	           matchedzs
							  	     end
							  	| NONE => raise ThisCannotHappen
              end
					  else
					    ())
					  | getnext matchcount [] []
					  = print ("NOMOREMATCHES\nrule = -1"
					  	       ^ "\nmatches = "
					  	       ^ Int.toString
					  	         (Array.foldl
					  	          (fn ((foundi, _, _), total) => total + foundi)
					  	          0
					  	          (!matches))
					  	       ^ "\nEND\n")
					  | getnext matchcount [] matchedzs =
					    getnext matchcount (rev matchedzs) []
        in
          matches := Array.fromList fmzs;
          getnext
            matchcount
            (List.drop (rfmzs, (if userules < 0 then 0 else userules)))
            []
        end
        | domatch (NONE, _, _, _) = log "Agent not specified!"
        | domatch (_, NONE, _, _) = log "Rules not specified!"
        | domatch (_, _, NONE, _) = log "Userules not specified!"
        | domatch (_, _, _, NONE) = log "Matchcount not specified!"
        
      fun doreact (SOME ruleno, SOME matchno) =
        let
          val (found, mz, ms) = Array.sub (!matches, ruleno)
          val {context, parameter, rule, ...}
            = Match.unmk (List.nth (rev ms, matchno))
          val context = BgBDNF.unmk context
          val {react, inst : BG.Instantiation.inst, ...} = Rule.unmk rule
          val agent = BgBDNF.unmk (valOf (!agent))
          (*val _ = print ("Inst: " ^ Instantiation.toString inst ^ "\n")
          val _ = print ("Parameter: " ^ BgBDNF.toString parameter ^ "\n")*)
          val instparam = Instantiation.instantiate inst parameter
          val Z = Interface.glob (BgBDNF.outerface parameter)
          val id_Z = BgVal.Wir noinfo (Wiring.id_X Z)
          val oo = BgVal.Com noinfo
          infix 6 oo
          fun ** (v1, v2) = BgVal.Ten noinfo [v1, v2]
          infix 5 **
          (*val _ = print ("Context: " ^ BgVal.toString context ^ "\n")
          val _ = print ("Instparam: " ^ BgVal.toString instparam ^ "\n")*)
          val newagent = context oo (id_Z ** react) oo instparam
          (*val _ = print ("Newagent: " ^BgVal.toString newagent ^ "\n")*)
        in
          print
            ("NEWAGENT\n" ^ BgVal.toString (BgVal.simplify newagent)
             ^ "\nEND\n")
        end
        | doreact (NONE, _) = log "Ruleno not specified!"
        | doreact (_, NONE) = log "Matchno not specified!"
      
      fun dosimplify (SOME agentstr) =
        let
          val simplifiedagent
            = BgVal.simplify
                (BgBDNF.unmk 
                  (BgBDNF.make
                    (BgVal.make
                      BgTerm.info
                        (parseStr BGTERM "agent" agentstr))))
        in
          print
            ("SIMPLIFIEDAGENT\n" ^ BgVal.toString simplifiedagent
             ^ "\nEND\n")
        end
        | dosimplify NONE = log "Agent not specified!"
      
      fun getuntil endmarker =
        let
          val markerlen = size endmarker
          fun getline s =
		        let
    		      val line
    		        = case inputLineNoBuf () of
    		            SOME s => s | NONE => raise Expected endmarker
        		in
        			if substring (line, 0, markerlen) = endmarker
        		 		handle Subscript => false then
        	  		s
        		else
        	  	getline (s ^ line)
		        end
		    in
		      getline ""
		    end
      
      val colonOrSpc = Char.contains " \t:"

      fun getrest col line =
        let
          val linelen = size line
          fun alnumcol i =
            if i < linelen
              andalso colonOrSpc (String.sub (line, i)) then
              alnumcol (i + 1)
            else
              i
        in
          String.extract (line, alnumcol col, NONE)
        end
      
      fun evalmatch (agent, rules, userules, matchcount) =
      	let
      	  val line
      	    = case inputLineNoBuf () of
      	        SOME s => s | NONE => raise Expected "ENDMATCH"
      	  val upline = String.translate (String.str o Char.toUpper) line
        in
        	if String.isPrefix "AGENT" upline then
        		evalmatch (SOME (getuntil "ENDAGENT"), rules, userules, matchcount)
        	else if String.isPrefix "RULES" upline then
        		evalmatch (agent, SOME (getuntil "ENDRULES"), userules, matchcount)
        	else if String.isPrefix "USERULES" upline then
        		evalmatch (agent, rules, SOME (getrest 8 upline), matchcount)
        	else if String.isPrefix "MATCHCOUNT" upline then
        		evalmatch (agent, rules, userules, SOME (getrest 10 upline))
        	else if String.isPrefix "ENDMATCH" upline then
        		domatch (agent, rules, userules, matchcount)
        	else if null (String.tokens Char.isSpace line) then
        	  evalmatch (agent, rules, userules, matchcount)
        	else
        		log ("Unrecognised input line: " ^ line)
        end
        
      fun evalreact (rule, match) =
      	let
      	  val line
      	    = case inputLineNoBuf () of
      	        SOME s => s | NONE => raise Expected "ENDREACT"
      	  val upline = String.translate (String.str o Char.toUpper) line
        in
        	if String.isPrefix "RULENO" upline then
        		evalreact (Int.fromString (getrest 6 upline), match)
        	else if String.isPrefix "MATCHNO" upline then
        		evalreact (rule, Int.fromString (getrest 7 upline))
        	else if String.isPrefix "ENDREACT" upline then
        		doreact (rule, match)
        	else if null (String.tokens Char.isSpace line) then
        	  evalreact (rule, match)
        	else
        		log ("Unrecognised input line: " ^ line)
        end
      
      fun evalsimplify (agent) =
      	let
      	  val line
      	    = case inputLineNoBuf () of
      	        SOME s => s | NONE => raise Expected "ENDSIMPLIFY"
      	  val upline = String.translate (String.str o Char.toUpper) line
        in
        	if String.isPrefix "AGENT" upline then
        		evalsimplify (SOME (getuntil "ENDAGENT"))
        	else if String.isPrefix "ENDSIMPLIFY" upline then
        		dosimplify (agent)
        	else if null (String.tokens Char.isSpace line) then
        	  evalsimplify (agent)
        	else
        		log ("Unrecognised input line: " ^ line)
        end
      
      fun readevalloop () =
        case inputLineNoBuf () of
          SOME line =>
        	((if (String.substring (line, 0, 5) = "MATCH"
        		    handle Subscript => false)
        		  andalso (size line < 6 orelse
        		           not (Char.isAlpha (String.sub (line, 5)))) then
        		  (evalmatch (NONE, NONE, NONE, NONE))
        		  handle e => BPLwebErrorHandler.explain e
        		  handle _ => ()
        	  else if String.substring (line, 0, 5) = "REACT"
        		  handle Subscript => false 
        		  andalso (size line < 6 orelse
        		           not (Char.isAlpha (String.sub (line, 5)))) then
        		  evalreact (NONE, NONE)
        		  handle e => BPLwebErrorHandler.explain e
        		  handle _ => ()
        	  else if String.substring (line, 0, 8) = "SIMPLIFY"
        		  handle Subscript => false
        		  andalso (size line < 7 orelse
        		           not (Char.isAlpha (String.sub (line, 8)))) then
        		  evalsimplify (NONE)
        		  handle e => BPLwebErrorHandler.explain e
        		  handle _ => ()
        	  else
        	    ());
        	 readevalloop ())
        | NONE => ()
    in
      readevalloop ()
    end
end

structure BG = BG (structure ErrorHandler = BPLwebErrorHandler)

structure BPLwebBack = BPLwebBack
  (structure BG = BG
   structure ErrorHandler = BPLwebErrorHandler
   structure SignalHandler = SignalHandler)

val _
  = ArgParse.parse NONE (Flags.toSpec ())
     (fn s
      => TextIO.output (TextIO.stdErr,
      ("Warning: argument `" ^ s ^ "' ignored.\n")));

(*val _ = BG.RulesLex.UserDeclarations.debug := true;

val _ = Flags.setIntFlag "/debug/level" Debug.LVL_HIGH*)

val _
  = BPLwebBack.run ()
    handle e => BPLwebErrorHandler.explain e handle _ => ();
