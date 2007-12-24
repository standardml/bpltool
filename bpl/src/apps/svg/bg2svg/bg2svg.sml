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
 * @version $LastChangedRevision: 932 $
 *)

functor Bg2SVG (structure BG : BG) =
struct
  open BG
  
  val noinfo = Info.noinfo

  open TextIO

  (** Please implement the PrimTextIO.RD readVec function for stdIn. *)
  exception PleaseImplement

  (** Expected some string in input before EOF. *)
  exception Expected of string

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

  fun getBigraph signaturestr =
    let
      val line
        = case inputLineNoBuf () of
            SOME s => s | NONE => raise Expected "BIGRAPH"
      val upline = String.translate (String.str o Char.toUpper) line
    in
      if String.isPrefix "BIGRAPH" upline then
      	{signaturestr = signaturestr, bigraphstr = getuntil "ENDBIGRAPH"}
      else
        getBigraph signaturestr
    end
      	  
  fun getInput () =
    let
      val line
        = case inputLineNoBuf () of
            SOME s => s | NONE => raise Expected "SIGNATURE"
      val upline = String.translate (String.str o Char.toUpper) line
    in
      if String.isPrefix "SIGNATURE" upline then
      	getBigraph (getuntil "ENDSIGNATURE")
      else
        getInput ()
    end
  
  fun toCDATA s =
    let
      fun to (#"]" :: #"]" :: #">" :: cs)
        = #"]" :: #"]" :: #"&" :: #"g" :: #"t" :: #";" :: to cs
        | to (c :: cs) = c :: to cs
        | to [] = []
    in
      implode (to (explode s))
    end
      	  
  fun run () =
    let
      open BG.PPSVG
      val {signaturestr, bigraphstr} = getInput ()
      val _ = Name.reset ()
      val signatur = parseStr SIGNATURE "signature" signaturestr
      val fixctrl = BgTerm.add1s o BgTerm.replacectrls signatur
      val barebigraph = parseStr BGTERM "bigraph" bigraphstr
      val ctrlfixedbigraph = fixctrl barebigraph
      val bigraph = BgBDNF.make (BgVal.make BgTerm.info ctrlfixedbigraph)
      val svg = makesvg (SOME defaultconfig) bigraph
      val {ctrlcharwidth, ctrlfontheight, ...} =
        unmkconfig defaultconfig ("", [])
      val svgstr =
        svgToString "" "svg:" NONE (ctrlcharwidth, ctrlfontheight) svg ""
      val TikZstr = svgToTikZ 0.02 svg ""
    in
      print svgstr;
      print "\n<div class='source' style='display:none'>\n  <![CDATA[\n";
      print (toCDATA svgstr);
      print ("\n\n" ^ toCDATA TikZstr);
      print "\n]]>\n</div>\n"
    end
end

structure HTMLErrorHandler
  :> ERRORHANDLER
     where type origin      = Origin.origin
       and type ppstream    = PrettyPrint.ppstream
       and type break_style = PrettyPrint.break_style
  =
struct
  open BaseErrorHandler
  open Debug

  fun toHTML #"<" = "&lt;"
    | toHTML #">" = "&gt;"
    | toHTML #"&" = "&amp;"
    | toHTML #"\n" = "<br />\n"
    | toHTML c = String.str c

  fun explain e =
      let
        fun print_exn_hist () =
            case Exception.history e of
              [] => print "\n\nException history unavailable.\n"
            | es => (print "\n\nException history:<br />\n";
                     app (fn e => (print "  ";
                                   print (String.translate toHTML (Exception.event2string e));
                                   print "<br />\n")) es;
                     print "\n")
      in
        (print "<p class='error'>";
         print (String.translate toHTML (explain' e));
         debug LVL_LOW print_exn_hist ();
         print "</p>\n")
      end handle e => raise e
end

structure BG = BG (structure ErrorHandler = HTMLErrorHandler)

structure Bg2SVG = Bg2SVG
  (structure BG = BG
   structure ErrorHandler = HTMLErrorHandler)

val _
  = ArgParse.parse NONE (Flags.toSpec ())
     (fn s
      => TextIO.output (TextIO.stdErr,
      ("Warning: argument `" ^ s ^ "' ignored.\n")));

(*val _ = BG.RulesLex.UserDeclarations.debug := true;

val _ = Flags.setIntFlag "/debug/level" Debug.LVL_HIGH*)

val _
  = Bg2SVG.run ()
    handle e => (HTMLErrorHandler.explain e) handle _ => ();
