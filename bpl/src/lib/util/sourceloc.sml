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

structure SourceLocation :> SOURCELOCATION =
struct

  (* ******************************************************************** *)
  (* Report locations in source files                                     *)

    type device = {name: string, input: unit -> char, seek: int -> unit}

    (* stuff taken from mosml location library *)
    fun incr (r as ref v) = r := v+1
    fun decr (r as ref v) = r := v-1

    val prompt = "! "
    local 
	val list : string list ref = ref []
	fun addc c = case !list of [] => list := [String.str c]
				 | s::ss => list := (s ^ String.str c) :: ss
	fun adds s = case !list of [] => list := [s]
				 | s'::ss => list := (s' ^ s) :: ss
    in  fun errNewline () = list := "" :: !list
	val errChr = addc
	val errString = adds
	fun errPrompt s = ( adds prompt ; adds s )
	fun errChrs n c = if n > 0 then (errChr c; errChrs (n-1) c) else ()
									 
	fun getErrList () = rev (!list) before list := []
    end

    fun errLoc (input: unit -> char) (seek: int -> unit)
	       (errorline: string) 
	       ((pos1, pos2): int * int) =
	let val nl = errNewline
	    val chr = errChr
	    val str = errString

	    fun int i = str (Int.toString i)
	    fun prmp s = errPrompt s
	    fun lines char1 char2 charline1 line1 line2 =
		concat [", line ", Int.toString line1,
			if line2<>line1 then "-" ^ Int.toString line2 else "",
			", characters ",
			Int.toString (char1-charline1),"-",
			Int.toString (char2-charline1),":"]

	    fun for f i j = if i > j then () else (f i : unit; for f (i+1) j)

	    fun skipLine () =
		(case input() of #"\^Z" => () | #"\n" => () | _ => skipLine())
		handle Size => ()
	    and copyLine () =
		(case input() of
		     #"\^Z" => raise Size
		   | #"\n" => nl()
		   | c => (chr c; copyLine()))
		handle Size => (str "<EOF>"; nl ())
	    and tr_line first len ch =
		let fun loop f l =
			(case input() of
			     #"\^Z" => raise Size
			   | #"\n" => ()
			   | c =>
			     if f > 0 then
				 (chr(if c = #"\t" then c else #" "); loop (f-1) l)
			     else if l > 0 then
				 (chr(if c = #"\t" then c else ch); loop f (l-1))
			     else ())
			handle Size => errChrs 5 ch
		in loop first len end
	    val pos = ref 0
	    val line1 = ref 1
	    val line1_pos = ref 0
	    val line2 = ref 1
	    val line2_pos = ref 0
	in
	    seek 0;
	    (while !pos < pos1 do
		 (incr pos;
		  case input() of
		      #"\^Z" => raise Size
		    | #"\n" => (incr line1; line1_pos := !pos)
		    | _ => ()))
	    handle Size => ();
	    line2 := !line1;
	    line2_pos := !line1_pos;
	    (while !pos < pos2 do
		 (incr pos;
		  case input() of
		      #"\^Z" => raise Size
		    | #"\n" => (incr line2; line2_pos := !pos)
		    | _ => ()))
	    handle Size => ();
	    str(errorline ^ lines pos1 pos2 (!line1_pos) (!line1) (!line2));
	    nl();
	    if !line1 = !line2 then
		(seek (!line1_pos);
		 prmp ""; copyLine ();
		 seek (!line1_pos);
		 prmp ""; tr_line (pos1 - !line1_pos) (pos2 - pos1) #"^";
		 nl())
	    else
		(
		 seek (!line1_pos);
		 prmp ""; tr_line 0 (pos1 - !line1_pos) #".";
		 seek pos1;
		 copyLine();
		 if !line2 - !line1 <= 16 then
		     (for (fn i => (prmp ""; copyLine()))
			  (!line1 + 1) (!line2 - 1))
		 else
		     (for (fn i => (prmp ""; copyLine()))
			  (!line1 + 1) (!line1 + 3);
		      prmp ".........."; nl();
		      for (fn i => skipLine())
			  (!line1 + 4) (!line2 - 4);
		      for (fn i => (prmp ""; copyLine()))
			  (!line2 - 3) (!line2 - 1));
		     prmp "";
		     (for (fn i => chr(input()))
			  (!line2_pos) (pos2 - 1);
		      tr_line 0 100 #".")
		     handle Size => str "<EOF>";
		     nl()
		);
           getErrList()
	end

    (* end stuff taken from mosml location library *)

    structure SS = Substring
	       
    fun fileDevice filename =
	let val is = TextIO.openIn filename
	    val file  = SS.full(TextIO.inputAll is)
	    val _ = TextIO.closeIn is
	    val pos = ref 0 
	in  { name = "File \"" ^ filename ^ "\""
            , input = fn () => SS.sub(file,!pos) before incr pos
            , seek = fn p => pos := p
            }
	end

    fun ppSourceLocation' {name,input,seek} (pos1,pos2) pps =
	Pretty.break(1,0)
	    (if pos2>0 then
		 Pretty.clist "#" (Pretty.forcemulti o Pretty.ppString)
			      (errLoc input seek name (pos1,pos2))
	     else Pretty.empty,
	     Pretty.clist "#" (Pretty.forcemulti o (Pretty.prefix prompt)) 
			  pps)

    fun ppSourceLocation filename =
	let val device = fileDevice filename
	in  ppSourceLocation' device
	end

end (* structure SourceLoc *)
