(*
 *   Copyright 2001      Henning Makholm
 *   Copyright 2001,2002 Henning Niss
 * 
 * Permission is hereby granted, to anyone and free of charge, to deal
 * in this software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the software, provided that
 * all copies or substantial portions of the software are accompanied
 * by the above copyright notice and this permission notice.
 * 
 * The software is provided "as is", without warranty of any kind, express
 * or implied, including but not limited to the warranties of
 * merchantability, fitness for a particular purpose and noninfringement.
 * In no event shall the above listed copyright holder(s) be liable for any
 * claim, damages or other liability, whether in an action of contract,
 * tort or otherwise, arising from, out of or in connection with the
 * software or the use or other dealings in the software.
 *)

infixr 5 ^+
infix 4 +^
infixr 4 ++
structure Pretty :> Pretty =
struct

  open PPengine
  open PPbuild

  datatype style = BOLD | ITALICS | SMALL | NORMAL | LARGE | COLOR of string

  type device = { init_spell : string
		, make_indent : int * int -> (style * string) list
		, make_token : style * string -> string
		, newline_spell : string
		, exit_spell : string
		}

  local
    fun mkindent vstyle cmtin cmtout useTab (vindent,indent)
      	= let fun f i = if i <= 0 then []
		      		  else if i >= 8 andalso useTab
 				       then #"\t" :: f (i-8)
				       else #" " :: f (i-1)
	      val spaces = implode o f
	  in (if vindent = 0
 	      then []
	      else let val num = Int.toString(vindent)
		   in [(vstyle,
			concat [ cmtin
			       , spaces(8-size num-size cmtin-size cmtout)
			       , num
			       , cmtout
			       ])]
		   end)
 	     @ [(NORMAL,spaces indent)]
	  end
  in
    fun plainOutput (<*,*>) : device
      = { init_spell = ""
	, make_indent = mkindent NORMAL <* *> true
	, make_token = #2
	, newline_spell = "\n"
	, exit_spell = ""
	}
    fun htmlOutput (<*,*>) : device
	= { init_spell = "<PRE>"
	  , make_indent = mkindent SMALL <* *> false
	  , make_token
 	    = let fun charc #"&" = "&amp;"
		    | charc #"<" = "&lt;"
		    | charc #">" = "&gt;"
		    | charc c = String.str c
		  fun tags BOLD = ("<b>","</b>")
		    | tags ITALICS = ("<i>","</i>")
		    | tags SMALL = ("<font size=\"-1\">","</font>")
		    | tags LARGE = ("<font size=\"+1\">","</font>")
		    | tags NORMAL = ("","")
		    | tags (COLOR c) = ("<font color=\"" ^ c ^ "\">"
					,"</font>")
	      in fn (style,s) => let val (a,b) = tags style
			       	 in a ^ String.translate charc s ^ b
				 end
	      end
	  , newline_spell = "\n"
	  , exit_spell = "</PRE>\n"
	  }
    fun xtermOutput (<*,*>) : device
        = { init_spell = ""
	  , make_indent = mkindent (COLOR "cyan") <* *> true
	  , make_token
 	    = fn (BOLD           ,s) => "\027[1m"  ^ s ^ "\027[m"
	       | (ITALICS        ,s) => "\027[4m"  ^ s ^ "\027[m"
	       | (COLOR "black"  ,s) => "\027[30m" ^ s ^ "\027[m"
	       | (COLOR "red"    ,s) => "\027[31m" ^ s ^ "\027[m"
	       | (COLOR "green"  ,s) => "\027[32m" ^ s ^ "\027[m"
	       | (COLOR "yellow" ,s) => "\027[33m" ^ s ^ "\027[m"
	       | (COLOR "blue"   ,s) => "\027[34m" ^ s ^ "\027[m"
	       | (COLOR "magenta",s) => "\027[35m" ^ s ^ "\027[m"
	       | (COLOR "cyan"   ,s) => "\027[36m" ^ s ^ "\027[m"
	       | (COLOR "white"  ,s) => "\027[37m" ^ s ^ "\027[m"
	       | ( _             ,s) =>              s
	  , newline_spell = "\n"
	  , exit_spell = ""
	  }
    fun lessOutput (<*,*>)
        = { init_spell = ""
	  , make_indent = mkindent NORMAL <* *> true
	  , make_token
 	    = fn (BOLD,s) => (implode
				  (List.concat
				       (map (fn c => [c,#"\008",c])
					    (explode s))))
	       | (ITALICS,s) => (implode
				     (List.concat
					  (map (fn c => [#"_",#"\008",c])
					       (explode s))))
	       | (_,s) => s
	  , newline_spell = "\n"
	  , exit_spell = ""
	  }
  end
  
  type 'x pp = 'x -> style pptree

  fun ppNone _ = empty
  fun ppStar _ = ppString "*"
  fun ppUnit () = ppString "N/A"
  fun ppBool true = ppString "true"
    | ppBool false = ppString "false"
  fun ppInt i = ppString (Int.toString i)
  fun ppOpt pp NONE = empty | ppOpt pp (SOME a) = pp a
  fun ppPair ppA ppB (a,b) = "(" ^+ break(1,0)(ppA a +^ ",", ppB b) +^ ")"
  fun ppList     pp = bracket "(#)"  o  ilist ",# " pp
(*
  fun ppSplaySet pp = bracket "{#}"  o  ilist ",# " pp  o  Splayset.listItems
  fun ppSplayMap pp_key pp_val = bracket "{#}"  o
  				 ilist ",# " (fn(k,v) => (pp_key k +^ ":")
 							 ++ pp_val v) o
				 Splaymap.listItems
*)

  fun ppCAnno ppR (cst,inp,outp)
      = case List.mapPartial
	     (fn (kw,[]) => NONE
	       | (kw,rs) => SOME(kw ^+ ilist ",#" ppR rs))
	     [("c: ",cst),("i: ",inp),("o: ",outp)]
	of [] => ppString "[||]"
	 | groups => bracket "[#]" (clist ";# " (fn t=>t) groups)

  fun ppBinary(t1,ope,t2) = break(1,0)(t1,ope ^+ " " ^+ t2)

  fun ppFold tree (dev:device)
      = let fun ftoken (_,s) = s
	    val lines = format (NORMAL,#1) { vindentpenalty = 30
				       	   , vindentwidth = 71
				       	   , width = 79 }tree
	    fun foldline folder ({vindent,indent,contents},s0)
		= let val s1 = foldl folder s0
			       (map (#make_token dev)
 		    	   	(#make_indent dev (vindent,indent) @ contents))
		      val s2 = folder(#newline_spell dev, s1)
		  in s2 end
	    fun doit folder s0
		= let val s1 = folder(#init_spell dev,s0)
 		      val s2 = foldl (foldline folder) s1 lines
		      val s3 = folder(#exit_spell dev,s2)
		  in s3 end
	in
 	  doit
	end
  fun ppPrint tree dev = let val doit = ppFold tree dev
			 in 
	  		   fn ost => doit (fn (s,()) => TextIO.output(ost,s))
 				     ()
			 end
	
  fun ppToString tree = concat (map #2 (noformat ((),#2) tree))

end (* structure Pretty *)
