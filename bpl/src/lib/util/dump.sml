structure Dump :> DUMP = struct

    val prefix =
	Flags.makeStringFlag{name="/dump/prefix",default="",
			     short="dump",long="dump-prefix",arg="FILE",
			     desc="Filename prefix for pretty print dumps to a file"}
    fun setPrefix filename =
	let open OS.Path
	    val {dir,file} = splitDirFile filename
	    val {base,ext} = splitBaseExt file
	in  prefix := 
	       joinDirFile{dir=dir,file=joinBaseExt{base=base,ext=NONE}}
	end

    fun pretty pp ext x =
	let val os = TextIO.openOut (!prefix ^ "." ^ ext)
	in  Pretty.ppPrint (pp x) (Pretty.plainOutput("(*","*)")) os
          ; TextIO.closeOut os
	end
    
    fun pp pp ext x =
	let val os = TextIO.openOut (!prefix ^ "." ^ ext)
	in  TextIO.output (os, (PrettyPrint.pp_to_string 72 (pp 0) x))
          ; TextIO.flushOut os
          ; TextIO.closeOut os
        end
(* The following only works with older versions of SML/NJ
	let val os = TextIO.openOut (!prefix ^ "." ^ ext)
	    val ps = PrettyPrint.mk_ppstream 
		        { consumer = fn s => TextIO.output(os, s)
		        , linewidth = 72
		        , flush = fn () => TextIO.flushOut(os)
		        }
	in  pp 0 ps x
          ; PrettyPrint.flush_ppstream ps
          ; TextIO.closeOut os
        end
*)

end (* structure Dump *)
