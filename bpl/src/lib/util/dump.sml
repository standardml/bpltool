structure Dump :> DUMP = struct

    val prefix =
	Flags.makeStringFlag{name="/dump/prefix",default="",
			     short="d",long="dump-prefix",arg="FILE",
			     desc="Set filename prefix for dumps to FILE"}
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
	    val ps = PrettyPrint.mk_ppstream 
		        { consumer = fn s => TextIO.output(os, s)
		        , linewidth = 72
		        , flush = fn () => TextIO.flushOut(os)
		        }
	in  pp 0 ps x
          ; PrettyPrint.flush_ppstream ps
          ; TextIO.closeOut os
        end

end (* structure Dump *)
