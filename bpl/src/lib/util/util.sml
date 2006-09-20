structure Util :> UTIL =
struct

    exception ShouldntHappen of int

    fun abort code = raise ShouldntHappen code

    val dump_prefix =
	Flags.makeStringFlag{name="/dump/prefix",default="",
			     short="",long="dump-prefix",
			     desc="Filename prefix for dumps"}

    fun setDumpPrefix filename =
	let open OS.Path
	    val {dir,file} = splitDirFile filename
	    val {base,ext} = splitBaseExt file
	in  dump_prefix := 
	       joinDirFile{dir=dir,file=joinBaseExt{base=base,ext=NONE}}
	end

    fun dumpPretty pp ext x =
	let val os = TextIO.openOut (!dump_prefix ^ "." ^ ext)
	in  Pretty.ppPrint (pp x) (Pretty.plainOutput("(*","*)")) os
          ; TextIO.closeOut os
	end
    
    fun dumpPP pp ext x =
	let val os = TextIO.openOut (!dump_prefix ^ "." ^ ext)
	    val ps = PrettyPrint.mk_ppstream 
		        { consumer = fn s => TextIO.output(os, s)
		        , linewidth = 72
		        , flush = fn () => TextIO.flushOut(os)
		        }
	in  pp 0 ps x
          ; PrettyPrint.flush_ppstream ps
          ; TextIO.closeOut os
        end

end (* structure Util *)
