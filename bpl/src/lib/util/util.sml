structure Util :> UTIL =
struct

    exception ShouldntHappen of int
    fun abort code = raise ShouldntHappen code

    local
	fun str _ nil _ _ = ""
	  | str p (h::t) sep needSep =
	    let val s = p h ^ (str p t sep true)
	    in  if needSep then sep ^ s else s
	    end
    in
	fun stringSep start finish sep p l = 
	    start ^ (str p l sep false) ^ finish
    end (* local *)


end (* structure Util *)
