structure Util :> UTIL =
struct

    exception ShouldntHappen of int

    fun abort code = raise ShouldntHappen code

end (* structure Util *)
