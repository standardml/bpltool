signature UTIL = sig
    exception ShouldntHappen of int

    val abort : int -> 'a

    val dump_prefix : string ref
    val setDumpPrefix : string (* filename *) -> unit
    val dumpPretty : 'a Pretty.pp -> string (* extension *) -> 'a -> unit
    val dumpPP : (int -> PrettyPrint.ppstream -> 'a -> unit)
                     -> string (* extension *) -> 'a -> unit
end
