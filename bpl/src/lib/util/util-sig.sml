signature UTIL = sig
    exception ShouldntHappen of int
    val abort : int -> 'a

    val stringSep : string -> string -> string (* start, finish, sep *)
		      -> ('a -> string) -> 'a list -> string
end
