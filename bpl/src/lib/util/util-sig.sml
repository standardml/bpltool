signature UTIL = sig
    exception ShouldntHappen of int

    val abort : int -> 'a

end
