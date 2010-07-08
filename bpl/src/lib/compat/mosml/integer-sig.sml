(* Moscow ML doesn't include the INTEGER signature from the Standard Library.
 * This copy has been borrowed from SML/NJ. *)
signature INTEGER =
  sig

    eqtype int

    val precision : Int.int option
    val minInt : int option
    val maxInt : int option

    val toLarge   : int -> int
    val fromLarge : int -> int
    val toInt     : int -> Int.int
    val fromInt   : Int.int -> int

    val ~ : int -> int
    val + : int * int -> int
    val - : int * int -> int
    val * : int * int -> int
    val div : int * int -> int
    val mod : int * int -> int
    val quot : int * int -> int
    val rem : int * int -> int

    val min : (int * int) -> int
    val max : (int * int) -> int

    val abs : int -> int

    val sign     : int -> Int.int
    val sameSign : (int * int) -> bool

    val >  : int * int -> bool
    val >= : int * int -> bool
    val <  : int * int -> bool
    val <= : int * int -> bool
    val compare : (int * int) -> order

    val toString   : int -> string
    val fromString : string -> int option
    val scan :
	  StringCvt.radix -> (char, 'a) StringCvt.reader
	    -> (int, 'a) StringCvt.reader
    val fmt  : StringCvt.radix -> int -> string

  end;


