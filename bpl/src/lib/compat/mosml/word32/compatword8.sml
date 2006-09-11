(* Word8 -- new basis 1994-11-01, 1995-04-12, 1996-09-30 *)

(* This unit relies on two's complement representation *)

structure Word8 : WORD = struct

    open Word8

(* AW some additions to "fix" the interface *)

	val toLarge   = toLargeWord
        val toLargeX  = toLargeWordX
        val fromLarge = fromLargeWord
  
end