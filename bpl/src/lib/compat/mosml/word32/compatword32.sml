(* $Id: word32.sml,v 1.1 2006/09/02 12:52:34 hniss Exp $
 *
 * Based on original code by Peter Bertelsen, October 1997
 * modified and extended by Andrzej Wasowski, February 2004
 *
 * CAUTION: this implementation of SML Basis Library's Word32 is largely useless,
 *          It is slow, not very well thought through and incomplete. It was made
 *          to satisfy requirements of fxp's code and make it possible to get
 *          fxp running in Moscow ML ASAP. I hope that one day Moscow ML supports
 *          Word32 natively and this workaround will not be needed.
 *)

structure Word32 :> WORD = struct 

open Word8Vector 

val valOf = Option.valOf
val rev   = List.rev

datatype word = WORD of vector

val wordSize = 32

(* for WORD bs, bs[0] is the most significant byte *)

local
    open Word
    infix >> << orb
in
    fun fromWord w =
	let fun h 0w0 w' l = l
	      | h i   w' l =
	        h (i-0w1) (w' >> 0w8) (Word8.fromLargeWord w' :: l)
	in
	    WORD (fromList(h 0w4 w []))
	end

    fun toWord' (b, w) = (w << 0w8) orb Word8.toLargeWord b

    fun toWord (WORD bs) = foldl toWord' 0w0 bs
end

fun fromInt k =
    let fun h 0 k' l = l
	  | h i k' l = h (i-1) (k' div 0x100) (Word8.fromInt k' :: l)
    in
	WORD (fromList(h 4 k []))
    end

fun toInt' k0 bs =
    let fun h (_, b, k) = k * 0x100 + Word8.toInt b
    in
	foldli h k0 (bs, 1, NONE)
    end

fun toInt (WORD bs) =
    let val b0 = sub(bs, 0)
    in
	toInt' (Word8.toInt b0) bs
    end

fun toIntX (WORD bs) =
    let val b0 = sub(bs, 0)
    in
	toInt' (Word8.toIntX b0) bs
    end

fun fromBytes bs =
    if length bs = 4 then SOME (WORD bs)
    else NONE

fun toBytes (WORD bs) = bs

fun compare' x y i =
    let fun h 0 i' = Word8.compare(sub(x, i'), sub(y, i'))
	  | h k i' =
	    (case Word8.compare(sub(x, i'), sub(y, i')) of
		 EQUAL => h (k-1) (i'+1)
	       | res   => res)
    in
	h (3-i) i
    end

fun compare (WORD x, WORD y) = compare' x y 0
    
fun compareX (WORD x, WORD y) =
    let val x0 = sub(x, 0)
	val y0 = sub(y, 0)
    in
	if (Word8.andb(x0, 0wx80) = Word8.andb(y0, 0wx80)) then
	    (case Word8.compare(x0, y0) of
		 EQUAL => compare' x y 1
	       | res   => res)
	else Word8.compare(y0, x0)  (* sign(x) <> sign(y) *)
    end

fun emit out (WORD bs) = app out bs


(* temporarily in MosmlCompat LargeWord = Word *)

val toLarge       = toWord
val toLargeX      = fn _ => raise Fail "Word32.toLargeX FIXME."
val toLargeWord   = toLarge
val toLargeWordX  = toLargeX

val fromLarge     = fromWord
val fromLargeWord = fromLarge

(* LargeInt is Int in MosmlCompat *)

val toLargeInt   = toInt
val toLargeIntX  = toIntX
val fromLargeInt = fromInt

fun mkbop ope (WORD a,WORD b) = WORD (tabulate(4, fn i => ope(sub(a,i),sub(b,i))))
fun mkuop ope (WORD a) = WORD (map ope a)

val andb = mkbop Word8.andb 
val orb  = mkbop Word8.orb
val xorb = mkbop Word8.xorb
val notb = mkuop Word8.notb

fun >> (WORD bs, i)  = 
    let val n = i mod 0wx8
	val s = Word.toInt i div 8
	val e = List.tabulate(s,fn _ => 0wx0) 
	fun shrc (b,(l,c)) = (Word8.orb(c,Word8.>>(b,n))::l, Word8.<<(b,0w8-n))
	val shifted = List.drop(#1 (foldl shrc (e,0w0) bs),s)
    in WORD(fromList (rev shifted)) end 
    	
fun << (WORD bs, i)  = 
    let val n = i mod 0wx8
	val s = Word.toInt i div 8
	val e = List.tabulate(s,fn _ => 0wx0) 
	fun shlc (b,(l,c)) = (Word8.orb(c,Word8.<<(b,n))::l, Word8.>>(b,0w8-n))
	val shifted = List.drop(#1 (foldr shlc (e,0w0) bs),s)
    in WORD(fromList shifted) end 


fun TODO s _ =
    let val msg = "Word32." ^ s ^ " is not implemented!\n"
    in TextIO.output (TextIO.stdErr, msg); raise Fail msg end


val ~>>        = TODO "~>>"
val op+        = TODO "+"
val op-        = TODO "-"
val op*        = TODO "*"
val op div     = TODO "div"
val op mod     = TODO "mod"
val op <       = TODO "<"
val op <=      = TODO "<="
val op >       = TODO ">"
val op >=      = TODO ">="
val min        = TODO "min"
val max        = TODO "max"
val fmt        = TODO "fmt"
val toString   = TODO "toString"
val radix      = TODO "radix"
val fromString = TODO "fromString"

fun scan rdx src = TODO "scan"

(* FIXME: THIS *IS* *SLOW* *)
(* should be val scan       : StringCvt.radix
                   -> (char, 'a) StringCvt.reader
                     -> (word, 'a) StringCvt.reader *)



(* Finish this mess:

   fun scan (rdx :StringCvt.radix) (src :(char, 'a) StringCvt.reader) =
    let fun h 0 state l = (l,state)
	   | h i state l = 
	     let val SOME (c,state1) = Word8.fromInt (Char.ord (src state))
	     in  h (Int.-(i,1)) state1 (c::l) end
	val (state1,bs) = h 4 state []
        val 		
	val vbs = fromList(rev bs)
     in	 SOME (WORD vbs,state1) end handle Option => NONE
*)



end