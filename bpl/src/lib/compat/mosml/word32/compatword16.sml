(* Word16.sml
 *
 * Peter Bertelsen
 * October 1997
 *
 * Modified: Andrzej Wasowski, 2004
 *)

structure Word16 :> WORD = struct

open Word8Vector
val valOf = Option.valOf



datatype word = WORD of vector

(* for WORD bs, bs[0] is the most significant byte *)


val wordSize = 16

local
    open Word
    infix >> << orb
in
    fun fromWord w =
	let fun h 0 = Word8.fromLargeWord(w >> 0w8)
	      | h _ = Word8.fromLargeWord w
	in
	    WORD (tabulate(2, h))
	end

    fun toWord (WORD bs) =
	let val b1 = sub(bs, 1)
	    val b0 = sub(bs, 0)
	in
	    (Word8.toLargeWord b0 << 0w8) orb Word8.toLargeWord b1
	end
end

fun fromInt k =
    let fun h 0 = Word8.fromInt(k div 0x100)
	  | h _ = Word8.fromInt k
    in
	WORD (tabulate(2, h))
    end

fun toInt (WORD bs) =
    let val b1 = sub(bs, 1)
	val b0 = sub(bs, 0)
    in
	Word8.toInt b0 * 0x100 + Word8.toInt b1
    end

fun toIntX (WORD bs) =
    let val b1 = sub(bs, 1)
	val b0 = sub(bs, 0)
    in
	Word8.toIntX b0 * 0x100 + Word8.toInt b1
    end

(* temporarily in MosmlCompat LargeWord = Word *)

val toLarge       = toWord
val toLargeX      = fn w => LargeWord.fromInt(toIntX w) (* FIXME: is this correct? *)
val toLargeWord   = toLarge
val toLargeWordX  = toLargeX

val fromLarge     = fromWord
val fromLargeWord = fromLarge

(* LargeInt is Int in MosmlCompat *)

val toLargeInt   = toInt
val toLargeIntX  = toIntX
val fromLargeInt = fromInt

(* FIXME: More efficient? *)
fun mkbop ope (WORD a,WORD b) = WORD (tabulate(2, fn i => ope(sub(a,i),sub(b,i))))
fun mkuop ope (WORD a) = WORD (map ope a)

val andb = mkbop Word8.andb 
val orb  = mkbop Word8.orb
val xorb = mkbop Word8.xorb
val notb = mkuop Word8.notb

val <<     = raise Fail "<< Not implemented!"
val ~>>    = raise Fail "~<< Not implemented!"
val >>     = raise Fail ">> Not implemented!"
val ~>>    = raise Fail "~>> Not implemented!"
val op+    = raise Fail "+   Not implemented!"
val op-    = raise Fail "-   Not implemented!"
val op*    = raise Fail "*   Not implemented!"
val op div = raise Fail "div Not implemented!"
val op mod = raise Fail "mod Not implemented!"
val op <   = raise Fail "<  Not implemented!"
val op <=  = raise Fail "<= Not implemented!"
val op >   = raise Fail "<  Not implemented!"
val op >=  = raise Fail ">= Not implemented!"
val min    = raise Fail "min Not implemented!"
val max    = raise Fail "maz not implemented!"
val fmt    = raise Fail "fmt not implemented!"
val toString = raise Fail "toString not implemented!"
val radix    = raise Fail "radix not implemented!"
val fromString = raise Fail "fromString not implemented!"

fun fromBytes bs =
    if length bs = 2 then SOME (WORD bs)
    else NONE
	
fun toBytes (WORD bs) = bs

fun compare (WORD x, WORD y) =
    let val x0 = sub(x, 0)
	val y0 = sub(y, 0)
    in
	case Word8.compare(x0, y0) of
	    EQUAL => Word8.compare(sub(x, 1), sub(y, 1))
	  | res   => res
    end

fun compareX (WORD x, WORD y) =
    let val x0 = sub(x, 0)
	val y0 = sub(y, 0)
    in
	if (Word8.andb(x0, 0wx80) = Word8.andb(y0, 0wx80)) then
	    (case Word8.compare(x0, y0) of
		 EQUAL => Word8.compare(sub(x, 1), sub(y, 1))
	       | res   => res)
	else Word8.compare(y0, x0)   (* sign(x) <> sign(y) *)
    end

fun emit out (WORD bs) = app out bs

(* FIXME: the basis library has a different interface now:
fun scan src =
    (let val bs = fromList [valOf(src()), valOf(src())]
     in
	 SOME (WORD bs)
     end) handle Option => NONE *)

val scan = raise Fail "scan not implemented!"

end