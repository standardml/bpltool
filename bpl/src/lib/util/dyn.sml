structure Dyn :> Dyn = struct

    type 'a brand = 'a option ref

    datatype action = Fill | Clear
    type t = action -> unit

    fun make_brand () = ref NONE

    fun pack brand value = 
	fn Fill => brand := SOME value
	 | Clear => brand := NONE

    fun unpack brand f = (f Fill; let val r = !brand in f Clear; r end)

    fun null _ = ()

end (* structure Dyn *)

