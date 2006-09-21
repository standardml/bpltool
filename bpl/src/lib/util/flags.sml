(* Copyright (c) 2006  The BPL Group at the IT University of Copenhagen
 *
 * This file is part of BPL.
 *
 * BPL is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 *
 * BPL is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with BPL; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
 * USA
 *)

structure Flags :> FLAGS = struct

    datatype flag_type =
	IntFlag of int
      | BoolFlag of bool
      | StringFlag of string
      | RealFlag of real

    fun toString ft =
	case ft of
	    IntFlag i => Int.toString i
	  | RealFlag r => Real.toString r
	  | StringFlag s => s
	  | BoolFlag b => Bool.toString b

    type 'a flag_info 
      = {name:string,desc:string,
	 short:string,long:string,arg:string,
	 default:'a}


    structure HT 
      = HashTableFn(type hash_key = string
                    val hashVal = HashString.hashString
                    val sameKey = op =)

    exception FlagNotFound
    val registered : (Dyn.t * flag_type flag_info) HT.hash_table
     = HT.mkTable (37, FlagNotFound)

    (* creating flags *)
    fun set_default ({name,desc,short,long,default,arg}:'a flag_info) def =
	{name=name,desc=desc,short=short,long=long,arg=arg,default=def}
    fun mk (info as {name,...}:'a flag_info) dref def =
	HT.insert registered (name, (dref, set_default info def))

    val intref_b : int ref Dyn.brand = Dyn.make_brand()
    fun makeIntFlag (info as {default,...}: int flag_info) =
	let val r : int ref = ref default
	    val r' = Dyn.pack intref_b r
	in  r before mk info r' (IntFlag default)
	end

    val realref_b : real ref Dyn.brand = Dyn.make_brand()
    fun makeRealFlag (info as {name,default,...}: real flag_info) =
	let val r : real ref = ref default
	    val r' = Dyn.pack realref_b r
	in  r before mk info r' (RealFlag default)
	end

    val boolref_b : bool ref Dyn.brand = Dyn.make_brand()
    fun makeBoolFlag (info as {name,default,...}: bool flag_info) =
	let val r : bool ref = ref default
	    val r' = Dyn.pack boolref_b r
	in  r before mk info r' (BoolFlag default)
	end

    val stringref_b : string ref Dyn.brand = Dyn.make_brand()
    fun makeStringFlag (info as {name,default,...}: string flag_info) =
	let val r : string ref = ref default
	    val r' = Dyn.pack stringref_b r
	in  r before mk info r' (StringFlag default)
	end

    fun setIntFlag name int =
	let val (r', info) = HT.lookup registered name
	    val r = case Dyn.unpack intref_b r' of
			NONE => Util.abort 98770
		      | SOME r => r
	in  r := int
	end

    fun setRealFlag name real =
	let val (r', info) = HT.lookup registered name
	    val r = case Dyn.unpack realref_b r' of
			NONE => Util.abort 98771
		      | SOME r => r
	in  r := real
	end

    fun setBoolFlag name bool =
	let val (r', info) = HT.lookup registered name
	    val r = case Dyn.unpack boolref_b r' of
			NONE => Util.abort 98772
		      | SOME r => r
	in  r := bool
	end

    fun setStringFlag name str =
	let val (r', info) = HT.lookup registered name
	    val r = case Dyn.unpack stringref_b r' of
			NONE => Util.abort 98773
		      | SOME r => r
	in  r := str
	end

    
    fun atDefault dref def =
	case def of
	    IntFlag i => ( case Dyn.unpack intref_b dref of
			       NONE => false
			     | SOME rf => !rf = i
                         )
	  | RealFlag r => ( case Dyn.unpack realref_b dref of
			       NONE => false
			     | SOME rf => Real.== (!rf, r)
                         )
	  | BoolFlag b => ( case Dyn.unpack boolref_b dref of
			       NONE => false
			     | SOME rf => !rf = b
                         )
	  | StringFlag s => ( case Dyn.unpack stringref_b dref of
				  NONE => false
				| SOME rf => !rf = s
                            )


    (* listing flags *)
    fun list_flags () = List.map #2 (HT.listItemsi registered)
    fun only_changed flags = 
	let fun f (r',info as {default,...} : flag_type flag_info) = not(atDefault r' default)
	in  List.filter f flags
	end
    fun with_option flags =
	let fun f (_, {long="",short="",...} : flag_type flag_info) = false
	      | f _ = true
	in  List.filter f flags
	end

    fun output outstream flags = 
	let fun f (_,{name,desc,default,...}: flag_type flag_info) =
		( TextIO.output(outstream, "# " ^ desc ^ "\n")
                ; TextIO.output(outstream, name ^" = "^ toString default ^ "\n")
                )
	in  List.app f flags
	end

    fun listDefaults outstream = output outstream (list_flags())
    fun listChanged outstream = output outstream (only_changed (list_flags()))

    fun clr r () = r := false
    fun set r () = r := true
    fun toSpec dref def =
	case def of
	    IntFlag i => ( case Dyn.unpack intref_b dref of
			       NONE => Util.abort 5540
			     | SOME rf => ArgParse.Int(fn i => rf := i)
                         )
	  | RealFlag r => ( case Dyn.unpack realref_b dref of
			       NONE => Util.abort 5541
			     | SOME rf => ArgParse.Real(fn r => rf := r)
                         )
	  | BoolFlag b => ( case Dyn.unpack boolref_b dref of
			       NONE => Util.abort 5542
			     | SOME rf => ArgParse.Unit(if b then clr rf
							else set rf)
                         )
	  | StringFlag s => ( case Dyn.unpack stringref_b dref of
				  NONE => Util.abort 5543
				| SOME rf => ArgParse.String(fn s => rf := s)
                            )

    fun options () =
	let fun f (dref, {name,short,long,desc,default,arg}) =
		( (if short = "" then [] else ["-"^short]) @
                  (if long  = "" then [] else ["--"^long])
                , arg
                , desc
                , toSpec dref default
                )
	in  List.map f (with_option (list_flags()))
	end

    fun mk_opt (opts, "", desc, spec) = 
	(Util.stringSep "" "" ", " (fn s=>s) opts, desc)
      | mk_opt (opts, arg, desc,spec) = 
	(Util.stringSep "" "" ", " (fn s=>s^" "^arg) opts, desc)

    fun mk_table opts =
	let val max = List.foldl (fn ((opt,dsc),m) => if String.size opt > m 
						      then String.size opt
						      else m)
		                 0 opts
	in  List.map (fn (opt,desc) => StringCvt.padRight #" " (max+2) opt ^ desc)
	             opts
	end

    fun usage () =
	let val opts = options ()
	in  mk_table (List.map mk_opt opts)
	end

end (* structure Flags *)

