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
      = {name:string,desc:string,short:string,long:string,default:'a}


    structure HT 
      = HashTableFn(type hash_key = string
                    val hashVal = HashString.hashString
                    val sameKey = op =)
    exception NotFound
    val registered = HT.mkTable (37, NotFound)

    (* creating flags *)
    type 'a mk_flag = 'a flag_info -> 'a ref
    fun set_default ({name,desc,short,long,default}:'a flag_info) def =
	{name=name,desc=desc,short=short,long=long,default=def}

    fun makeBoolFlag (info as {name,default,...}: bool flag_info) =
	let val r : bool ref = ref default
	in  r before
	    HT.insert registered (name, set_default info (BoolFlag default))
	end

    fun makeStringFlag (info as {name,default,...}: string flag_info) =
	let val r : string ref = ref default
	in  r before
	    HT.insert registered (name, set_default info (StringFlag default))
	end

    (* listing flags *)
    fun list_flags () = List.map #2 (HT.listItemsi registered)

    fun listDefaults outstream = 
	let fun f ({name,desc,default,...}) =
		( TextIO.output(outstream, "# " ^ desc ^ "\n")
                ; TextIO.output(outstream, name ^" = "^ toString default ^ "\n")
                )
	in  List.app f (list_flags())
	end

end (* structure Flags *)
   
