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

    open Dyn

    exception Invalid of string
    type 'a flag_info 
      = {name:string,desc:string,short:string,long:string,default:'a}

    type 'a mk_flag = 'a flag_info -> 'a ref

    structure HT = HashTableFn(type hash_key = string
                               val hashVal = HashString.hashString
                               val sameKey = op =)
    exception NotFound
    val registered = HT.mkTable (37, NotFound)

    (* creating flags *)
    fun pack_info brand {name,desc,short,long,default} =
	{name=name,desc=desc,short=short,long=long,default=Dyn.pack brand default}

    val bool_b : bool brand = make_brand()
    val boolref_b : bool ref brand = make_brand()
    fun fromBool t =
	case Dyn.unpack bool_b t of
	    NONE => raise Invalid("Expected boolean")
	  | SOME b => Bool.toString b
    fun toBool s =
	case Bool.fromString s of
	    NONE => raise Invalid("Expected boolean; got " ^ s)
	  | SOME b => Dyn.pack bool_b b
    fun makeBoolFlag (info as {name,default,...}: bool flag_info) = 
	let val r : bool ref = ref default
	in  r before 
	    HT.insert registered 
		(name, (pack boolref_b r, pack_info bool_b info, 
			fromBool, toBool))
	end

    val string_b : string brand = make_brand()
    val stringref_b : string ref brand = make_brand()
    fun fromString t =
	case Dyn.unpack string_b t of
	    NONE => raise Invalid("Expected string")
	  | SOME s => s
    fun toString s = Dyn.pack string_b s
    fun makeStringFlag (info as {name,default,...}: string flag_info) =
	let val r : string ref = ref default
	in  r before 
	    HT.insert registered 
		(name, (pack stringref_b r, pack_info string_b info, 
			fromString, toString))
	end

    (* listing flags *)
    fun listFlags () =
	List.map (fn (_, (_,info,tos,froms)) => (info,tos,froms)) 
		 (HT.listItemsi registered)

    fun listDefaults outstream =
	let fun f ({name,desc,default,...},tos,_) =
		( TextIO.output(outstream, "# " ^ desc ^ "\n")
                ; TextIO.output(outstream, name ^ " = " ^ tos default ^ "\n")
                )
	in  List.app f (listFlags())
	end
(*
    fun listSpec () =
	let fun f (({long,short,..},tos,froms),acc) =
*)	
end (* structure Flags *)
