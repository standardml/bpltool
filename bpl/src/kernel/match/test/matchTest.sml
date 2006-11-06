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

(** Testing module for bdnf stuff.
 * @version $LastChangedRevision: 179 $
 *)

functor MatchTest (structure BG : BG
                  where type BgVal.info = int * int
                    sharing type BG.Name.name =
				 BG.NameSet.elt =
				 BG.Link.name =
				 BG.Ion.name
		    sharing type BG.NameSet.Set =
				 BG.Permutation.nameset =
				 BG.Link.nameset =
				 BG.BgVal.nameset
		    sharing type BG.Link.link = BG.LinkSet.elt
		    sharing type BG.LinkSet.Set = 
				 BG.Wiring.linkset
		    sharing type BG.Permutation.permutation =
				 BG.BgVal.permutation
		    sharing type BG.Wiring.wiring = 
				 BG.BgVal.wiring
		    sharing type BG.BgVal.bgval = BG.bgval
		  structure Assert :
			    sig 
			      datatype failure =
				       GeneralFailure of string 
				     | NotEqualFailure of string * string
			      exception Fail of failure
			      val assertEqualString : string -> string -> string 
			    end
		  structure Test :
			    sig
			      type testFunction = unit -> unit
			      type test
			      val labelTests : (string * testFunction) list -> test
			    end
			      ) =
struct
		
open BG
open Assert

  val suite = (fn () => Test.labelTests [])
end