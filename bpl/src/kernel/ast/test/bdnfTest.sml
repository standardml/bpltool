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
 * @version $Revision: 1.16 $
 *)

functor BDNFTest (structure BG : BG
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
     
datatype expectedresult = S of string | E of string

  (* remove whitespace characters *)
  fun removeSpaces s =
      String.concat (String.tokens Char.isSpace s)

  fun testfile get (S expectedstring) filename =
      (assertEqualString
         (removeSpaces expectedstring)
         (removeSpaces (get filename)); ())
    | testfile get (E expectedexnname) filename = 
      (((get filename; 
	 raise Assert.Fail
		 (GeneralFailure ("expected exception "
				  ^ expectedexnname)))
	handle exn => 
	       (if exnName exn = expectedexnname then
		  ()
		else
		  (raise Assert.Fail
		     (GeneralFailure 
			("expected exception " ^
			 expectedexnname ^ ", got " ^
			 exnName exn)))));
       ())
      
  val suite =
       let
	 fun testModule getfile getexpected =
	     map 
	       (fn (txt, expected1, expected2, file)
		   => (txt, 
		       (fn () =>
			   testfile 
			     getfile
			     (getexpected (expected1, expected2))
			     (file ^ ".bg"))))
	[("00-0: barren root", S"1", 
	  S"(idw_0 * idp_1)\
           \ (idw_0\
           \  * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \     (({})(idw_0 * 1) idx_0) idp_0)", "00-0"),
         ("01-0: merge with 0 sites", S"1", 
	  S"(idw_0 * idp_1)\
           \ (idw_0\
           \  * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \     (({})(idw_0 * 1) idx_0) idp_0)", "01-0"),
         ("01-1: merge with 1 site", S"merge_1", 
	  S"(idw_0 * idp_1)\
           \ (idw_0\
           \  * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \     (({})(idw_0 * merge_1)\
           \           ((idw_0 * idp_1) '{}'))\
           \     idp_1)", "01-1"),
         ("01-2: merge with >1 sites", S"merge_2", 
	  S"(idw_0 * idp_1)\
           \ (idw_0\
           \  * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \     (({})(idw_0 * merge_2)\
           \           ((idw_0 * idp_1) '{}'\
           \            * (idw_0 * idp_1) '{}'))\
           \     idp_2)", "01-2"),
         ("02-0: concretion with 0 names", S"'{}'", 
	  S"(idw_0 * idp_1)\
           \ (idw_0\
           \  * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \     (({})(idw_0 * merge_1)\
           \           ((idw_0 * idp_1) '{}'))\
           \     idp_1)", "02-0"),
         ("02-1: concretion with 1 name", S"'{x}'", 
	  S"(x/x * idp_1)\
           \ (idw_0\
           \  * (x/x * ({})(idw_0 * idp_1) '{}')\
           \     (({})(x/x * merge_1)\
           \           ((x/x * idp_1) '{x}'))\
           \     [0{x}])", "02-1"),
         ("02-2: concretion with >1 names", S"'{x, y}'", 
	  S"(idw_{x, y} * idp_1)\
           \ (idw_0\
           \  * (idw_{x, y} * ({})(idw_0 * idp_1) '{}')\
           \     (({})(idw_{x, y} * merge_1)\
           \           ((idw_{x, y} * idp_1) '{x, y}'))\
           \     [0{x, y}])", "02-2"),
         ("02-3: concretion with duplicate name",
          E"DuplicateNames", E"DuplicateNames", "02-3"),
         ("02-4: concretion composition", S"'{x}' [0{x}]", 
	  S"(x/x * idp_1)\
           \ (idw_0\
           \  * (x/x * ({})(idw_0 * idp_1) '{}')\
           \     (({})(x/x * merge_1)\
           \           ((x/x * idp_1) '{x}'))\
           \     [0{x}])", "02-4"),
         ("03-0: wiring with 0/0 names", S"/{}",
          S"(/{} * idp_0) (idw_0 * idx_0 idp_0)", "03-0"),
         ("03-1: wiring with 0/1 name", S"/x",
          S"(/x * idp_0) (x/x * idx_0 idp_0)", "03-1"),
         ("03-2: wiring with 0/>1 names", S"/{x, y}", 
	  S"(/{x, y} * idp_0) (idw_{x, y} * idx_0 idp_0)", "03-2"),
         ("03-3: wiring with duplicate names",
          E"DuplicateNames", E"DuplicateNames", "03-3"),
         ("03-4: wiring with 1/0 names", S"y/{}",
          S"(y/{} * idp_0) (idw_0 * idx_0 idp_0)", "03-4"),
         ("03-5: wiring renaming with 1/1 name", S"y/x",
          S"(y/x * idp_0) (x/x * idx_0 idp_0)", "03-5"),
         ("03-6: wiring identity", S"x/x",
          S"(x/x * idp_0) (x/x * idx_0 idp_0)", "03-6"),
         ("03-7: wiring with 1/>1 name", S"y/{x, y}", 
	  S"(y/{x, y} * idp_0) (idw_{x, y} * idx_0 idp_0)", "03-7"),
         ("03-8: wiring with duplicate name",
          E"DuplicateNames", E"DuplicateNames", "03-8"),
         ("04-0-0: Ion with 0/0 names/sets", S"K", 
	  S"(idw_0 * idp_1)\
           \ (idw_0\
           \  * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \     (({})\
           \      (idw_0 * merge_1)\
           \       ((idw_0 * K)\
           \         ({})(idw_0 * merge_1) ((idw_0 * idp_1) '{}')))\
           \     idp_1)", "04-0-0"),
         ("04-0-1: ion with 0/1(0) names/sets(elements)", S"K<><{}>", 
	  S"(idw_0 * idp_1)\
           \ (idw_0\
           \  * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \     (({})\
           \      (idw_0 * merge_1)\
           \       ((idw_0 * K<><{}>)\
           \        ({})(idw_0 * merge_1) ((idw_0 * idp_1) '{}')))\
           \     idp_1)", "04-0-1"),
         ("04-0-2: ion with 0/1(1) names/sets(elements)", S"K<><{x}>", 
	  S"(idw_0 * idp_1)\
           \ (idw_0\
           \  * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \     (({})\
           \      (idw_0 * merge_1)\
           \       ((idw_0 * K<><{x}>)\
           \        ({x})(x/x * merge_1) ((x/x * idp_1) '{x}')))\
           \     [0{x}])", "04-0-2"),
         ("04-0-3: ion with 0/1(>1) names/sets(elements)", S"K<><{x, y}>",
	  S"(idw_0 * idp_1)\
           \ (idw_0\
           \  * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \     (({})\
           \      (idw_0 * merge_1)\
           \       ((idw_0 * K<><{x, y}>)\
           \        ({x, y})\
           \         (idw_{x, y} * merge_1)\
           \          ((idw_{x, y} * idp_1) '{x, y}')))\
           \     [0{x, y}])", "04-0-3"),
         ("04-0-4: ion with 0/duplicate names",
          E"DuplicateNames", E"DuplicateNames", "04-0-4"),
         ("04-0-5: ion with 0/>1(0) names/sets(elements)",
          S"K<><{}, {}>",
	  S"(idw_0 * idp_1)\
           \ (idw_0\
           \  * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \     (({})\
           \      (idw_0 * merge_1)\
           \       ((idw_0 * K<><{}, {}>)\
           \        ({})(idw_0 * merge_1) ((idw_0 * idp_1) '{}')))\
           \     idp_1)", "04-0-5"),
         ("04-0-6: ion with 0/>1(>0) names/sets(elements)",
          S"K<><{x}, {y, z}>", 
	  S"(idw_0 * idp_1)\
           \ (idw_0\
           \  * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \     (({})\
           \      (idw_0 * merge_1)\
           \       ((idw_0 * K<><{x}, {y, z}>)\
           \        ({x, y, z})\
           \         (idw_{x, y, z} * merge_1)\
           \          ((idw_{x, y, z} * idp_1) '{x, y, z}')))\
           \     [0{x, y, z}])", "04-0-6"),
         ("04-0-7: ion with 0/>1(>0) names/sets(duplicate elements)",
          E"DuplicateNames", E"DuplicateNames", "04-0-7"),
         ("04-1-0: Ion with 1/0 names/sets", S"K<x>", 
	  S"(x/x * idp_1)\
           \ (idw_0\
           \  * (x/x * ({})(idw_0 * idp_1) '{}')\
           \     (({})\
           \      (x/x * merge_1)\
           \       ((idw_0 * K<x>)\
           \         ({})(idw_0 * merge_1) ((idw_0 * idp_1) '{}')))\
           \     idp_1)", "04-1-0"),
         ("04-1-1: ion with 1/1(>1) names/sets(elements)",
          S"K<x><{x, y}>", 
	  S"(x/x * idp_1)\
           \ (idw_0\
           \  * (x/x * ({})(idw_0 * idp_1) '{}')\
           \     (({})\
           \      (x/x * merge_1)\
           \       ((idw_0 * K<x><{x, y}>)\
           \        ({x, y})\
           \         (idw_{x, y} * merge_1)\
           \          ((idw_{x, y} * idp_1) '{x, y}')))\
           \     [0{x, y}])", "04-1-1"),
         ("04-2-0: ion with >1/>1(>1) names/sets(elements)",
          S"K<x, y><{y, z}, {}>", 
	  S"(idw_{x, y} * idp_1)\
           \ (idw_0\
           \  * (idw_{x, y} * ({})(idw_0 * idp_1) '{}')\
           \     (({})\
           \      (idw_{x, y} * merge_1)\
           \       ((idw_0 * K<x, y><{y, z}, {}>)\
           \        ({y, z})\
           \         (idw_{y, z} * merge_1)\
           \          ((idw_{y, z} * idp_1) '{y, z}')))\
           \     [0{y, z}])", "04-2-0"),
         ("04-2-1: ion with duplicate/>1(?) names/sets(elements)",
          E"DuplicateNames", E"DuplicateNames", "04-2-1"),
         ("05-0-0: Permutation of width 0", S"idp_0",
          S"(idw_0 * idp_0) (idw_0 * idx_0 idp_0)", "05-0-0"),
         ("05-1-0: Permutation of width(elements) 1(0)", S"idp_1", 
	  S"(idw_0 * idp_1)\
           \ (idw_0\
           \  * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \     (({})(idw_0 * merge_1) ((idw_0 * idp_1) '{}'))\
           \     idp_1)", "05-1-0"),
         ("05-1-1: Permutation of width(elements) 1(>0)", S"[0{x}]", 
	  S"(idw_0 * [0{x}])\
           \ (idw_0\
           \  * (idw_0 * ({x})(x/x * idp_1) '{x}')\
           \     (({x})(x/x * merge_1) ((x/x * idp_1) '{x}'))\
           \     [0{x}])", "05-1-1"),
         ("05-2-0: identity Permutation of width(elements) >1(>0)",
          S"[0{x}, 1, 2{y, z}]", 
	  S"(idw_0 * [0{x}, 1, 2{y, z}])\
           \ (idw_0\
           \  * ((idw_0 * ({x})(x/x * idp_1) '{x}')\
           \      (({x})(x/x * merge_1) ((x/x * idp_1) '{x}'))\
           \     * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \        (({})(idw_0 * merge_1) ((idw_0 * idp_1) '{}'))\
           \     * (idw_0 * ({y, z})(idw_{y, z} * idp_1) '{y, z}')\
           \        ({y, z})\
           \         (idw_{y, z} * merge_1)\
           \          ((idw_{y, z} * idp_1) '{y, z}'))\
           \     [0{x}, 1, 2{y, z}])", "05-2-0"),
         ("05-2-1: Permutation of width(elements) >1(>0)",
          S"[2{x}, 0, 1{y, z}]", 
	  S"(idw_0 * [0, 1{y, z}, 2{x}])\
           \ (idw_0\
           \  * ((idw_0 * ({})(idw_0 * idp_1) '{}')\
           \      (({})(idw_0 * merge_1) ((idw_0 * idp_1) '{}'))\
           \     * (idw_0 * ({y, z})(idw_{y, z} * idp_1) '{y, z}')\
           \        (({y, z})\
           \         (idw_{y, z} * merge_1)\
           \          ((idw_{y, z} * idp_1) '{y, z}'))\
           \     * (idw_0 * ({x})(x/x * idp_1) '{x}')\
           \        ({x})(x/x * merge_1) ((x/x * idp_1) '{x}'))\
           \     [2{x}, 0, 1{y, z}])", "05-2-1"),
         ("05-2-2: Permutation with duplicate names",
          E"DuplicateNames", E"DuplicateNames", "05-2-2"),
         ("05-2-3: Permutation with duplicate names",
          E"DuplicateNames", E"DuplicateNames", "05-2-3"),
         ("05-2-4: ill-formed permutation",
          E"NotPermutation", E"NotPermutation", "05-2-4"),
         ("06-0-0: abstraction of 0 of 0 names", S"({})1", 
	  S"(idw_0 * idp_1)\
           \ (idw_0\
           \  * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \     (({})(idw_0 * 1) idx_0)\
           \  idp_0)", "06-0-0"),
         ("06-0-1: abstraction of 0 of >0 names", S"({})K<y>", 
	  S"(y/y * idp_1)\
           \ (idw_0\
           \  * (y/y * ({})(idw_0 * idp_1) '{}')\
           \     (({})\
           \      (y/y * merge_1)\
           \       ((idw_0 * K<y>)\
           \         ({})(idw_0 * merge_1) ((idw_0 * idp_1) '{}')))\
           \     idp_1)", "06-0-1"),
         ("06-0-2: abstraction of bigraph of width >1",
          E"NotPrime", E"NotPrime", "06-0-2"),
         ("06-1-0: abstraction of >0 of 0 names",
          E"NameMissing", E"NameMissing", "06-1-0"),
         ("06-1-1: abstraction of >0 of >0 names", S"({x})'{x, y}'", 
	  S"(y/y * [0{x}])\
           \ (idw_0\
           \  * (y/y * ({x})(x/x * idp_1) '{x}')\
           \     (({x})(idw_{x, y} * merge_1)\
           \            ((idw_{x, y} * idp_1) '{x, y}'))\
           \     [0{x, y}])", "06-1-1"),
         ("06-1-2: abstraction of bigraph of width 0",
          E"NotPrime", E"NotPrime", "06-1-2"),
         ("06-1-3: abstraction of bigraph of width 1",
          S"K L<><{x}>",
	  S"(idw_0 * idp_1)\
	   \ (idw_0\
	   \  * (idw_0 * ({})(idw_0 * idp_1) '{}')\
	   \     (({})\
	   \      (idw_0 * merge_1)\
	   \       ((idw_0 * K)\
	   \        ({})\
	   \         (idw_0 * merge_1)\
	   \          ((idw_0 * L<><{x}>)\
	   \           ({x})(x/x * merge_1) ((x/x * idp_1) '{x}')))) [0{x}])",
	  "06-1-3"),
         ("06-1-4: normalisation and composition",
          S"K<><{x}> ({x})L * x/{}",
	  S"(idw_0 * idp_1)\
	   \ (idw_0\
	   \  * (idw_0 * ({})(idw_0 * idp_1) '{}')\
	   \     (({})\
	   \       (idw_0 * merge_1)\
	   \        ((idw_0 * K<><{}>)\
	   \          ({})\
	   \           (idw_0 * merge_1)\
	   \            ((idw_0 * L)\
	   \              ({})\
	   \               (idw_0 * merge_1) ((idw_0 * idp_1) '{}'))))\
	   \     idp_1)",
	  "06-1-4"),
         ("07-0-0: tensor product of bigraphs of width 0/0",
          S"x/{x, y} * y/z * z/{} * /w", 
	  S"((x/{x, y} * y/z * z/{} * /w) * idp_0)\
           \ (idw_{w, x, y, z} * idx_0 idp_0)", "07-0-0"),
         ("07-0-1: tensor product of bigraphs of width 0/>0",
          S"x/{x, y} * y/z * [1, 0{w}]", 
	  S"((x/{x, y} * y/z) * [0{w}, 1])\
           \ (idw_{x, y, z}\
           \  * ((idw_0 * ({w})(w/w * idp_1) '{w}')\
           \      (({w})(w/w * merge_1) ((w/w * idp_1) '{w}'))\
           \     * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \        ({})(idw_0 * merge_1)\
           \             ((idw_0 * idp_1) '{}'))\
           \     [1, 0{w}])", "07-0-1"),
         ("07-1-0: tensor product of bigraphs of width >0/0",
          S"[1, 0{w}] * x/{x, y} * y/z",
	  S"((x/{x, y} * y/z) * [0{w}, 1])\
           \ (idw_{x, y, z}\
           \  * ((idw_0 * ({w})(w/w * idp_1) '{w}')\
           \      (({w})(w/w * merge_1) ((w/w * idp_1) '{w}'))\
           \     * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \        ({})(idw_0 * merge_1)\
           \             ((idw_0 * idp_1) '{}'))\
           \     [1, 0{w}])", "07-1-0"),
         ("07-1-1: tensor product of bigraphs of width >0/>0",
          S"[1, 0{w}] * [2{x}, 0, 1{y, z}]", 
	  S"(idw_0 * [0{w}, 1, 2, 3{y, z}, 4{x}])\
           \ (idw_0\
           \  * ((idw_0 * ({w})(w/w * idp_1) '{w}')\
           \      (({w})(w/w * merge_1) ((w/w * idp_1) '{w}'))\
           \     * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \        (({})(idw_0 * merge_1) ((idw_0 * idp_1) '{}'))\
           \     * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \        (({})(idw_0 * merge_1) ((idw_0 * idp_1) '{}'))\
           \     * (idw_0 * ({y, z})(idw_{y, z} * idp_1) '{y, z}')\
           \        (({y, z})\
           \         (idw_{y, z} * merge_1)\
           \          ((idw_{y, z} * idp_1) '{y, z}'))\
           \     * (idw_0 * ({x})(x/x * idp_1) '{x}')\
           \        ({x})(x/x * merge_1) ((x/x * idp_1) '{x}'))\
           \     [1, 0{w}, 4{x}, 2, 3{y, z}])", "07-1-1"),
         ("07-2-0: tensor product with global outer name clash",
          E"NotTensorable", E"NotTensorable", "07-2-0"),
         ("07-2-1: tensor product with global inner name clash",
          E"NotTensorable", E"NotTensorable", "07-2-1"),
         ("07-2-2: tensor product with local outer name clash",
          E"NotTensorable", E"NotTensorable", "07-2-2"),
         ("07-2-3: tensor product with local inner name clash",
          E"NotTensorable", E"NotTensorable", "07-2-3"),
         ("07-2-4: tensor product with local/global inner name clash",
          E"NotTensorable", E"NotTensorable", "07-2-4"),
         ("07-2-5: tensor product with local/global outer name clash",
          E"NotTensorable", E"NotTensorable", "07-2-5"),
         ("08-0-0: composition of bigraphs of width 0/0",
          S"x/{x, y} (y/z * x/{})", 
	  S"(x/z * idp_0) (z/z * idx_0 idp_0)", "08-0-0"),
         ("08-1-0: composition of bigraphs of width >0/0",
          S"(1 * x/{y, z}) (y/z * z/{})", 
	  S"(x/z * idp_1)\
           \ (z/z\
           \  * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \     (({})(idw_0 * 1) idx_0)\
           \     idp_0)", "08-1-0"),
         ("08-1-1: composition of bigraphs of width >0/>0", 
	  S"('{x, y}' * /w * K<z><{z}, {u, v}>)\
	   \ ((({x, y})L<x, y, w>) * ({u, v, z})M<u, v, z>)", 
	  S"((x/x * y/y * z/z * /w) * idp_2)\
           \ (idw_0\
           \  * ((idw_{w, x, y} * ({})(idw_0 * idp_1) '{}')\
           \      (({})\
           \       (idw_{w, x, y} * merge_1)\
           \        ((idw_0 * L<w, x, y>)\
           \         ({})(idw_0 * merge_1) ((idw_0 * idp_1) '{}')))\
           \     * (z/z * ({})(idw_0 * idp_1) '{}')\
           \        ({})\
           \         (z/z * merge_1)\
           \          ((idw_0 * K<z><{z}, {u, v}>)\
           \           ({u, v, z})\
           \            (idw_{u, v, z} * merge_1)\
           \             ((idw_0 * M<u, v, z>)\
           \              ({})(idw_0 * merge_1)\
           \                   ((idw_0 * idp_1) '{}'))))\
           \     idp_2)", "08-1-1"),
         ("08-1-2: composition of inner/outer width 0/>0",
          E"NotComposable", E"NotComposable", "08-1-2"),
         ("08-1-3: composition of inner/outer width >0/0",
          E"NotComposable", E"NotComposable", "08-1-3"),
         ("08-1-4: composition of inner/outer width >0 <> >0",
          E"NotComposable", E"NotComposable", "08-1-4"),
         ("08-1-5: composition of bigraph with barren root", S"'{}' 1", 
	  S"(idw_0 * idp_1)\
           \ (idw_0\
           \  * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \     (({})(idw_0 * 1) idx_0) idp_0)", "08-1-5"),
         ("08-2-0: composition with inner/outer local name set sizes 0/>0",
          E"NotComposable", E"NotComposable", "08-2-0"),
         ("08-2-1: composition with inner/outer global name set sizes 0/>0",
          E"NotComposable", E"NotComposable", "08-2-1"),
         ("08-2-2: composition with inner/outer names global/local",
          E"NotComposable", E"NotComposable", "08-2-2"),
         ("08-2-3: composition with inner/outer local name set sizes >0/0",
          E"NotComposable", E"NotComposable", "08-2-3"),
         ("08-2-4: composition with inner/outer global name set sizes >0/0",
          E"NotComposable", E"NotComposable", "08-2-4"),
         ("09-0-0: parallel product of bigraphs of width 0/0",
          S"x/{x, y} * y/z * z/{} * /w", 
	  S"((x/{x, y} * y/z * z/{} * /w) * idp_0)\
           \ (idw_{w, x, y, z} * idx_0 idp_0)", "09-0-0"),
         ("09-0-1: parallel product of bigraphs of width 0/>0",
          S"x/{x, y} * y/z * [1, 0{w}]", 
	  S"((x/{x, y} * y/z) * [0{w}, 1])\
           \ (idw_{x, y, z}\
           \  * ((idw_0 * ({w})(w/w * idp_1) '{w}')\
           \      (({w})(w/w * merge_1) ((w/w * idp_1) '{w}'))\
           \     * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \        ({})(idw_0 * merge_1)\
           \             ((idw_0 * idp_1) '{}'))\
           \     [1, 0{w}])", "09-0-1"),
         ("09-1-0: parallel product of bigraphs of width >0/0",
          S"[1, 0{w}] * x/{x, y} * y/z",
	  S"((x/{x, y} * y/z) * [0{w}, 1])\
           \ (idw_{x, y, z}\
           \  * ((idw_0 * ({w})(w/w * idp_1) '{w}')\
           \      (({w})(w/w * merge_1) ((w/w * idp_1) '{w}'))\
           \     * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \        ({})(idw_0 * merge_1)\
           \             ((idw_0 * idp_1) '{}'))\
           \     [1, 0{w}])", "09-1-0"),
         ("09-1-1: parallel product of bigraphs of width >0/>0",
          S"[1, 0{w}] * [2{x}, 0, 1{y, z}]", 
	  S"(idw_0 * [0{w}, 1, 2, 3{y, z}, 4{x}])\
           \ (idw_0\
           \  * ((idw_0 * ({w})(w/w * idp_1) '{w}')\
           \      (({w})(w/w * merge_1) ((w/w * idp_1) '{w}'))\
           \     * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \        (({})(idw_0 * merge_1) ((idw_0 * idp_1) '{}'))\
           \     * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \        (({})(idw_0 * merge_1) ((idw_0 * idp_1) '{}'))\
           \     * (idw_0 * ({y, z})(idw_{y, z} * idp_1) '{y, z}')\
           \        (({y, z})\
           \         (idw_{y, z} * merge_1)\
           \          ((idw_{y, z} * idp_1) '{y, z}'))\
           \     * (idw_0 * ({x})(x/x * idp_1) '{x}')\
           \        ({x})(x/x * merge_1) ((x/x * idp_1) '{x}'))\
           \     [1, 0{w}, 4{x}, 2, 3{y, z}])", "09-1-1"),
         ("09-2-0: parallel product with global outer name clash",
          S"((x/x * y/{y_0,y_1}) * idp_2)\
           \ (((x/x * y_1/y) * idp_1) K<x,y>\
           \     * (y_0/y * idp_1) (y/z * merge_1))",
          S"((x/x * y/{y,z}) * idp_2)\
           \ (z/z\
           \  * ((idw_{x,y} * ({}) (idw_0 * idp_1) '{}')\
           \      (({})\
           \        (idw_{x,y} * merge_1)\
           \         ((idw_0 * K<x,y>)\
           \           ({})(idw_0 * merge_1) ((idw_0 * idp_1) '{}')))\
           \     * (idw_0 * ({})(idw_0 * idp_1) '{}')\
           \        ({})\
           \         (idw_0 * merge_1) ((idw_0 * idp_1) '{}'))\
           \  idp_2)", "09-2-0"),
         ("09-2-1: parallel product with global inner name clash",
          E"NotParallelisable", E"NotParallelisable", "09-2-1"),
         ("09-2-2: parallel product with local outer name clash",
          E"NotParallelisable", E"NotParallelisable", "09-2-2"),
         ("09-2-3: parallel product with local inner name clash",
          E"NotParallelisable", E"NotParallelisable", "09-2-3"),
         ("09-2-4: parallel product with local/global inner name clash",
          E"NotParallelisable", E"NotParallelisable", "09-2-4"),
         ("09-2-5: parallel product with local/global outer name clash",
          E"NotParallelisable", E"NotParallelisable", "09-2-5"),
         ("10-0-0: prime product of bigraphs of width 0/0",
          E"NotPrimeable", E"NotPrimeable", "10-0-0"),
         ("10-0-1: prime product of bigraphs of width 0/>0",
          E"NotPrimeable", E"NotPrimeable", "10-0-1"),
         ("10-1-0: prime product of bigraphs of width >0/0",
          E"NotPrimeable", E"NotPrimeable", "10-1-0"),
         ("10-1-1: prime product of bigraphs of width >0/>0",
          S"({w,x,y,z})\
           \ (idw_{w,x,y,z} * merge_5)\
           \  (('{w}'*'{}') [1, 0{w}]\
           \   * ('{}' * '{y,z}' * '{x}') [2{x},0,1{y,z}])",
	  S"(idw_0 * [0{w,x,y,z}])\
           \ (idw_0\
           \  * (idw_0\
           \     * ({w,x,y,z}) (idw_{w,x,y,z} * idp_1) '{w,x,y,z}')\
           \     (({w,x,y,z})\
           \       (idw_{w,x,y,z} * merge_5)\
           \        ((w/w * idp_1) '{w}'\
           \         * (idw_0 * idp_1) '{}'\
           \         * (idw_0 * idp_1) '{}'\
           \         * (idw_{y,z} * idp_1) '{y,z}'\
           \         * (x/x * idp_1) '{x}'))\
           \  [1, 0{w}, 4{x}, 2, 3{y,z}])", "10-1-1"),
         ("10-2-0: prime product with global inner names",
          E"NotPrimeable", E"NotPrimeable", "10-2-0"),
         ("10-2-1: prime product with global inner name clash",
          E"NotPrimeable", E"NotPrimeable", "10-2-1"),
         ("10-2-2: prime product with local outer name clash",
          E"NotPrimeable", E"NotPrimeable", "10-2-2"),
         ("10-2-3: prime product with local inner name clash",
          E"NotPrimeable", E"NotPrimeable", "10-2-3"),
         ("10-2-4: prime product with local/global inner name clash",
          E"NotPrimeable", E"NotPrimeable", "10-2-4"),
         ("10-2-5: prime product with local/global outer name clash",
          E"NotPrimeable", E"NotPrimeable", "10-2-5")
]

	val bgvaltests =
	    let
	      open BG.BgVal
	      val x1 = Name.make "x1"
	      val x2 = Name.make "x2"
	      val x3 = Name.make "x3"
	      val x4 = Name.make "x4"
	      val y1 = Name.make "y1"
	      val y2 = Name.make "y2"
	      val y3 = Name.make "y3"
	      val y4 = Name.make "y4"
	      val y5 = Name.make "y5"
	      val y6 = Name.make "y6"
	      val Y12 = NameSet.fromList [y1, y2]
	      val Y23 = NameSet.fromList [y2, y3]
	      val Y3 = NameSet.fromList [y3]
	      val Y34 = NameSet.fromList [y3, y4]
	      val Y45 = NameSet.fromList [y4, y5]
	      val Y56 = NameSet.fromList [y5, y6]
	      val p1 = Per (0,0) (Permutation.id [Y12, Y3])
	      val b1 = Ten (0,1) [Con (0,2) Y45, p1]
	      val b2 = Wir (0,3) 
			   (Wiring.make
			      (LinkSet.fromList 
			      [Link.make {outer = SOME y5, 
					  inner = NameSet.empty},
			       Link.make {outer = SOME y6, 
					  inner = NameSet.singleton y6}]))
	      val b3 = Wir (0,4)
			   (Wiring.make
			      (LinkSet.fromList
				 [Link.make {outer = SOME y2, 
					     inner = NameSet.empty}]))
	      val b4 = Abs (0,5) 
			   (NameSet.singleton y2,
			    Ten (0,6) 
				[Mer (0,7) 0,
				 Wir (0,8)
				     (Wiring.make
					(LinkSet.fromList 
					   [Link.make 
					      {outer = SOME y2, 
					       inner = NameSet.empty},
					      Link.make
						{outer = SOME y5, 
						 inner = NameSet.empty}]))])
	      fun bgvalToString' f () =
		  bgvalToString (f ())
		  handle e => (BGErrorHandler.explain' e; raise e)
	      fun assertEqualExpected (S expstr) f () =
		  assertEqualString expstr (f ())
		| assertEqualExpected (E expexn) f () =
		  let
		    val actual = f ()
		  in
		    raise Assert.Fail
			    (GeneralFailure 
			       ("expected exception " ^
				expexn ^ ", got <" ^ actual ^ ">"))
		  end
		    handle exn =>
			   (if exnName exn = expexn then
			      ""
			    else
			      (raise Assert.Fail
				       (GeneralFailure 
					  ("expected exception " ^
					   expexn ^ ", got " ^
					   exnName exn))))
	      fun startsubsuite ()
		= (ErrorMsg.reset(); ErrorMsg.fileName := "bdnfTest.sml")
	    in
	      map (fn (t, expected, f) 
		      => (t, ignore o assertEqualExpected expected
			      (bgvalToString' f)))
	    [("Empty parallel product", S"idx_0",
	      fn () => (startsubsuite (); Par (1,0) [])),
	     ("Singleton parallel product", 
	      S"'{y4, y5}' * [0{y1, y2}, 1{y3}]",
	      fn () => Par (2,0) [b1]),
	     ("Empty prime product", S"1",
	      fn () => Pri (1,0) []),
	     ("Singleton prime product", 
	      S"({y1, y2, y3})\n\
	       \ (idw_{y1, y2, y3, y4, y5} * merge_3)\n\
	       \  ((idw_{y4, y5} * '{}' * '{y1, y2}' * '{y3}')\n\
	       \   ('{y4, y5}' * [0{y1, y2}, 1{y3}]))",
	      fn () => Pri (2,0) [b1])]
	    end
       in
	 Test.labelTests
	 o (fn () => testModule (bgvalToString o bgvalUsefile'') #1
		     @ testModule (toString o usefile'') #2
		     @ bgvaltests)
       end

end
       
