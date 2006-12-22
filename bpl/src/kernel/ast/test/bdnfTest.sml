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
 * @version $LastChangedRevision$
 *)

functor BDNFTest (structure ErrorHandler : ERRORHANDLER
		  structure Assert       : ASSERT
		  structure Test         : TEST) =
struct
		
open BPL'
open Assert
val noinfo = Info.noinfo
     
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
           \ o (idw_0\
           \    * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \       o (({})(idw_0 * 1) o idx_0) o idp_0)", "00-0"),
         ("01-0: merge with 0 sites", S"1", 
	  S"(idw_0 * idp_1)\
           \ o (idw_0\
           \    * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \       o (({})(idw_0 * 1) o idx_0) o idp_0)", "01-0"),
         ("01-1: merge with 1 site", S"merge_1", 
	  S"(idw_0 * idp_1)\
           \ o (idw_0\
           \    * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \       o (({})(idw_0 * merge_1)\
           \               o ((idw_0 * idp_1) o '{}'))\
           \       o idp_1)", "01-1"),
         ("01-2: merge with >1 sites", S"merge_2", 
	  S"(idw_0 * idp_1)\
           \ o (idw_0\
           \    * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \       o (({})(idw_0 * merge_2)\
           \               o ((idw_0 * idp_1) o '{}'\
           \                  * (idw_0 * idp_1) o '{}'))\
           \       o idp_2)", "01-2"),
         ("02-0: concretion with 0 names", S"'{}'", 
	  S"(idw_0 * idp_1)\
           \ o (idw_0\
           \    * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \       o (({})(idw_0 * merge_1)\
           \               o ((idw_0 * idp_1) o '{}'))\
           \       o idp_1)", "02-0"),
         ("02-1: concretion with 1 name", S"'{x_a}'", 
	  S"(x_a/x_a * idp_1)\
           \ o (idw_0\
           \    * (x_a/x_a * ({})(idw_0 * idp_1) o '{}')\
           \       o (({})(x_a/x_a * merge_1)\
           \               o ((x_a/x_a * idp_1) o '{x_a}'))\
           \       o [0{x_a}])", "02-1"),
         ("02-2: concretion with >1 names", S"'{x_a, y_b}'", 
	  S"(idw_{x_a, y_b} * idp_1)\
           \ o (idw_0\
           \    * (idw_{x_a, y_b} * ({})(idw_0 * idp_1) o '{}')\
           \       o (({})(idw_{x_a, y_b} * merge_1)\
           \               o ((idw_{x_a, y_b} * idp_1) o '{x_a, y_b}'))\
           \       o [0{x_a, y_b}])", "02-2"),
         ("02-3: concretion with duplicate name",
          E"DuplicateNames", E"DuplicateNames", "02-3"),
         ("02-4: concretion composition", S"'{x_a}' o [0{x_a}]", 
	  S"(x_a/x_a * idp_1)\
           \ o (idw_0\
           \    * (x_a/x_a * ({})(idw_0 * idp_1) o '{}')\
           \       o (({})(x_a/x_a * merge_1)\
           \               o ((x_a/x_a * idp_1) o '{x_a}'))\
           \       o [0{x_a}])", "02-4"),
         ("03-0: wiring with 0/0 names", S"/{}",
          S"(/{} * idp_0) o (idw_0 * idx_0 o idp_0)", "03-0"),
         ("03-1: wiring with 0/1 name", S"/x_a",
          S"(/x_a * idp_0) o (x_a/x_a * idx_0 o idp_0)", "03-1"),
         ("03-2: wiring with 0/>1 names", S"/{x_a, y_b}", 
	  S"(/{x_a, y_b} * idp_0) o (idw_{x_a, y_b} * idx_0 o idp_0)", "03-2"),
         ("03-3: wiring with duplicate names",
          E"DuplicateNames", E"DuplicateNames", "03-3"),
         ("03-4: wiring with 1/0 names", S"y_b/{}",
          S"(y_b/{} * idp_0) o (idw_0 * idx_0 o idp_0)", "03-4"),
         ("03-5: wiring renaming with 1/1 name", S"y_b/x_a",
          S"(y_b/x_a * idp_0) o (x_a/x_a * idx_0 o idp_0)", "03-5"),
         ("03-6: wiring identity", S"x_a/x_a",
          S"(x_a/x_a * idp_0) o (x_a/x_a * idx_0 o idp_0)", "03-6"),
         ("03-7: wiring with 1/>1 name", S"y_b/{x_a, y_b}", 
	  S"(y_b/{x_a, y_b} * idp_0) o (idw_{x_a, y_b} * idx_0 o idp_0)", "03-7"),
         ("03-8: wiring with duplicate name",
          E"DuplicateNames", E"DuplicateNames", "03-8"),
         ("04-0-0: Ion with 0/0 names/sets", S"K", 
	  S"(idw_0 * idp_1)\
           \ o (idw_0\
           \    * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \       o (({})\
           \          (idw_0 * merge_1)\
           \           o ((idw_0 * K)\
           \               o ({})\
           \                 (idw_0 * merge_1)\
           \                  o ((idw_0 * idp_1) o '{}')))\
           \       o idp_1)", "04-0-0"),
         ("04-0-1: ion with 0/1(0) names/sets(elements)", S"K<><{}>", 
	  S"(idw_0 * idp_1)\
           \ o (idw_0\
           \    * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \       o (({})\
           \          (idw_0 * merge_1)\
           \           o ((idw_0 * K<><{}>)\
           \              o ({})\
           \                (idw_0 * merge_1)\
           \                 o ((idw_0 * idp_1) o '{}')))\
           \       o idp_1)", "04-0-1"),
         ("04-0-2: ion with 0/1(1) names/sets(elements)", S"K<><{x_a}>", 
	  S"(idw_0 * idp_1)\
           \ o (idw_0\
           \    * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \       o (({})\
           \          (idw_0 * merge_1)\
           \           o ((idw_0 * K<><{x_a}>)\
           \              o ({x_a})\
           \                (x_a/x_a * merge_1) o ((x_a/x_a * idp_1) o '{x_a}')))\
           \       o [0{x_a}])", "04-0-2"),
         ("04-0-3: ion with 0/1(>1) names/sets(elements)", S"K<><{x_a, y_b}>",
	  S"(idw_0 * idp_1)\
           \ o (idw_0\
           \    * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \       o (({})\
           \          (idw_0 * merge_1)\
           \           o ((idw_0 * K<><{x_a, y_b}>)\
           \              o ({x_a, y_b})\
           \                (idw_{x_a, y_b} * merge_1)\
           \                 o ((idw_{x_a, y_b} * idp_1) o '{x_a, y_b}')))\
           \       o [0{x_a, y_b}])", "04-0-3"),
         ("04-0-4: ion with 0/duplicate names",
          E"DuplicateNames", E"DuplicateNames", "04-0-4"),
         ("04-0-5: ion with 0/>1(0) names/sets(elements)",
          S"K<><{}, {}>",
	  S"(idw_0 * idp_1)\
           \ o (idw_0\
           \    * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \       o (({})\
           \          (idw_0 * merge_1)\
           \           o ((idw_0 * K<><{}, {}>)\
           \               o ({})\
           \                 (idw_0 * merge_1)\
           \                  o ((idw_0 * idp_1) o '{}')))\
           \       o idp_1)", "04-0-5"),
         ("04-0-6: ion with 0/>1(>0) names/sets(elements)",
          S"K<><{x_a}, {y_b, z_c}>", 
	  S"(idw_0 * idp_1)\
           \ o (idw_0\
           \    * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \       o (({})\
           \          (idw_0 * merge_1)\
           \           o ((idw_0 * K<><{x_a}, {y_b, z_c}>)\
           \              o ({x_a, y_b, z_c})\
           \                (idw_{x_a, y_b, z_c} * merge_1)\
           \                 o ((idw_{x_a, y_b, z_c} * idp_1)\
           \                     o '{x_a, y_b, z_c}')))\
           \       o [0{x_a, y_b, z_c}])", "04-0-6"),
         ("04-0-7: ion with 0/>1(>0) names/sets(duplicate elements)",
          E"DuplicateNames", E"DuplicateNames", "04-0-7"),
         ("04-1-0: Ion with 1/0 names/sets", S"K<x_a>", 
	  S"(x_a/x_16 * idp_1)\
           \ o (idw_0\
           \    * (x_16/x_16 * ({})(idw_0 * idp_1) o '{}')\
           \       o (({})\
           \          (x_16/x_16 * merge_1)\
           \           o ((idw_0 * K<x_16>)\
           \               o ({})\
           \                 (idw_0 * merge_1)\
           \                  o ((idw_0 * idp_1) o '{}')))\
           \       o idp_1)", "04-1-0"),
         ("04-1-1: ion with 1/1(>1) names/sets(elements)",
          S"K<x_a><{x_a, y_b}>", 
	  S"(x_a/x_17 * idp_1)\
           \ o (idw_0\
           \    * (x_17/x_17 * ({})(idw_0 * idp_1) o '{}')\
           \       o (({})\
           \          (x_17/x_17 * merge_1)\
           \           o ((idw_0 * K<x_17><{x_a, y_b}>)\
           \              o ({x_a, y_b})\
           \                (idw_{x_a, y_b} * merge_1)\
           \                 o ((idw_{x_a, y_b} * idp_1) o '{x_a, y_b}')))\
           \       o [0{x_a, y_b}])", "04-1-1"),
         ("04-2-0: ion with >1/>1(>1) names/sets(elements)",
          S"K<x_a, y_b><{y_b, z_c}, {}>", 
	  S"((x_a/x_19 * y_b/y_18) * idp_1)\
           \ o (idw_0\
           \    * (idw_{y_18, x_19} * ({})(idw_0 * idp_1) o '{}')\
           \       o (({})\
           \          (idw_{y_18, x_19} * merge_1)\
           \           o ((idw_0 * K<x_19, y_18><{y_b, z_c}, {}>)\
           \               o ({y_b, z_c})\
           \                 (idw_{y_b, z_c} * merge_1)\
           \                  o ((idw_{y_b, z_c} * idp_1) o '{y_b, z_c}')))\
           \       o [0{y_b, z_c}])", "04-2-0"),
         ("04-2-1: ion with duplicate/>1(?) names/sets(elements)",
          E"DuplicateNames", E"DuplicateNames", "04-2-1"),
         ("05-0-0: Permutation of width 0", S"idp_0",
          S"(idw_0 * idp_0) o (idw_0 * idx_0 o idp_0)", "05-0-0"),
         ("05-1-0: Permutation of width(elements) 1(0)", S"idp_1", 
	  S"(idw_0 * idp_1)\
           \ o (idw_0\
           \    * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \       o (({})(idw_0 * merge_1) o ((idw_0 * idp_1) o '{}'))\
           \       o idp_1)", "05-1-0"),
         ("05-1-1: Permutation of width(elements) 1(>0)", S"[0{x_a}]", 
	  S"(idw_0 * [0{x_a}])\
           \ o (idw_0\
           \    * (idw_0 * ({x_a})(x_a/x_a * idp_1) o '{x_a}')\
           \       o (({x_a})(x_a/x_a * merge_1) o ((x_a/x_a * idp_1) o '{x_a}'))\
           \       o [0{x_a}])", "05-1-1"),
         ("05-2-0: identity Permutation of width(elements) >1(>0)",
          S"[0{x_a}, 1, 2{y_b, z_c}]", 
	  S"(idw_0 * [0{x_a}, 1, 2{y_b, z_c}])\
           \ o (idw_0\
           \    * ((idw_0 * ({x_a})(x_a/x_a * idp_1) o '{x_a}')\
           \        o (({x_a})(x_a/x_a * merge_1) o ((x_a/x_a * idp_1) o '{x_a}'))\
           \       * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \          o (({})\
           \            (idw_0 * merge_1)\
           \             o ((idw_0 * idp_1) o '{}'))\
           \       * (idw_0 * ({y_b, z_c})(idw_{y_b, z_c} * idp_1) o '{y_b, z_c}')\
           \          o ({y_b, z_c})\
           \            (idw_{y_b, z_c} * merge_1)\
           \             o ((idw_{y_b, z_c} * idp_1) o '{y_b, z_c}'))\
           \       o [0{x_a}, 1, 2{y_b, z_c}])", "05-2-0"),
         ("05-2-1: Permutation of width(elements) >1(>0)",
          S"[2{x_a}, 0, 1{y_b, z_c}]", 
	  S"(idw_0 * [0, 1{y_b, z_c}, 2{x_a}])\
           \ o (idw_0\
           \    * ((idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \        o (({})\
           \           (idw_0 * merge_1)\
           \            o ((idw_0 * idp_1) o '{}'))\
           \       * (idw_0 * ({y_b, z_c})(idw_{y_b, z_c} * idp_1) o '{y_b, z_c}')\
           \          o (({y_b, z_c})\
           \             (idw_{y_b, z_c} * merge_1)\
           \              o ((idw_{y_b, z_c} * idp_1) o '{y_b, z_c}'))\
           \       * (idw_0 * ({x_a})(x_a/x_a * idp_1) o '{x_a}')\
           \          o ({x_a})(x_a/x_a * merge_1) o ((x_a/x_a * idp_1) o '{x_a}'))\
           \       o [2{x_a}, 0, 1{y_b, z_c}])", "05-2-1"),
         ("05-2-2: Permutation with duplicate names",
          E"DuplicateNames", E"DuplicateNames", "05-2-2"),
         ("05-2-3: Permutation with duplicate names",
          E"DuplicateNames", E"DuplicateNames", "05-2-3"),
         ("05-2-4: ill-formed permutation",
          E"NotPermutation", E"NotPermutation", "05-2-4"),
         ("06-0-0: abstraction of 0 of 0 names", S"({})1", 
	  S"(idw_0 * idp_1)\
           \ o (idw_0\
           \    * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \       o (({})(idw_0 * 1) o idx_0)\
           \       o idp_0)", "06-0-0"),
         ("06-0-1: abstraction of 0 of >0 names", S"({})K<y_b>", 
	  S"(y_b/y_1a * idp_1)\
           \ o (idw_0\
           \    * (y_1a/y_1a * ({})(idw_0 * idp_1) o '{}')\
           \       o (({})\
           \          (y_1a/y_1a * merge_1)\
           \           o ((idw_0 * K<y_1a>)\
           \               o ({})\
           \                 (idw_0 * merge_1)\
           \                  o ((idw_0 * idp_1) o '{}')))\
           \       o idp_1)", "06-0-1"),
         ("06-0-2: abstraction of bigraph of width >1",
          E"NotPrime", E"NotPrime", "06-0-2"),
         ("06-1-0: abstraction of >0 of 0 names",
          E"NameMissing", E"NameMissing", "06-1-0"),
         ("06-1-1: abstraction of >0 of >0 names", S"({x_a})'{x_a, y_b}'", 
	  S"(y_b/y_b * [0{x_a}])\
           \ o (idw_0\
           \    * (y_b/y_b * ({x_a})(x_a/x_a * idp_1) o '{x_a}')\
           \       o (({x_a})(idw_{x_a, y_b} * merge_1)\
           \                o ((idw_{x_a, y_b} * idp_1) o '{x_a, y_b}'))\
           \       o [0{x_a, y_b}])", "06-1-1"),
         ("06-1-2: abstraction of bigraph of width 0",
          E"NotPrime", E"NotPrime", "06-1-2"),
         ("06-1-3: abstraction of bigraph of width 1",
          S"K o L<><{x_a}>",
	  S"(idw_0 * idp_1)\
	   \ o (idw_0\
	   \    * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
	   \       o (({})\
	   \          (idw_0 * merge_1)\
	   \           o ((idw_0 * K)\
	   \               o ({})\
	   \                 (idw_0 * merge_1)\
	   \                  o ((idw_0 * L<><{x_a}>)\
	   \                      o ({x_a})\
           \                        (x_a/x_a * merge_1)\
           \                         o ((x_a/x_a * idp_1) o '{x_a}'))))\
           \       o [0{x_a}])",
	  "06-1-3"),
         ("06-1-4: normalisation and composition",
          S"K<><{x_a}> o ({x_a})L * x_a/{}",
	  S"(idw_0 * idp_1)\
	   \ o (idw_0\
	   \    * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
	   \       o (({})\
	   \          (idw_0 * merge_1)\
	   \           o ((idw_0 * K<><{}>)\
	   \               o ({})\
	   \                 (idw_0 * merge_1)\
	   \                  o ((idw_0 * L)\
	   \                      o ({})\
	   \                        (idw_0 * merge_1)\
           \                         o ((idw_0 * idp_1) o '{}'))))\
	   \       o idp_1)",
	  "06-1-4"),
         ("07-0-0: tensor product of bigraphs of width 0/0",
          S"x_a/{x_a, y_b} * y_b/z_c * z_c/{} * /w_d", 
	  S"((x_a/{x_a, y_b} * y_b/z_c * z_c/{} * /w_d) * idp_0)\
           \ o (idw_{x_a, y_b, z_c, w_d} * idx_0 o idp_0)", "07-0-0"),
         ("07-0-1: tensor product of bigraphs of width 0/>0",
          S"x_a/{x_a, y_b} * y_b/z_c * [1, 0{w_d}]", 
	  S"((x_a/{x_a, y_b} * y_b/z_c) * [0{w_d}, 1])\
           \ o (idw_{x_a, y_b, z_c}\
           \    * ((idw_0 * ({w_d})(w_d/w_d * idp_1) o '{w_d}')\
           \        o (({w_d})(w_d/w_d * merge_1) o ((w_d/w_d * idp_1) o '{w_d}'))\
           \           * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \              o ({})(idw_0 * merge_1)\
           \                     o ((idw_0 * idp_1) o '{}'))\
           \       o [1, 0{w_d}])", "07-0-1"),
         ("07-1-0: tensor product of bigraphs of width >0/0",
          S"[1, 0{w_d}] * x_a/{x_a, y_b} * y_b/z_c",
	  S"((x_a/{x_a, y_b} * y_b/z_c) * [0{w_d}, 1])\
           \ o (idw_{x_a, y_b, z_c}\
           \    * ((idw_0 * ({w_d})(w_d/w_d * idp_1) o '{w_d}')\
           \        o (({w_d})(w_d/w_d * merge_1) o ((w_d/w_d * idp_1) o '{w_d}'))\
           \           * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \              o ({})(idw_0 * merge_1)\
           \                     o ((idw_0 * idp_1) o '{}'))\
           \       o [1, 0{w_d}])", "07-1-0"),
         ("07-1-1: tensor product of bigraphs of w_didth >0/>0",
          S"[1, 0{w_d}] * [2{x_a}, 0, 1{y_b, z_c}]", 
	  S"(idw_0 * [0{w_d}, 1, 2, 3{y_b, z_c}, 4{x_a}])\
           \ o (idw_0\
           \    * ((idw_0 * ({w_d})(w_d/w_d * idp_1) o '{w_d}')\
           \        o (({w_d})(w_d/w_d * merge_1) o ((w_d/w_d * idp_1) o '{w_d}'))\
           \       * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \          o (({})(idw_0 * merge_1) o ((idw_0 * idp_1) o '{}'))\
           \       * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \          o (({})(idw_0 * merge_1) o ((idw_0 * idp_1) o '{}'))\
           \       * (idw_0 * ({y_b, z_c})(idw_{y_b, z_c} * idp_1) o '{y_b, z_c}')\
           \          o (({y_b, z_c})\
           \             (idw_{y_b, z_c} * merge_1)\
           \              o ((idw_{y_b, z_c} * idp_1) o '{y_b, z_c}'))\
           \       * (idw_0 * ({x_a})(x_a/x_a * idp_1) o '{x_a}')\
           \          o ({x_a})(x_a/x_a * merge_1) o ((x_a/x_a * idp_1) o '{x_a}'))\
           \       o [1, 0{w_d}, 4{x_a}, 2, 3{y_b, z_c}])", "07-1-1"),
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
          S"x_a/{x_a, y_b} o (y_b/z_c * x_a/{})", 
	  S"(x_a/z_c * idp_0) o (z_c/z_c * idx_0 o idp_0)", "08-0-0"),
         ("08-1-0: composition of bigraphs of width >0/0",
          S"(1 * x_a/{y_b, z_c}) o (y_b/z_c * z_c/{})", 
	  S"(x_a/z_c * idp_1)\
           \ o (z_c/z_c\
           \    * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \       o (({})(idw_0 * 1) o idx_0)\
           \       o idp_0)", "08-1-0"),
         ("08-1-1: composition of bigraphs of width >0/>0", 
	  S"('{x_a, y_b}' * /w_d * K<z_c><{z_c}, {v_e, u_f}>)\
	   \ o ((({x_a, y_b})L<x_a, y_b, w_d>) * ({z_c, v_e, u_f})M<u_f, v_e, z_c>)", 
	  S"((x_a/x_2e * y_b/y_2d * z_c/z_2f * /w_2c) * idp_2)\
           \ o (idw_0\
           \    * ((idw_{w_2c, y_2d, x_2e} * ({})(idw_0 * idp_1) o '{}')\
           \        o (({})\
           \           (idw_{w_2c, y_2d, x_2e} * merge_1)\
           \            o ((idw_0 * L<x_2e, y_2d, w_2c>)\
           \                o ({})(idw_0 * merge_1)\
           \                       o ((idw_0 * idp_1) o '{}')))\
           \       * (z_2f/z_2f * ({})(idw_0 * idp_1) o '{}')\
           \          o ({})\
           \            (z_2f/z_2f * merge_1)\
           \             o ((idw_0 * K<z_2f><{z_29}, {v_2a, u_2b}>)\
           \                 o ({z_29, v_2a, u_2b})\
           \                   (idw_{z_29, v_2a, u_2b} * merge_1)\
           \                    o ((idw_0 * M<u_2b,v_2a,z_29>)\
           \                        o ({})(idw_0 * merge_1)\
           \                               o ((idw_0 * idp_1) o '{}'))))\
           \       o idp_2)", "08-1-1"),
         ("08-1-2: composition of inner/outer width 0/>0",
          E"NotComposable", E"NotComposable", "08-1-2"),
         ("08-1-3: composition of inner/outer width >0/0",
          E"NotComposable", E"NotComposable", "08-1-3"),
         ("08-1-4: composition of inner/outer width >0 <> >0",
          E"NotComposable", E"NotComposable", "08-1-4"),
         ("08-1-5: composition of bigraph with barren root", S"'{}' o 1", 
	  S"(idw_0 * idp_1)\
           \ o (idw_0\
           \    * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \       o (({})(idw_0 * 1) o idx_0) o idp_0)", "08-1-5"),
         ("08-2-0: composition with inner/outer local name set sizces 0/>0",
          E"NotComposable", E"NotComposable", "08-2-0"),
         ("08-2-1: composition with inner/outer global name set sizces 0/>0",
          E"NotComposable", E"NotComposable", "08-2-1"),
         ("08-2-2: composition with inner/outer names global/local",
          E"NotComposable", E"NotComposable", "08-2-2"),
         ("08-2-3: composition with inner/outer local name set sizes >0/0",
          E"NotComposable", E"NotComposable", "08-2-3"),
         ("08-2-4: composition with inner/outer global name set sizes >0/0",
          E"NotComposable", E"NotComposable", "08-2-4"),
         ("09-0-0: parallel product of bigraphs of width 0/0",
          S"x_a/{x_a, y_b} * y_b/z_c * z_c/{} * /w_d", 
	  S"((x_a/{x_a, y_b} * y_b/z_c * z_c/{} * /w_d) * idp_0)\
           \ o (idw_{x_a, y_b, z_c, w_d} * idx_0 o idp_0)", "09-0-0"),
         ("09-0-1: parallel product of bigraphs of width 0/>0",
          S"x_a/{x_a, y_b} * y_b/z_c * [1, 0{w_d}]", 
	  S"((x_a/{x_a, y_b} * y_b/z_c) * [0{w_d}, 1])\
           \ o (idw_{x_a, y_b, z_c}\
           \    * ((idw_0 * ({w_d})(w_d/w_d * idp_1) o '{w_d}')\
           \        o (({w_d})(w_d/w_d * merge_1) o ((w_d/w_d * idp_1) o '{w_d}'))\
           \       * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \          o ({})(idw_0 * merge_1)\
           \                 o ((idw_0 * idp_1) o '{}'))\
           \       o [1, 0{w_d}])", "09-0-1"),
         ("09-1-0: parallel product of bigraphs of width >0/0",
          S"[1, 0{w_d}] * x_a/{x_a, y_b} * y_b/z_c",
	  S"((x_a/{x_a, y_b} * y_b/z_c) * [0{w_d}, 1])\
           \ o (idw_{x_a, y_b, z_c}\
           \    * ((idw_0 * ({w_d})(w_d/w_d * idp_1) o '{w_d}')\
           \        o (({w_d})(w_d/w_d * merge_1) o ((w_d/w_d * idp_1) o '{w_d}'))\
           \       * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \          o ({})(idw_0 * merge_1)\
           \                 o ((idw_0 * idp_1) o '{}'))\
           \       o [1, 0{w_d}])", "09-1-0"),
         ("09-1-1: parallel product of bigraphs of width >0/>0",
          S"[1, 0{w_d}] * [2{x_a}, 0, 1{y_b, z_c}]", 
	  S"(idw_0 * [0{w_d}, 1, 2, 3{y_b, z_c}, 4{x_a}])\
           \ o (idw_0\
           \    * ((idw_0 * ({w_d})(w_d/w_d * idp_1) o '{w_d}')\
           \        o (({w_d})(w_d/w_d * merge_1) o ((w_d/w_d * idp_1) o '{w_d}'))\
           \       * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \          o (({})(idw_0 * merge_1) o ((idw_0 * idp_1) o '{}'))\
           \       * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \          o (({})(idw_0 * merge_1) o ((idw_0 * idp_1) o '{}'))\
           \       * (idw_0 * ({y_b, z_c})(idw_{y_b, z_c} * idp_1) o '{y_b, z_c}')\
           \          o (({y_b, z_c})\
           \             (idw_{y_b, z_c} * merge_1)\
           \              o ((idw_{y_b, z_c} * idp_1) o '{y_b, z_c}'))\
           \       * (idw_0 * ({x_a})(x_a/x_a * idp_1) o '{x_a}')\
           \          o ({x_a})(x_a/x_a * merge_1) o ((x_a/x_a * idp_1) o '{x_a}'))\
           \       o [1, 0{w_d}, 4{x_a}, 2, 3{y_b, z_c}])", "09-1-1"),
         ("09-2-0: parallel product with global outer name clash",
          S"((x_a/x_a * y_b/{y_10, y_11}) * idp_2)\
           \ o (((x_a/x_a * y_11/y_b) * idp_1) o K<x_a,y_b>\
           \     * (y_10/y_b * idp_1) o (y_b/z_c * merge_1))",
          S"((x_a/x_3c * y_b/{z_c,y_3b}) * idp_2)\
           \ o (z_c/z_c\
           \    * ((idw_{y_3b,x_3c} * ({})(idw_0 * idp_1) o '{}')\
           \        o (({})\
           \           (idw_{y_3b,x_3c} * merge_1)\
           \            o ((idw_0 * K<x_3c,y_3b>)\
           \               o ({})(idw_0 * merge_1)\
           \                      o ((idw_0 * idp_1) o '{}')))\
           \       * (idw_0 * ({})(idw_0 * idp_1) o '{}')\
           \          o ({})\
           \            (idw_0 * merge_1) o ((idw_0 * idp_1) o '{}'))\
           \      o idp_2)", "09-2-0"),
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
          S"({x_a,y_b,z_c,w_d})\
           \(idw_{x_a,y_b,z_c,w_d} * merge_5)\
           \ o (('{w_d}'*'{}') o [1, 0{w_d}]\
           \     * ('{}' * '{y_b,z_c}' * '{x_a}') o [2{x_a},0,1{y_b,z_c}])",
	  S"(idw_0 * [0{x_a,y_b,z_c,w_d}])\
           \ o (idw_0\
           \    * (idw_0\
           \       * ({x_a,y_b,z_c,w_d})(idw_{x_a,y_b,z_c,w_d} * idp_1) o '{x_a,y_b,z_c,w_d}')\
           \       o (({x_a,y_b,z_c,w_d})\
           \          (idw_{x_a,y_b,z_c,w_d} * merge_5)\
           \           o ((w_d/w_d * idp_1) o '{w_d}'\
           \           * (idw_0 * idp_1) o '{}'\
           \           * (idw_0 * idp_1) o '{}'\
           \           * (idw_{y_b,z_c} * idp_1) o '{y_b,z_c}'\
           \           * (x_a/x_a * idp_1) o '{x_a}'))\
           \       o [1, 0{w_d}, 4{x_a}, 2, 3{y_b,z_c}])", "10-1-1"),
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
	      open BgVal
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
	      val p1 = Per noinfo (Permutation.id [Y12, Y3])
	      val b1 = Ten noinfo [Con noinfo Y45, p1]
	      val b2 = Wir noinfo 
			   (Wiring.make
			      (LinkSet.fromList 
			      [Link.make {outer = SOME y5, 
					  inner = NameSet.empty},
			       Link.make {outer = SOME y6, 
					  inner = NameSet.singleton y6}]))
	      val b3 = Wir noinfo
			   (Wiring.make
			      (LinkSet.fromList
				 [Link.make {outer = SOME y2, 
					     inner = NameSet.empty}]))
	      val b4 = Abs noinfo
			   (NameSet.singleton y2,
			    Ten noinfo 
				[Mer noinfo 0,
				 Wir noinfo
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
		  handle e => (ErrorHandler.explain e; raise e)
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
	      fn () => (startsubsuite (); Par noinfo [])),
	     ("Singleton parallel product", 
	      S"'{y4_7, y5_8}' * [0{y1_4, y2_5}, 1{y3_6}]",
	      fn () => Par noinfo [b1]),
	     ("Empty prime product", S"1",
	      fn () => Pri noinfo []),
	     ("Singleton prime product", 
	      S"({y1_4, y2_5, y3_6})\n\
	       \ (idw_{y1_4, y2_5, y3_6, y4_7, y5_8} * merge_3)\n\
	       \  o ((idw_{y4_7, y5_8} * '{}' * '{y1_4, y2_5}' * '{y3_6}')\n\
	       \     o ('{y4_7, y5_8}' * [0{y1_4, y2_5}, 1{y3_6}]))",
	      fn () => Pri noinfo [b1])]
	    end
       in
	 Test.labelTests
	 o (fn () => testModule (bgvalToString o bgvalUsefile'') #1
		     @ testModule (toString o usefile'') #2
		     @ bgvaltests)
       end

end
       
