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
 *  Tests for correct normalization, and also for 
 * correct tensor2parallel conversion 
 * (see <pre>fun t2p</pre> in <pre>bgval</pre>.)
 * 
 * @version $LastChangedRevision$
 *)

functor BDNFTest (structure ErrorHandler  : ERRORHANDLER
		  structure Assert        : ASSERT
		  structure Test          : TEST) =
struct

open BPL'
open Assert

(* The following format should be used for *.bgtest files: *)
(*
one line description

the bg term to use as input
may use multiple lines

result:

expected term
may use multiple lines
*)
(* The result is also allowed to be an exception as in the following: *)
(*
one line description

the bg term to use as input
may use multiple lines

exception:
nameOfException
*)
(* the exception name must come on the line immediately
   after the exception keyword. *)


  datatype expectedresult = S of bgval | E of string

  val assertEqualBgVal
    = assertEqual (fn (b1, b2) => BgVal.eq b1 b2)
                  (PrettyPrint.pp_to_string 72 (BgVal.pp 0))

  val assertEqualBgBDNF
    = let val ppids  = Flags.getBoolFlag "/kernel/ast/bgterm/ppids"
	  val pp0abs = Flags.getBoolFlag "/kernel/ast/bgterm/pp0abs"
      in 
	  ( Flags.setBoolFlag "/kernel/ast/bgterm/ppids" true;
	    Flags.setBoolFlag "/kernel/ast/bgterm/pp0abs" true;
	    assertEqual (fn (b1, b2) => BgBDNF.eq' 
					    ((BgBDNF.make o (BgVal.make BgTerm.info)) b1) 
					    ((BgBDNF.make o (BgVal.make BgTerm.info)) b2))
			(PrettyPrint.pp_to_string 72 (BgTerm.pp 0))

			 
	    before 
	    (Flags.setBoolFlag "/kernel/ast/bgterm/ppids" ppids;
	     Flags.setBoolFlag "/kernel/ast/bgterm/pp0abs" pp0abs))
      end

  exception InvalidTestFile of string * string

  (* utility function to remove the last character from
   * a string (usually \n) *)
  fun strip_last_char s = String.substring (s, 0, String.size s - 1)

  fun build_test_suite test_data_dir () =
      let
        open OS.FileSys

        (* Test function for a given file name *)
        fun test filename () =
            let
              (* split the file into comment, input, and expected result *)
              val test_file = test_data_dir ^ "/" ^ filename
              val is = TextIO.openIn test_file
              val lines
                = ref (Substring.fields
                         (fn #"\n" => true | _ => false)
                         (Substring.full (TextIO.inputAll is)))
              val () = TextIO.closeIn is

              fun getLine () =
                  case !lines of
                    [] => NONE
                  | (l::ls) => (  lines := ls
                                ; SOME (Substring.string l))

              val comment
                = case getLine () of
                    SOME s => s
                  | NONE => raise InvalidTestFile
                                    (test_file, "the file is empty")

              val input_file  = test_data_dir ^ "/" ^ filename ^ ".input.tmp"
              val result_file = test_data_dir ^ "/" ^ filename ^ ".result.tmp"
              val input_os  = TextIO.openOut input_file
              val result_os = TextIO.openOut result_file
              fun close_files () = (  TextIO.closeIn is
                                    ; TextIO.closeOut input_os
                                    ; TextIO.closeOut result_os)
              fun rm_tmp_files () = (  remove input_file
                                     ; remove result_file)
                                    handle OS.SysErr (s, _) => () (*FIXME sometimes files cannot be deleted?*)
              fun cleanup () = (  close_files ()
                                ; rm_tmp_files())
                
              fun parse_result () =
                  case getLine () of
                    SOME s => (  TextIO.output (result_os, s)
                               ; parse_result ())
                  | NONE => ()

              fun parse_exception () =
                  case getLine () of
                    SOME s => s
                  | NONE => (  cleanup ()
                             ; raise InvalidTestFile
                                       (test_file,
                                        "the file has no exception name"))

              fun parse_test_file () =
                  case getLine () of
                    SOME s =>
                      if String.isPrefix "result:" s then
                        (parse_result (); NONE)
                      else if String.isPrefix "exception:" s then
                        SOME (parse_exception ())
                      else
                        (  TextIO.output (input_os, s)
                         ; TextIO.output (input_os, "\n")
                         ; parse_test_file ())
                  | NONE => (  cleanup ()
                             ; raise InvalidTestFile
                                       (test_file,
                                        "the file has no result/\
                                         \exception section"))
            in
              case parse_test_file () of
                NONE =>
                let
                  val () = close_files ()

(* TODO: (TCD) Shouldn't InvalidTestFile exception be Fails rather than Errors?
 ... Allows the testsuite to show these explanations. 
 UPDATE: I've implemented this below. *)

		  val input_bgterm 
		    = (bgtermUsefile'' input_file)
                      handle e => ( rm_tmp_files()
                                   ; 
				   raise Assert.Fail
				     (GeneralFailure 
					  ("testfile is invalid - the input is not parsable as \
                                               \a bgterm")))
(*
                          		      raise InvalidTestFile
                                             (test_file,
                                              "the input is not parsable as \
                                               \a bgterm"))
*)

                  val input_bgval
                    = (bgvalUseBgTermfile'' input_file)
                      handle e => (  rm_tmp_files()
                                   ; 
				   raise Assert.Fail
				     (GeneralFailure 
					  ("testfile is invalid - the input is not parsable as \
                                               \a wellformed bgterm")))
(*
				   raise InvalidTestFile
                                             (test_file,
                                              "the input is not a \
                                               \well-formed bgterm"))
*)
                  val result_bgval
                    = (bgvalUseBgTermfile'' result_file)
                      handle e => (  rm_tmp_files()
                                   ; 
				   raise Assert.Fail
				     (GeneralFailure 
					  ("testfile is invalid - the result is not parsable as \
                                               \a wellformed bgterm")))
(*
                                             raise InvalidTestFile
                                             (test_file,
                                              "the result is not a \
                                              \well-formed bgterm"))
*)
		       
                in
		    
                  (  assertEqualBgVal
			 result_bgval
			 (BgBDNF.unmk (BgBDNF.make input_bgval))
                   ; (* UPDATE: Added bgval tensor2parallel testing *)
                     assertEqualBgBDNF
    			 input_bgterm		 
			 ((#1 o BgVal.t2p) input_bgval)
                   ;
                   rm_tmp_files ())
		    
                end
              | SOME exnname =>
                (  close_files ()
                 ; useBgTermfile'' input_file
                 ; raise Assert.Fail
                           (GeneralFailure ("expected exception "
                                            ^ exnname)))
                handle exn => 
                  (  rm_tmp_files ()
                   ; case exn of
                       Assert.Fail g => raise Assert.Fail g
                     | exn =>
                       if exnName exn = exnname then
                         ()
                       else
                         (raise Assert.Fail
                                  (GeneralFailure 
                                     ("expected exception " ^
                                      exnname ^ ", got " ^
                                      exnName exn))))
            end		   

        (* scan the test data directory for *.bgtest files *)
        val dirstream = openDir test_data_dir
        fun build_test_list NONE acc = (closeDir dirstream; acc)
          | build_test_list (SOME filename) acc =
            if Substring.isSuffix ".bgtest" (Substring.full filename) then
              let
                (* the test name is taken to be the filename (ex. extension) *)
                val testname
                  = String.substring (filename, 0, String.size filename - 7)
              in
                build_test_list
                  (readDir dirstream)
                  ((testname, test filename) :: acc)
              end
            else
              build_test_list (readDir dirstream) acc
      in
        Test.labelTests (build_test_list (readDir dirstream) [])
      end

  val suite = build_test_suite

end
       
