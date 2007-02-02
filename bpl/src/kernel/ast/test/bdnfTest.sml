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

functor BDNFTest (structure ErrorHandler  : ERRORHANDLER
		  structure Assert        : ASSERT
		  structure Test          : TEST
                  (*val       test_data_dir : string*)) =
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

  val assertEqualBgVal = assertEqual (fn (b1, b2) => BgVal.eq b1 b2) (PrettyPrint.pp_to_string 72 (BgVal.pp 0))

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
              val comment
                = case TextIO.inputLine is of
                    SOME s => strip_last_char s
                  | NONE => (  TextIO.closeIn is
                             ; raise InvalidTestFile
                               (test_file, "the file is empty"))

              val input_file = test_data_dir ^ "/" ^ filename ^ ".input.tmp"
              val result_file = test_data_dir ^ "/" ^ filename ^ ".result.tmp"
              val input_os = TextIO.openOut input_file
              val result_os = TextIO.openOut result_file
              fun close_files () = (  TextIO.closeIn is
                                    ; TextIO.closeOut input_os
                                    ; TextIO.closeOut result_os)
              fun rm_tmp_files () = (  remove input_file
                                     ; remove result_file)
              fun cleanup () = (  close_files ()
                                ; rm_tmp_files())
                
              fun parse_result () =
                  case TextIO.inputLine is of
                    SOME s => (  TextIO.output (result_os, s)
                               ; parse_result ())
                  | NONE => ()

              fun parse_exception () =
                  case TextIO.inputLine is of
                    SOME s => strip_last_char s
                  | NONE => (  cleanup ()
                             ; raise InvalidTestFile
                                       (test_file,
                                        "the file has no exception name"))
              fun parse_test_file () =
                  case TextIO.inputLine is of
                    SOME s => if String.isPrefix "result:" s then
                                (parse_result (); NONE)
                              else if String.isPrefix "exception:" s then
                                SOME (parse_exception ())
                              else
                                (  TextIO.output (input_os, s)
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
                  val input_bgval
                    = (bgvalUsefile'' input_file)
                      handle e => (  rm_tmp_files()
                                   ; raise InvalidTestFile
                                             (test_file,
                                              "the input is not a /\
                                               \well-formed bgterm"))
                  val result_bgval
                    = (bgvalUsefile'' result_file)
                      handle e => (  rm_tmp_files()
                                   ; raise InvalidTestFile
                                             (test_file,
                                              "the result is not a /\
                                              \well-formed bgterm"))
                in
                  (  assertEqualBgVal
                       result_bgval
                       (BgBDNF.unmk (BgBDNF.make input_bgval))
                   ; rm_tmp_files ())
                end
              | SOME exnname =>
                (  close_files ()
                 ; usefile'' input_file
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
            if String.isSuffix ".bgtest" filename then
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
       
