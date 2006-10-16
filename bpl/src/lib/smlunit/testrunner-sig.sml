(**
 * This module runs test cases and makes report of their results.
 * @author YAMATODANI Kiyoshi
 * @version $Id: testrunner-sig.sml,v 1.1 2006/09/02 12:52:35 hniss Exp $
 *
 * Copyright (c) 2006, Tohoku University. All rights reserved.
 * See license.txt (http://www.pllab.riec.tohoku.ac.jp/smlsharp/?SMLSharpLicence) 
 * for details.
 *)
signature TESTRUNNER =
sig
  
  (***************************************************************************)

  (**
   * implementation specific parameter to runTest function.
   *)
  type parameter

  (**
   *  perform tests
   * @params parameter test
   * @param parameter implementation specific parameter
   * @param test to perform
   *)
  val runTest : parameter -> Test.test -> unit

  (***************************************************************************)

end
