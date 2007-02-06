(**
 * This module runs test cases and makes report of their results.
 * @author YAMATODANI Kiyoshi
 * @copyright Copyright (c) 2006, Tohoku University. All rights reserved.<br/>
 * See license.txt 
 * (or the <a href="http://www.pllab.riec.tohoku.ac.jp/smlsharp/?SMLSharpLicence">SML# web page</a>)
 * for details.
 * @version $LastChangedRevision: 102 $
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
