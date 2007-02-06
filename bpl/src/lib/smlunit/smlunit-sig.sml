(**
 * Collection of modules.
 * @author YAMATODANI Kiyoshi
 * @copyright Copyright (c) 2006, Tohoku University. All rights reserved.<br/>
 * See license.txt 
 * (or the <a href="http://www.pllab.riec.tohoku.ac.jp/smlsharp/?SMLSharpLicence">SML# web page</a>)
 * for details.
 * @version $LastChangedRevision: 102 $
 *)
signature SMLUNIT =
sig

  (***************************************************************************)

  structure Assert : ASSERT

  structure Test : TEST

  structure TextUITestRunner : TESTRUNNER

(*
  structure HTMLReportTestRunner : TESTRUNNER
*)

  (***************************************************************************)

end
