(**
 * Collection of modules.
 * @author YAMATODANI Kiyoshi
 * @version $Id: smlunit-sig.sml,v 1.1 2006/09/02 12:52:34 hniss Exp $
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