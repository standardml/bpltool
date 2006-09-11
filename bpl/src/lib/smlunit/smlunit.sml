(**
 * Collection of modules.
 * @author YAMATODANI Kiyoshi
 * @version $Id: smlunit.sml,v 1.1 2006/09/02 12:52:35 hniss Exp $
 *)
structure SMLUnit : SMLUNIT =
struct

  (***************************************************************************)

  structure Assert = Assert

  structure Test = Test

  structure TextUITestRunner = TextUITestRunner

(*
  structure HTMLReportTestRunner = HTMLReportTestRunner
*)

  (***************************************************************************)

end