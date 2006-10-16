(**
 * Collection of modules.
 * @author YAMATODANI Kiyoshi
 * @version $Id: smlunit.sml,v 1.1 2006/09/02 12:52:35 hniss Exp $
 *
 * Copyright (c) 2006, Tohoku University. All rights reserved.
 * See license.txt (http://www.pllab.riec.tohoku.ac.jp/smlsharp/?SMLSharpLicence) 
 * for details.
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
