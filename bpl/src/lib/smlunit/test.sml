(**
 * datatypes for test cases and utility operators for them.
 * @author YAMATODANI Kiyoshi
 * @copyright Copyright (c) 2006, Tohoku University. All rights reserved.<br/>
 * See license.txt 
 * (or the <a href="http://www.pllab.riec.tohoku.ac.jp/smlsharp/?SMLSharpLicence">SML# web page</a>)
 * for details.
 * @version $LastChangedRevision$
 *)
structure Test :> TEST =
struct
  
  (***************************************************************************)

  type testFunction = unit -> unit

  datatype test =
           TestCase of (unit -> unit)
         | TestLabel of (string * test)
         | TestList of test list

  (***************************************************************************)

  fun labelTests labelTestPairList =
      TestList
      (map
       (fn (label, function) => (TestLabel (label, TestCase function)))
       labelTestPairList)

  (***************************************************************************)

end
