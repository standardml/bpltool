(**
 * datatypes for test cases and utility operators for them.
 * @author YAMATODANI Kiyoshi
 * @version $Id: test.sml,v 1.1 2006/09/02 12:52:35 hniss Exp $
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
