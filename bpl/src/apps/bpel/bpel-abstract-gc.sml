(* Formalization of a subset of BPEL using Binding Bigraphical Reactive
 * Systems in BPLtool.
 *
 * This variant
 * - is a flattened (more abstract) version of the one in bpel-gc.sml.
 * - has garbage collection.
 * - has a status node just beneath each Instance node.
 * - encodes part of the place graph in the concrete semantics as links
 *   to enable garbage collection:
 *   there is an atomic ActScope node for each active scope (including 
 *   instances) which has four free ports which are connected as 
 *   follows:
 *     1. to the variables of that scope
 *     2. to the child activities (including scopes) of the scope
 *     3. to the parent scope's ActScope child-port
 *     4. to the instance id
 *   This allows us to garbage collect an instance step-wise and depth first.
 *
 * NB! Uses the SML/NJ version of the BPLtool command line.
 *)

SMLofNJ.Internals.GC.messages false;
val bpel_dir = OS.FileSys.getDir ();
val _ = OS.FileSys.chDir "../bpl/";
use "smlnj.sml";
val _ = OS.FileSys.chDir bpel_dir;
use "figinfo.sml";

Flags.setBoolFlag "/kernel/ast/bgval/pp-simplify" true;
(* Does not work right now!
Flags.setBoolFlag "/kernel/ast/bgval/pp-tensor2parallel" true;
Flags.setBoolFlag "/kernel/ast/bgval/pp-merge2prime" true;*)
val _ = use_shorthands true;


use "bpel-abstract-strings-gc.sml";
use "bpel-abstract-sig-gc.sml";
use "bpel-abstract-rules-gc.sml";
(* use "bpel-abstract-examples-gc.sml"; *)
