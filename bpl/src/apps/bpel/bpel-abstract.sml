(* Formalization of a subset of BPEL using Binding Bigraphical Reactive
 * Systems in BPLtool.
 *
 * This variant
 * - is a flattened (more abstract) version of the one in bpel.sml.
 * - has no garbage collection.
 * - encloses each instance in an Instance node (similar to distributed pi).
 * - uses the enclosing node to control the state of the instance (as opposed to
 *   having a status node). Depending on the state, the outermost node of an
 *   instance has one the controls Invoked, Instance, or Stopped.
 * - encodes part of the place graph in the concrete semantics as links
 *   to enable garbage collection.
 *
 *
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


use "bpel-abstract-strings.sml";
use "bpel-abstract-sig.sml";
use "bpel-abstract-rules.sml";
(* use "bpel-abstract-examples.sml"; *)
