(* Formalization of a subset of BPEL using Binding Bigraphical Reactive
 * Systems in BPLtool.
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
