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
Flags.setBoolFlag "/kernel/ast/bgval/pp-tensor2parallel" false;
Flags.setBoolFlag "/kernel/ast/bgval/pp-merge2prime" false;
val _ = use_shorthands true;


use "bpel-namedports-strings.sml";
use "bpel-namedports-sig.sml";
use "bpel-namedports-rules.sml";
use "bpel-namedports-examples.sml";

