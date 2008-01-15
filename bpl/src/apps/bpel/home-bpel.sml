(* Formalization of a subset of BPEL extended with mobile processes
 * using Binding Bigraphical Reactive Systems in BPLtool.
 *
 * NB! Uses the SML/NJ version of the BPLtool command line.
 *)

SMLofNJ.Internals.GC.messages false;
val bpel_dir = OS.FileSys.getDir ();
val _ = OS.FileSys.chDir "../bpl/";
use "smlnj.sml";
val _ = OS.FileSys.chDir bpel_dir;
use "figinfo.sml";
Flags.setBoolFlag "/kernel/ast/bgval/tensor2parallel" true;
val _ = use_shorthands true;


use "home-bpel-strings.sml";
use "home-bpel-sig.sml";
use "home-bpel-rules.sml";
use "home-bpel-examples.sml";

