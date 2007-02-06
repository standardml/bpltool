structure PrettyPrint : PRETTYPRINT =
struct
  open Compiler.PrettyPrint

  type ppconsumer = device
  type ppstream = stream

  datatype break_style  = CONSISTENT  | INCONSISTENT
  exception PP_FAIL of string

  val mk_ppstream = openStream
  val dest_ppstream = getDevice
  fun add_break pps (nsp, offset) = break pps {nsp = nsp, offset = offset}
  val add_newline = newline
  val add_string = string
  fun begin_block pps CONSISTENT indent =
      openHVBox pps (Rel indent)
    | begin_block pps INCONSISTENT indent =
      openHOVBox pps (Rel indent)
  val end_block = closeBox
  val flush_ppstream = flushStream
  (* FIXME no similar function in Compiler.PrettyPrint *)
  fun clear_ppstream pps = flush_ppstream pps
end
