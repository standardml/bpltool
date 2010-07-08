(** Replacement for OS_PROCESS which has different implementations in
 * the various versions of the Standard Library. *)

signature OS_PROCESS = sig
  type status

  val success   : status
  val failure   : status
  val isSuccess : status -> bool

  val system    : string -> status

  val atExit    : (unit -> unit) -> unit
  val exit      : status -> 'a
  val terminate : status -> 'a

  val getEnv    : string -> string option
end
