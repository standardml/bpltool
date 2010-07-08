(** Implementation of the compromise signature OS_PROCESS. *)
structure OS_Process = struct
  open OS.Process

  fun isSuccess status = status = success
end
