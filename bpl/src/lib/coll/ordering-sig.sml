signature ORDERING =
sig
    type T
    val lt: T -> T -> bool
    (* lt x y; returns true if x is less than y; returns false otherwise. *)
end;

