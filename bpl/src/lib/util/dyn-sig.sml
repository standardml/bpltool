signature Dyn = sig
    type 'a brand
    type t
    val make_brand : unit -> 'a brand
    val pack : 'a brand -> 'a -> t
    val unpack : 'a brand -> t -> 'a option
    val null : t
end

