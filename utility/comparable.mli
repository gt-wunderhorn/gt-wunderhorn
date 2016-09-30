module type T = sig
  type t
  val compare : t -> t -> int
end
