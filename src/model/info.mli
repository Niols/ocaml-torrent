(** {1 info} *)

type t

val to_bencoding : t -> Bencoding.t
val from_bencoding : Bencoding.t -> t

val show : t -> string
val pp : Format.formatter -> t -> unit
