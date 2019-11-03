(** {1 Bencoding} *)

(** Type of the Bencodable values. *)
type t =
  | Int of int
  | String of string
  | List of t list
  | Dict of (string * t) list

(** {2 Constructors & Destructors} *)

val int : int -> t
val string : string -> t
val list : ('a -> t) -> 'a list -> t

val isany : t -> bool
val isint : t -> bool
val isstring : t -> bool
val islist : (t -> bool) -> t -> bool

val unint : t -> int
val unstring : t -> string
val unlist : (t -> 'a) -> t -> 'a list

(** {2 Parsers & printers} *)

val to_buffer : Buffer.t -> t -> unit
val to_string : t -> string
(* val to_file : string -> t -> unit *)

val from_string : string -> t
val from_file : string -> t

(** {2 Debug} *)

val show : t -> string
