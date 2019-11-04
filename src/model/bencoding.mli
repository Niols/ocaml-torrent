(** {1 Bencoding} *)

(** Type of the Bencodable values. *)
type t =
  | Int of int
  | String of string
  | List of t list
  | Dict of (string * t) list

(** {2 Getters} *)

module Getter : sig
  type 'a converter = t -> 'a

  val find : string list -> t -> t option

  val mem : string list -> t -> bool

  val gen_get :
    ?on_not_found:(unit -> 'b) -> ?on_conversion_failed:(unit -> 'b) ->
    on_success:('a -> 'b) ->
    'a converter -> string list -> t -> 'b

  type 'a get_t = NotFound | ConversionFailed | Success of 'a

  val get_t :
    ?on_not_found:(unit -> 'a get_t) ->
    ?on_conversion_failed:(unit -> 'a get_t) ->
    ?on_success:('a -> 'a get_t) ->
    'a converter -> string list -> t -> 'a get_t

  val get :
    ?on_not_found:(unit -> 'a) -> ?on_conversion_failed:(unit -> 'a) ->
    'a converter -> string list -> t -> 'a

  val int : int converter
  val string : string converter
  val list : 'a converter -> ('a list) converter
end

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
