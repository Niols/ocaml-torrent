(** {1 metainfo files}

  Metainfo files (also known as .torrent files) are dictionaries with the
  following keys:

  announce
    The URL of the tracker.
  info
    This maps to a dictionary, with keys described below.

  All strings in a .torrent file that contains text must be UTF-8 encoded. *)

type t

val announce : t -> string
val info : t -> Info.t

val to_bencoding : t -> Bencoding.t
val from_bencoding : Bencoding.t -> t

val show : t -> string
val pp : Format.formatter -> t -> unit
