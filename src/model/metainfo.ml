type t =
  { announce : string ;
    info : Info.t }
[@@deriving show { with_path = false }]

let announce m = m.announce
let info m = m.info

let to_bencoding m =
  Bencoding.(
    Dict [
      "announce", String m.announce ;
      "info", (Info.to_bencoding m.info) ;
    ]
  )

let from_bencoding b =
  let open Bencoding.Getter in
  let announce = get string ["announce"] b in
  let info = get Info.from_bencoding ["info"] b in
  { announce; info }
