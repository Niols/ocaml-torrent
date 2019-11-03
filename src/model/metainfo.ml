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
  match b with
  | Bencoding.Dict d ->
    (
      let announce = List.assoc_opt "announce" d in
      let info = List.assoc_opt "info" d in
      match announce, info with
      | Some (String announce), Some info ->
        { announce ; info = Info.from_bencoding info }
      | None, _ -> failwith "Metainfo.from_bencoding: missing key: 'announce'"
      | _, None -> failwith "Metainfo.from_bencoding: missing key: 'info'"
      | _, Some _ -> failwith "Metainfo.from_bencoding: wrong type for key: 'announce'"
    )
  | _ ->
    failwith "Metainfo.from_bencoding: not a dictionary"
