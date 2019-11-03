type file =
  { length : int ;
    path : string list }
[@@deriving show { with_path = false }]

let file_to_bencoding f =
  Bencoding.(
    Dict [
      "length", int f.length ;
      "path", list string f.path ;
    ]
  )

(* FIXME: would me soo much easier with a good helper. *)
let file_from_bencoding b =
  match b with
  | Bencoding.Dict d ->
    (
      match List.assoc_opt "length" d with
      | Some (Int length) ->
        (
          match List.assoc_opt "path" d with
          | Some path when Bencoding.(islist isstring path) ->
            (
              { length ; path = Bencoding.(unlist unstring path) }
            )
          | None -> failwith "Info.file_from_bencoding: missing key: 'path'"
          | _ -> failwith "Info.file_from_bencoding: wrong type for key: 'path'"
        )
      | None -> failwith "Info.file_from_bencoding: missing key: 'length'"
      | _ -> failwith "Info.file_from_bencoding: wrong type for key: 'length'"
    )
  | _ ->
    failwith "Info.file_from_bencoding: not a dictionary"

type t =
  { name : string ; (* Name of a file or directory, depending on the case. *)
    piece_length : int ;
    pieces : string list ; (* All strings are of length 20 *)
    length : int option ;
    files : file list }
(* length and files cannot be both set or unset at the same time. *)
[@@deriving show { with_path = false }]

let piece_hash_size = 20

let to_bencoding i =
  Bencoding.(
    Dict ([
        "name", string i.name ;
        "piece length", int i.piece_length ;
        "pieces", string (String.concat "" i.pieces);
      ] @ (
          match i.length, i.files <> [] with
          | None, true -> ["files", list file_to_bencoding i.files]
          | Some length, false -> ["length", int length]
          | _ -> assert false
        ))
  )

let from_bencoding b =
  match b with
  | Bencoding.Dict d ->
    (
      match List.assoc_opt "name" d with
      | Some (String name) ->
        (
          match List.assoc_opt "piece length" d with
          | Some (Int piece_length) ->
            (
              match List.assoc_opt "pieces" d with
              | Some (String pieces) ->
                let pieces =
                  let l = String.length pieces in
                  if l mod piece_hash_size <> 0 then
                    failwith ("Info.from_bencoding: 'pieces' must have a size dividable by " ^ (string_of_int piece_hash_size));
                  let rec cut i =
                    if i >= l then []
                    else String.sub pieces i piece_hash_size :: cut (i + piece_hash_size)
                  in
                  cut 0
                in
                (
                  match List.assoc_opt "length" d, List.assoc_opt "files" d with
                  | Some (Int length), None ->
                    { name ; piece_length ;
                      pieces = pieces ;
                      length = Some length ; files = [] }
                  | None, Some files when Bencoding.(islist isany files) ->
                    { name ; piece_length ;
                      pieces = pieces ;
                      length = None ; files = Bencoding.unlist file_from_bencoding files }
                  | None, None -> failwith "Info.from_bencoding: one of 'length' and 'files' must be set"
                  | _ -> failwith "Info.from_bencoding: 'length' and 'files' cannot be both set"
                )
              | None -> failwith "Info.from_bencoding: missing key: 'pieces'"
              | _ -> failwith "Info.from_bencoding: wrong type for key: 'pieces'"
            )
          | None -> failwith "Info.from_bencoding: missing key: 'piece_length'"
          | _ -> failwith "Info.from_bencoding: wrong type for key: 'piece_length'"
        )
      | None -> failwith "Info.from_bencoding: missing key: 'name'"
      | _ -> failwith "Info.from_bencoding: wrong type for key: 'name'"
    )
  | _ ->
    failwith "Info.from_bencoding: not a dictionary"
