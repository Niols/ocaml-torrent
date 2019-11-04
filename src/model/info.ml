open Torrent_ext

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

let file_from_bencoding b =
  let open Bencoding.Getter in
  let length = get int ["length"] b in
  let path = get (list string) ["path"] b in
  { length ; path }

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

let cut size s =
  let l = String.length s in
  let rec cut i =
    if i >= l then []
    else String.sub s i size :: cut (i + size)
  in
  cut 0

let from_bencoding b =
  let open Bencoding.Getter in
  let name = get string ["name"] b in
  let piece_length = get int ["piece length"] b in
  let pieces = get (string ||> cut piece_hash_size) ["pieces"] b in
  let length, files =
    let on_conversion_failed = fun () ->
      failwith "Info.from_bencoding: conversion failed"
    in
    match
      get_t int ["length"] b ~on_conversion_failed,
      get_t (list file_from_bencoding) ["files"] b ~on_conversion_failed
    with
    | Success length, NotFound -> Some length, []
    | NotFound, Success files ->
      (* FIXME: files can be empty? *)
      None, files
    | _ -> failwith "Info.from_bencoding: 'length' and 'files' cannot be both set or unset"
  in
  { name; piece_length; pieces; length; files }
