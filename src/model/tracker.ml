open Torrent_ext

type query = (string * string list) list

(* The 20 byte sha1 hash of the bencoded form of the info value from the
   metainfo file. This value will almost certainly have to be escaped.

   Note that this is a substring of the metainfo file. The info-hash must be the
   hash of the encoded form as found in the .torrent file, which is identical to
   bdecoding the metainfo file, extracting the info dictionary and encoding it
   if and only if the bdecoder fully validated the input (e.g. key ordering,
   absence of leading zeros). Conversely that means clients must either reject
   invalid metainfo files or extract the substring directly. They must not
   perform a decode-encode roundtrip on invalid data. *)

(* A string of length 20 which this downloader uses as its id. Each downloader
   generates its own id at random at the start of a new download. This value
   will also almost certainly have to be escaped. *)

(* An optional parameter giving the IP (or dns name) which this peer is at.
   Generally used for the origin if it's on the same machine as the tracker. *)

(* The port number this peer is listening on. Common behavior is for a
   downloader to try to listen on port 6881 and if that port is taken try 6882,
   then 6883, etc. and give up after 6889. *)

(* The total amount uploaded so far, encoded in base ten ascii. *)

(* The total amount downloaded so far, encoded in base ten ascii. *)

(* The number of bytes this peer still has to download, encoded in base ten
   ascii. Note that this can't be computed from downloaded and the file length
   since it might be a resume, and there's a chance that some of the downloaded
   data failed an integrity check and had to be re-downloaded. *)

(* This is an optional key which maps to started, completed, or stopped (or
   empty, which is the same as not being present). If not present, this is one
   of the announcements done at regular intervals. An announcement using started
   is sent when a download first begins, and one using completed is sent when
   the download is complete. No completed is sent if the file was complete when
   started. Downloaders send an announcement using stopped when they cease
   downloading. *)

type event = Empty | Started | Completed | Stopped
let event_to_string = function
  | Empty -> "empty"
  | Started -> "started"
  | Completed -> "completed"
  | Stopped -> "stopped"

let make_query
    ~info
    ~peer_id ?ip ~port
    ~uploaded ~downloaded ~left
    ?(event=Empty)
    ()
  =
  [ "info_hash", [info |> Info.to_bencoding |> Bencoding.to_string |> Sha1.string |> Sha1.to_bin] ; (* FIXME: to bin? *)
    "peer_id", [peer_id] ;
    "port", [string_of_int port] ;
    "uploaded", [string_of_int uploaded] ;
    "downloaded", [string_of_int downloaded] ;
    "left", [string_of_int left] ;
    "event", [event_to_string event] ]
  @ (match ip with
      | None -> []
      | Some ip -> ["ip", [ip |> Unix.string_of_inet_addr]])

type peer =
  { ip : Unix.inet_addr ;
    port : int ;
    id : PeerId.t option }
[@@deriving show { with_path = false }]

let peer_from_bencoding b =
  match b with
  | Bencoding.Dict d ->
    (
      match List.assoc_opt "ip" d with
      | Some (String ip) ->
        (
          match List.assoc_opt "port" d with
          | Some (Int port) ->
            (
              match List.assoc_opt "id" d with
              | Some (String id) ->
                (
                  { ip = Unix.inet_addr_of_string ip ; port ;
                    id = Some (PeerId.from_string id) }
                )
              | None ->
                (
                  { ip = Unix.inet_addr_of_string ip ; port ;
                    id = None }
                )
              | Some _ -> failwith "Tracker.peer_from_bencoding: wrong type for key: 'id'"
            )
          | Some _ -> failwith "Tracker.peer_from_bencoding: wrong type for key: 'port'"
          | None -> failwith "Tracker.peer_from_bencoding: missing key: 'port'"
        )
      | Some _ -> failwith "Tracker.peer_from_bencoding: wrong type for key: 'ip'"
      | None -> failwith "Tracker.peer_from_bencoding: missing key: 'ip'"
    )
  | _ ->
    failwith "Tracker.peer_from_bencoding: not a dictionary"

type response =
  | Failure of string
  | Success of { interval : int ; peers : peer list }
[@@deriving show { with_path = false }]

let response_from_bencoding b =
  match b with
  | Bencoding.Dict d ->
    (
      match List.assoc_opt "failure reason" d with
      | Some (String reason) ->
        Failure reason
      | Some _ -> failwith "Tracker.response_from_bencoding: wrong type for key: 'failure reason'"
      | None ->
        (
          match List.assoc_opt "interval" d with
          | Some (Int interval) ->
            (
              match List.assoc_opt "peers" d with
              | Some (List peers) ->
                (
                  Success {
                    interval ;
                    peers = List.map peer_from_bencoding peers ;
                  }
                )
              | Some _ -> failwith "Tracker.response_from_bencoding: wrong type for key: 'peers'"
              | None -> failwith "Tracker.response_from_bencoding: missing key: 'peers'"
            )
          | Some _ -> failwith "Tracker.response_from_bencoding: wrong type for key: 'interval'"
          | None -> failwith "Tracker.response_from_bencoding: missing key: 'interval'"
        )
    )
  | _ ->
    failwith "Tracker.response_from_bencoding: not a dictionary"
