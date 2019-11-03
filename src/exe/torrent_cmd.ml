open Torrent.Model

let metainfo =
  Sys.argv.(1)
  |> Bencoding.from_file
  |> Metainfo.from_bencoding

let peer_id = PeerId.generate ()

let tracker_query =
  Tracker.make_query
    ~info:(Metainfo.info metainfo)
    ~peer_id
    ~port:6881
    ~uploaded:0
    ~downloaded:0
    ~left:0 (* FIXME *)
    ()

let uri =
  metainfo
  |> Metainfo.announce
  |> Uri.of_string
  |> (fun uri -> Uri.with_query uri tracker_query)

open Lwt
open Cohttp_lwt_unix

let tracker_response =
  Client.get uri >>= fun (_resp, body) ->
  (* FIXME: check status code *)
  body |> Cohttp_lwt.Body.to_string >|= fun body ->
  body |> Bencoding.from_string |> Tracker.response_from_bencoding

let () =
  let tracker_response = Lwt_main.run tracker_response in
  Format.eprintf "%s@." (Tracker.show_response tracker_response);
