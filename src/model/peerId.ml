
let peer_id_size = 20

type t = string
(* 20 chars long *)
[@@deriving show { with_path = false }]

let generate () =
  String.init peer_id_size (fun _ -> Char.chr (97 + Random.int 26))

let from_string s = s
