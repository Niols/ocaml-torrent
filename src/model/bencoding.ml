type t =
  | Int of int
  | String of string
  | List of t list
  | Dict of (string * t) list
[@@deriving show { with_path = false }]

let int i = Int i
let string s = String s
let list k l = List (List.map k l)

let isany _ = true
let isint = function Int _ -> true | _ -> false
let isstring = function String _ -> true | _ -> false
let islist is_k = function List l -> List.for_all is_k l | _ -> false

let unint = function Int i -> i | _ -> failwith "unint"
let unstring = function String s -> s | _ -> failwith "unstring"
let unlist unk = function List l -> List.map unk l | _ -> failwith "unlist"

let is_sorted_dict =
  let rec is_sorted_dict k = function
    | [] -> true
    | (l, _) :: dict ->
      k < l && is_sorted_dict l dict
  in
  function
  | [] -> true
  | (k, _) :: dict ->
    is_sorted_dict k dict

let rec to_buffer b = function
  | Int n ->
    Buffer.add_char b 'i';
    Buffer.add_string b (string_of_int n);
    Buffer.add_char b 'e'
  | String s ->
    Buffer.add_string b (String.length s |> string_of_int);
    Buffer.add_char b ':';
    Buffer.add_string b s
  | List l ->
    Buffer.add_char b 'l';
    List.iter (to_buffer b) l;
    Buffer.add_char b 'e'
  | Dict d ->
    Buffer.add_char b 'd';
    let d = List.sort (fun (a, _) (b, _) -> compare a b) d in
    if not (is_sorted_dict d) then assert false;
    List.iter
      (fun (s, v) ->
         to_buffer b (String s);
         to_buffer b v)
      d;
    Buffer.add_char b 'e'

let to_string v =
  let b = Buffer.create 8 in
  to_buffer b v;
  Buffer.contents b

module IString = struct
  type t =
    { str : string ;
      mutable pos : int }

  let from_string str =
    { str ; pos = 0 }

  let peek s = s.str.[s.pos]

  let skip ?(n=1) s =
    s.pos <- s.pos + n

  let next ?(n=1) s =
    let r = String.sub s.str s.pos n in
    skip ~n s;
    r

  let until c s =
    let i = ref s.pos in
    while s.str.[!i] <> c do
      incr i
    done;
    let r = String.sub s.str s.pos (!i - s.pos) in
    s.pos <- !i;
    r

  let is_at_the_end s =
    s.pos = String.length s.str
end

let from_string s =
  let s = IString.from_string s in
  let rec from_string () =
    match IString.peek s with
    | 'i' ->
      (* Integer from here to the 'e' *)
      IString.skip s;
      let i = IString.until 'e' s in
      IString.skip s;
      if i = "-0" then failwith "Bencoding.from_string: -0 forbidden";
      if String.length i > 1 && i.[0] = '0' then failwith "Bencoding.from_string: leading 0 forbidden";
      Some (Int (int_of_string i))

    | c when 48 <= Char.code c && Char.code c <= 57 ->
      (* String. We first find the size by going to the next ':'. And then we
         get the string. *)
      let l = IString.until ':' s |> int_of_string in
      IString.skip s;
      Some (String (IString.next ~n:l s))

    | 'l' ->
      (* List. *)
      IString.skip s;
      Some (List (list_from_string ()))

    | 'd' ->
      (* Dict. *)
      IString.skip s;
      let d = dict_from_string () in
      if not (is_sorted_dict d) then failwith "Bencoding.from_string: dict must be sorted";
      Some (Dict d)

    | 'e' -> IString.skip s; None
    | c -> failwith ("Bencoding.from_string: unexpected char: " ^ String.make 1 c)

  and list_from_string () =
    (* We call ourselves in a loop until we get a 'None' (the 'e' symbol). *)
    match from_string () with
    | None -> []
    | Some v -> v :: list_from_string ()

  and dict_from_string () =
    match from_string () with
    | None -> []
    | Some (String k) ->
      (match from_string () with
       | None -> failwith "Bencoding.from_string: dict end after key"
       | Some v -> (k, v) :: dict_from_string ())
    | _ -> failwith "Bencoding.from_string: dict key must be string"
  in
  match from_string () with
  | None -> failwith "Bencoding.from_string: cannot start on 'e'"
  (* | exception exn -> raise exn *)
  | Some v ->
    if not (IString.is_at_the_end s) then
      failwith "Bencoding.from_string: read finished before the end of the string";
    v

let%test _ = to_string (String "spam") = "4:spam"
let%test _ = from_string "4:spam" = String "spam"

let%test _ = to_string (Int 3) = "i3e"
let%test _ = from_string "i3e" = Int 3
let%test _ = to_string (Int (-3)) = "i-3e"
let%test _ = from_string "i-3e" = Int (-3)
let%test _ = try ignore (from_string "i-0e"); false with _ -> true
let%test _ = try ignore (from_string "i03e"); false with _ -> true
let%test _ = to_string (Int 0) = "i0e"
let%test _ = from_string "i0e" = Int 0

let%test _ = to_string (List [String "spam"; String "eggs"]) = "l4:spam4:eggse"
let%test _ = from_string "l4:spam4:eggse" = List [String "spam"; String "eggs"]

let%test _ = to_string (Dict ["cow", String "moo"; "spam", String "eggs"]) = "d3:cow3:moo4:spam4:eggse"
let%test _ = from_string "d3:cow3:moo4:spam4:eggse" = Dict ["cow", String "moo"; "spam", String "eggs"]
let%test _ = to_string (Dict ["spam", List [String "a"; String "b"]]) = "d4:spaml1:a1:bee"
let%test _ = from_string "d4:spaml1:a1:bee" = Dict ["spam", List [String "a"; String "b"]]
let%test _ = to_string (Dict ["spam", String "eggs"; "cow", String "moo"]) = "d3:cow3:moo4:spam4:eggse"
let%test _ = try ignore (from_string "d4:spam4:eggs3:cow3:mooe"); false with _ -> true

let%test _ = from_string "l4:spami24e4:eggse" = List [String "spam"; Int 24; String "eggs"]

let from_file path =
  let ic = open_in path in
  let content =
    let bufsize = 1024 in
    let buf = Buffer.create bufsize in
    let tmp = Bytes.create bufsize in
    let rec read () =
      match input ic tmp 0 bufsize with
      | 0 -> ()
      | len ->
        Buffer.add_subbytes buf tmp 0 len;
        read ()
    in
    read ();
    Buffer.contents buf
  in
  from_string content

module Getter = struct
  type 'a converter = t -> 'a

  let rec find path b =
    match path with
    | [] -> Some b
    | key :: path ->
      match b with
      | Dict dict ->
        (
          match List.assoc_opt key dict with
          | Some b' -> find path b'
          | None -> None
        )
      | _ -> None

  let mem path b =
    find path b <> None

  (* FIXME: tout passer en Ok|Error *)

  let gen_get
      ?(on_not_found=fun () -> failwith "Bencoding.Getter.gen_get: not found")
      ?(on_conversion_failed=fun () -> failwith "Bencoding.Getter.gen_get: conversion failed")
      ~on_success
      converter path b
    =
    match find path b with
    | None -> on_not_found ()
    | Some found ->
      let converted =
        try Ok (converter found)
        with Failure _ -> Error (on_conversion_failed ())
      in
      match converted with
      | Ok converted -> on_success converted
      | Error error -> error

  type 'a get_t = NotFound | ConversionFailed | Success of 'a

  let get_t
      ?(on_not_found=fun () -> NotFound)
      ?(on_conversion_failed=fun () -> ConversionFailed)
      ?(on_success=fun v -> Success v)
      converter path b
    =
    gen_get
      ~on_not_found ~on_conversion_failed ~on_success
      converter path b

  let get
      ?on_not_found ?on_conversion_failed
      converter path b
    =
    gen_get
      ?on_not_found ?on_conversion_failed ~on_success:(fun x -> x)
      converter path b

  let int = function
    | Int i -> i
    | _ -> failwith "Bencoding.Getter.int"

  let string = function
    | String s -> s
    | _ -> failwith "Bencoding.Getter.string"

  let list conv = function
    | List l -> List.map conv l
    | _ -> failwith "Bencoding.Getter.list"
end
